## How scheme-langserver schedules and analyzes API requests

In **multi-threaded mode**, scheme-langserver uses a single-consumer request queue to process LSP requests serially, and leverages Chez Scheme's **engine** mechanism to assign preemptible time slices (ticks) to each task. This balances long-running analysis (such as type inference) with document consistency.

> **Note**: The "peephole optimization merging didChange" strategy mentioned in the old document no longer exists in the current implementation. The current queue uses a model of **strict serial execution + task deduplication/cancellation**.

---

### 1. Threading model overview

The server initializes the following components only when both `enable-multi-thread?` and the internal `threaded?` flag are true:

- **Thread pool**: size 2 (provided by `ufo-thread-pool`).
- **Request queue**: `make-request-queue` creates a queue with a mutex and a condition variable.
- **Interval timer**: an `interval-timer` that pushes an internal notification `private:publish-diagnoses` into the queue once per second.

**Key design**: despite the name "multi-threaded", there is effectively **only one worker thread** dedicated to consuming the request queue (the other thread typically serves the timer). The main thread (I/O thread) reads LSP JSON-RPC messages from standard input and calls `request-queue-push` to enqueue them. Therefore, all requests are processed **serially rather than in parallel**, which naturally avoids most data races.

---

### 2. Request queue structure

A `request-queue` consists of the following parts:

| Field | Description |
|------|----------|
| `mutex` | Mutex protecting the queue and `tickal-task-list` |
| `condition` | Condition variable on which the worker thread waits when the queue is empty |
| `queue` | Underlying `slib queue`, holding pending `tickal-task`s |
| `tickal-task-list` | List of all **enqueued or running** tasks, used for lookup, deduplication, and cancellation |

> **Why a linked list instead of a hash table?** `request-id` is not guaranteed to be unique. A client may send multiple requests with the same `id` (e.g., retries after a timeout), or the server may internally reuse IDs. A hash table keyed by `id` would either overwrite earlier tasks or require complex multi-value handling. A simple list with `find`/`remq` correctly handles duplicates at the small scale typical of an LSP request queue (usually < 10 items).

---

### 3. Tickal task and engine mechanism

Every enqueued LSP request is wrapped as a `tickal-task` and executed via Chez Scheme's `make-engine`. An engine is a continuation-based cooperative multitasking primitive:

- **Time slice**: each task is initially allocated `ticks = 100000` computation credits.
- **Completion callback** (`complete`): called when the task finishes normally within its time slice; removes the task from `tickal-task-list`.
- **Expiration callback** (`expire`): called when the task exhausts its time slice; decides whether to **continue running** or **terminate**.

```scheme
((make-engine job) ticks complete expire)
```

**Design purpose**: `expire` is mainly aimed at long-running background analysis (for example, type inference). When type inference exhausts its time slice, the engine triggers `expire`, at which point it can check whether the task has been cancelled by the client (`stop?`), thereby avoiding blocking the queue with useless computation.

---

### 4. Enqueue strategy and priority control

`request-queue-push` adopts different enqueue logic depending on the request method:

#### 4.1 `private:publish-diagnoses` (internal diagnostic publication)
- **Deduplication**: if a task of the same kind already exists in `tickal-task-list`, it is not enqueued again.
- This task is triggered once per second by the interval timer and is used to send accumulated diagnostic information to the client.

#### 4.2 `$/cancelRequest` (cancellation request)
- Locates the target request in `tickal-task-list` according to `params.id`.
- Sets the target task's `stop?` flag to `#t`.
- Calls `potential-request-processor` to process the cancellation request itself (usually sending a cancellation acknowledgement to the client).
- The cancelled task will be safely removed on the next `expire`, after acquiring `workspace-mutex`.

#### 4.3 `textDocument/didChange` (document change)
- If there is a pending `private:publish-diagnoses` task in the queue, it is immediately marked as `stop?` (the document has changed, so the old diagnostics are no longer valid).
- Then `didChange` itself is enqueued as a new `tickal-task`.

#### 4.4 All other requests
- A new `tickal-task` is created and enqueued directly.

---

### 5. Special protection for document synchronization requests

LSP document synchronization requests (`textDocument/didOpen`, `textDocument/didChange`, `textDocument/didClose`) enjoy **non-interruptible privilege** in the `expire` callback:

```scheme
[(or (equal? "textDocument/didChange" ...)
     (equal? "textDocument/didOpen" ...)
     (equal? "textDocument/didClose" ...))
  (remains ticks complete expire)]
```

- When these requests exhaust their time slice, they do not stop; instead they are immediately **recharged** via `(remains ticks ...)` and continue executing.
- This guarantees that the server-side document state always stays strictly consistent with the client, and will not suffer from half-finished index updates due to time-slice expiration.

> The comment in the source specifically states: `expire` should not interrupt the workspace refreshing procedure; its primary goal is to interrupt **type inference** and similar reentrant pure-computation tasks.

---

### 6. Consumer side: worker thread loop

`request-queue-pop` pops a `tickal-task` from the head of the queue under mutex protection. If the queue is empty, it blocks via `condition-wait` until `push` signals `condition-signal`.

The popped task is not executed immediately; instead a **thunk** (parameterless function) is returned. The worker thread calls this thunk in a loop, and the thunk starts the engine internally:

```scheme
(lambda ()
  ((make-engine job) ticks (tickal-task-complete task) (tickal-task-expire task)))
```

This indirection decouples the synchronization primitives (mutex/condition) of `request-queue-pop` from the actual execution environment (engine/thread) of the task.

---

### 7. Interplay between mutex, engine time slice, and LSP cancelable tasks

This section explains how the queue-level mutex, the engine-based time slicing, and the LSP cancellation protocol interact to form a safe, cooperative cancellation model.

#### 7.1 LSP cancelable task semantics

In LSP, every request message carries an `id`. A client may later send a `$/cancelRequest` notification containing that `id` to advise the server that the result is no longer needed. The specification treats cancellation as **advisory**: the server *may* stop processing, but it is not required to. If the server does stop, it can either omit the original response or return an error with code `RequestCancelled` (-32800).

scheme-langserver implements this by associating each request with a mutable `stop?` flag inside its `tickal-task`.

#### 7.2 Two-layer locking: `request-queue-mutex` vs `workspace-mutex`

The design deliberately separates the lock that protects queue metadata from the lock that protects workspace state:

| Lock | Protected resource | Held during |
|------|-------------------|-------------|
| `request-queue-mutex` | `queue` (slib queue) and `tickal-task-list` | `push`, `pop` (dequeue only), `remove:from-request-tickal-task-list` |
| `workspace-mutex` | Mutable workspace fields (file-node, library-node, linkage, undiagnosed-paths) | `init-references` (batch processing), `expire` callback when `stop?` is true |

Crucially, once the worker thread dequeues a task, `request-queue-pop` returns a thunk and **releases the queue mutex before the thunk is invoked**. Therefore, the actual execution of `request-processor` (and the engine that wraps it) runs **outside** the queue mutex. This prevents a slow request from starving the I/O thread or the timer thread.

#### 7.3 How cancellation propagates through the queue

Consider a cancelable request (e.g., `textDocument/hover` with `id = 5`) followed by `$/cancelRequest` with the same `id`:

**Step 1 – Enqueue.** The main thread calls `request-queue-push` under `request-queue-mutex`, creates a `tickal-task` (with `stop? = #f`), appends it to `tickal-task-list`, enqueues it, and signals the condition variable.

**Step 2 – Cancel.** The main thread later reads `$/cancelRequest` and calls `request-queue-push` again:
- It acquires `request-queue-mutex`.
- It scans `tickal-task-list` for the task whose `request-id` equals 5.
- It atomically sets that task's `stop?` to `#t`.
- It releases `request-queue-mutex`.

At this point the cancellation flag is visible to the worker thread, but the task may be in one of two states.

##### Case A: the task is still in the queue (not yet dequeued)

When the worker thread eventually pops the task, the returned thunk starts the engine. The engine first executes the `job` lambda:

```scheme
(lambda ()
  (if (tickal-task-stop? task)
      (remove:from-request-tickal-task-list queue task)
      (request-processor request)))
```

Because `stop?` is now `#t`, the worker skips `request-processor` entirely, removes the task from the tracking list, and silently discards the request. **No JSON-RPC response is sent for the original request**, which is explicitly permitted by the LSP specification for cancelled requests.

##### Case B: the task has already been dequeued and is running inside the engine

If the worker thread has already dequeued the task and passed it to `make-engine`, the `job` lambda has already passed the initial `if` check (at which point `stop?` was still `#f`) and `request-processor` is executing. Here the engine's time slice becomes essential:

- **Short request** (finishes before ticks exhaust): `request-processor` runs to completion. The cancellation flag is effectively ignored. The engine's `complete` callback fires, removes the task from `tickal-task-list`, and the worker may or may not send a response. This is acceptable because LSP cancellation is advisory.

- **Long request** (exhausts ticks): execution suspends and the `expire` callback is invoked with a `remains` continuation. Inside `expire` the code checks `stop?`:
  - If `stop?` is `#t`, it acquires `workspace-mutex`, removes the task from `tickal-task-list`, and **does not call `remains`**. The engine is abandoned; the long-running computation is terminated.
  - If `stop?` is still `#f`, it calls `(remains ticks complete expire)` to grant another time slice and continue.

The `workspace-mutex` acquisition in `expire` is deliberate: it ensures that the task is torn down only when no other thread is modifying the workspace, preventing orphaned or inconsistent index state.

#### 7.4 Why engine time slicing is necessary for cancellation

Without the engine mechanism, a single long-running request (such as type inference over a large file) would monopolize the sole worker thread. The `stop?` flag could be set by the main thread, but the worker would have no opportunity to inspect it until `request-processor` returns—potentially seconds later. The engine transforms this into a **cooperative preemption model**: the task runs for a bounded number of ticks, then voluntarily yields into the `expire` callback, where it can poll the cancellation flag and decide whether to live or die. This makes cancellation of long-running tasks practical while keeping the implementation simple and free of unsafe OS-level thread interruption.

#### 7.5 Document sync requests are immune to engine termination

`textDocument/didOpen`, `textDocument/didChange`, and `textDocument/didClose` are notifications, not cancelable requests, yet they receive special treatment in `expire`: they always call `remains` and are never terminated, even if some external code were to set their `stop?` flag. The reason is data integrity. These notifications mutate the virtual file system's document text and index. If such a mutation were abandoned mid-way, every subsequent analysis (hover, completion, diagnostics, linkage) would operate on a corrupt or partially-updated document. By forcing unconditional continuation, the server guarantees that its internal document state always converges to the client's state.

---

### 8. Single-threaded fallback

If the server does not run in multi-threaded mode (`thread-pool` is `#f`), then no `request-queue` is created. The main thread calls `request-processor` (i.e. `private:try-catch`) directly and synchronously after reading each LSP message. In this mode there is no engine time slicing, task cancellation, or queue deduplication.

---

### 9. Summary

| Feature | Current implementation |
|------|----------|
| Concurrency model | Single-consumer queue (logically serial) |
| Task execution | Chez `make-engine`, cooperative time slicing based on ticks |
| Deduplication | `private:publish-diagnoses` keeps at most one instance in the queue |
| Cancellation | `$/cancelRequest` sets `stop?`; safe removal happens at the next `expire` (long tasks) or at job start (pending tasks) |
| Document sync protection | `didOpen`/`didChange`/`didClose` are infinitely recharged in `expire`, ensuring they are never interrupted |
| Timeout purpose | Mainly interrupts long-running computation such as type inference, not ordinary requests |
| Lock layering | `request-queue-mutex` guards queue metadata; `workspace-mutex` guards workspace mutation and teardown |
