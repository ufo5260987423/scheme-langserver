# publish-diagnoses — Diagnostic Publication Pipeline

This document describes how scheme-langserver generates, accumulates, and
publishes diagnostics to the LSP client via `textDocument/publishDiagnostics`.

---

## 1. Overview

scheme-langserver uses a **push model** for diagnostics: the server periodically
publishes diagnostic notifications to the client.  The internal trigger is a
request method named `private:publish-diagnostics`, which is produced by an
interval timer and processed through the same single-consumer request queue as
all other LSP requests.

Key design goals:
- Do not block the worker thread with long-running analysis.
- Coalesce rapid successive changes so the client is not flooded.
- Guarantee that document-sync notifications (`didOpen`/`didChange`/`didClose`)
  are never interrupted by engine time-slicing.

---

## 2. Trigger mechanism

### 2.1 Interval timer (multi-threaded mode)

When the server starts in multi-threaded mode (`thread-pool` is non-`#f`),
`init-server` creates an `interval-timer` with a 1-second period:

```scheme
(init-interval-timer
  (make-time 'time-duration 0 1)
  (lambda ()
    (request-queue-push request-queue
      (make-request '() "private:publish-diagnostics" '())
      request-processor
      (server-workspace server-instance)))
  ...)
```

The timer callback simply pushes a `private:publish-diagnostics` request into the
queue.  The actual publication happens later when the worker thread pops and
executes it.

### 2.2 Deduplication in the queue

`request-queue-push` treats `private:publish-diagnostics` specially:

```scheme
["private:publish-diagnostics"
  (let* ([predicator ...]
      [tickal-task (find predicator (request-queue-tickal-task-list queue))])
    (when (not tickal-task)
      (make-tickal-task request queue workspace)))]
```

If a `private:publish-diagnostics` task already exists in `tickal-task-list`
(either pending in the queue or currently running), the new request is **dropped**.
This guarantees at most one publish task is alive at any moment.

### 2.3 Cancellation by `textDocument/didChange`

When a `didChange` arrives, `request-queue-push` walks `tickal-task-list` and
sets `stop? = #t` on every `private:publish-diagnostics` task it finds (among
others).  The old publish task is therefore cancelled, because the document has
changed and its diagnostics are stale.

> **Important**: `didChange` does **not** enqueue a replacement publish task.
> The client must wait until the next timer tick (up to 1 second) to receive
> updated diagnostics.

---

## 3. Diagnostic lifecycle

Diagnostics flow through four stages: **generation**, **accumulation**,
**publication**, and **cleanup**.

### 3.1 Generation — where diagnostics come from

Every diagnostic is attached to a `document` record (field `diagnoses`).
The generation pipeline is triggered by `init-references` (batch analysis) or
`refresh-workspace-for` (incremental analysis).

#### 3.1.1 `private-init-references`

For each target path:

1. **Clear old diagnostics**:
   ```scheme
   (document-diagnoses-set! document '())
   ```

2. **Run the abstract interpreter** (`step`):
   - Resolves identifier references across the file graph.
   - If resolution fails, appends a warning:
     ```scheme
     (append-new-diagnoses current-document
       `(start end 2 "Scheme-langserver Warnning: Fail to catch identifiers"))
     ```

3. **Process excluded references** (`process-library-identifier-excluded-references`):
   - Validates `import` forms.
   - If a library cannot be found, appends:
     ```scheme
     (append-new-diagnoses document
       `(start end 2 "Fail to find library:..."))
     ```
   - Similar checks exist for `load` ("Fail to find file:...").

4. **Type inference** (optional, when `type-inference?` is enabled):
   - Runs `construct-substitutions-for`.
   - Errors during type inference are caught and logged, but currently **do not**
     produce user-visible diagnostics (they are only warnings in the server log).

5. **Mark document as non-refreshable**:
   ```scheme
   (document-refreshable?-set! document #f)
   ```
   This prevents the same document from being re-analysed until the next change.

#### 3.1.2 Shape of a single diagnose

A raw diagnose is a 4-element list:

```scheme
(range-start range-end severity message)
```

- `range-start`, `range-end` — byte offsets into the document text.
- `severity` — LSP severity integer (1=Error, 2=Warning, 3=Information, 4=Hint).
- `message` — human-readable string.

---

### 3.2 Accumulation — `workspace-undiagnosed-paths`

Not every file with diagnostics is published immediately.  Instead, paths are
stored in a workspace field called `undiagnosed-paths`.

#### 3.2.1 When paths are added

| Event | Code location | What happens |
|-------|---------------|--------------|
| **Workspace init** | `init-workspace` | All paths from `get-init-reference-batches` are appended to `undiagnosed-paths`. |
| **Full refresh** | `refresh-workspace` | Same as init — all batch paths are appended. |
| **Incremental refresh** | `refresh-workspace-for` | The changed file and its dependency-closure paths are merged into `undiagnosed-paths`. |

The merge uses `ordered-dedupe` to keep the list sorted and unique:

```scheme
(workspace-undiagnosed-paths-set! workspace-instance
  (ordered-dedupe
    (merge string<?
      (workspace-undiagnosed-paths workspace-instance)
      (sort string<? path))
    string=?))
```

> **Why not publish immediately?**  Batch `init-references` may re-analyse dozens
> of files.  Collecting paths and publishing once per second amortises JSON
> serialization and I/O overhead.

---

### 3.3 Publication — `unpublish-diagnostics->list`

When the worker thread eventually executes `private:publish-diagnostics`, it
calls `private:publish-diagnostics` in `scheme-langserver.sls`, which delegates
to `unpublish-diagnostics->list`:

```scheme
(define (unpublish-diagnostics->list workspace)
  (let ([result
        (map
          (lambda (d)
            (make-alist
              'uri (document-uri d)
              'diagnostics (private:document->diagnostic-vec d)))
          (filter
            (lambda (node) (not (null? node)))
            (map
              (lambda (s)
                (let ([file-node (walk-file (workspace-file-node workspace) s)])
                  (if (null? file-node) '() (file-node-document file-node))))
              (workspace-undiagnosed-paths workspace))))])
    (workspace-undiagnosed-paths-set! workspace '())
    result))
```

Data transformation steps:

1. **Path → file-node**: `walk-file` locates the `file-node` for each path.
   If the path is stale (file deleted), `'()` is returned and skipped.
2. **file-node → document**: `file-node-document` extracts the `document`.
3. **Raw diagnose → LSP diagnostic**: `private:document->diagnostic-vec`
   converts each 4-tuple into a JSON-serialisable alist with `range`,
   `severity`, and `message`.
4. **Clear accumulator**: `undiagnosed-paths` is reset to `'()`.

> **Note**: empty diagnostics are **not** filtered out.  When a document has
> zero diagnoses, an empty `diagnostics` array is sent so the client clears
> any stale errors.  See Bug 1 below.

The resulting list of alists is then iterated by `private:publish-diagnostics`,
which sends one `textDocument/publishDiagnostics` notification per document:

```scheme
(for-each
  (lambda (params)
    (send-message server-instance
      (make-notification "textDocument/publishDiagnostics" params)
      'publish))
  (unpublish-diagnostics->list (server-workspace server-instance)))
```

---

### 3.4 Cleanup

After a successful publish:
- `workspace-undiagnosed-paths` is `'()`.
- Each published `document` still retains its `diagnoses` list (it is **not**
  cleared after publishing).

This means if a client reconnects or a pull-diagnostic request arrives later,
the same diagnostics are still available in-memory.

---

## 4. Complete data-flow diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Client edits a file                         │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│  textDocument/didChange  →  request-queue-push                      │
│  - cancels old publish-diagnostics tasks (stop? = #t)                 │
│  - enqueues didChange itself (non-interruptible)                    │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Worker thread pops didChange → update document text & index        │
│  → refresh-workspace-for(target-file-node)                          │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    ▼               ▼               ▼
         ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
         │ Add target  │  │ Add dep-    │  │ init-references
         │ path to     │  │ closure     │  │ (re-analyse)
         │ undiagnosed │  │ paths to    │  │
         │ paths       │  │ undiagnosed │  │ step / type inference
         └─────────────┘  │ paths       │  │ → document-diagnoses
                        └─────────────┘  └─────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Interval timer (1 s) → push private:publish-diagnostics              │
│  (dedup: skipped if one already exists)                             │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Worker thread pops publish-diagnostics                               │
│  → unpublish-diagnostics->list                                      │
│    - walk-file  → file-node (or '() if deleted)                    │
│    - file-node-document → document                                  │
│    - private:document->diagnostic-vec → LSP format                  │
│    - clear undiagnosed-paths                                        │
│  → send-message "textDocument/publishDiagnostics"                   │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 5. Known bugs

### Bug 1 (Fixed): Stale diagnostics are never cleared on the client

**Location**: `protocol/apis/document-diagnostic.sls`, inside
`unpublish-diagnostics->list`.

**Problem**: the function filtered out documents whose `document-diagnoses` was
empty:

```scheme
(filter 
  (lambda (d) (not (null? (document-diagnoses d))))
  ...)
```

When a user fixes an error (e.g. corrects a misspelled library import), the
server re-analyses the file, clears `document-diagnoses`, and places the path in
`undiagnosed-paths`.  On the next timer tick `publish-diagnostics` runs, but
because the document now has zero diagnoses, it is dropped from the publish
list.  The client **never receives an update** for that document, so the old
diagnostic remains visible forever.

**Trigger scenario**:
1. Open a file with `(import (nonexistent-lib))`.
2. Server publishes `"Fail to find library:nonexistent-lib"`.
3. User fixes the import to a real library name.
4. Server re-analyses → `document-diagnoses` becomes `'()`.
5. `publish-diagnostics` sends nothing for this document.
6. Client still shows the old error.

**Fix**: remove the `(not (null? (document-diagnoses d)))` filter.  An empty
`diagnostics` array in `textDocument/publishDiagnostics` is the LSP-compliant
way to tell the client to clear diagnostics for that document.

---

### Bug 2 (Fixed): `walk-file` returning `'()` causes a crash

**Location**: `protocol/apis/document-diagnostic.sls`, inside
`unpublish-diagnostics->list`.

**Problem**: `walk-file` returns `'()` when a path no longer exists in the
virtual file system (e.g. the file was deleted after the path was added to
`undiagnosed-paths`).  The old code chained:

```scheme
(map file-node-document
  (map (lambda (s) (walk-file ... s)) ...))
```

`file-node-document` was called on `'()`, which is not a `file-node` record,
raising a type error and crashing the server.

**Trigger scenario**:
1. File `a.scm` is opened → path added to `undiagnosed-paths`.
2. File `a.scm` is deleted externally.
3. Timer fires → `publish-diagnostics` tries to walk the stale path → **crash**.

**Fix**: guard each `walk-file` result and skip `'()` before calling
`file-node-document`.

---

### Bug 3 (Fixed): `undiagnosed-paths` is not cleared if publication fails

**Location**: `protocol/apis/document-diagnostic.sls`.

The reset `(workspace-undiagnosed-paths-set! workspace '())` happens **after**
`result` is fully computed.  If an exception inside the traversal aborts
execution, control never reaches the `set!`.  The stale paths remain in
`undiagnosed-paths`.

**Consequences**:
- On the next timer tick, the same paths are processed again.
- If the failure was caused by a deleted file, the server may crash-loop.

**Fix**: snapshot `undiagnosed-paths` into a local variable and clear the
workspace field *before* starting the traversal.  Even if an exception aborts
the fold, the paths have already been removed from the accumulator.

---

### Bug 4 (Fixed): Same crash in pull diagnostics (`textDocument/diagnostic`)

**Location**: `protocol/apis/document-diagnostic.sls` and seven other API files.

**Problem**: the exact pattern (`walk-file` + `substring` fallback +
`file-node-document`) was copy-pasted into **eight** API files:
`hover`, `definition`, `completion`, `document-symbol`, `document-highlight`,
`formatting`, `references`, and `document-diagnostic`.  If both `walk-file`
calls returned `'()`, `file-node-document` crashed.

**Fix**: extracted the pattern into a shared helper `resolve-uri->file-node`
in `virtual-file-system/file-node.sls`.  It guards against `'()` before
returning, and all eight API files now use it.

---

## 6. Improvement opportunities

### 6.1 ✅ Done — Use `for-each` instead of `map` when the result is discarded

**Location**: `scheme-langserver.sls:63` (`private:publish-diagnostics`).

Changed from `map` to `for-each` since the list is discarded and only the
side-effect (`send-message`) matters.

---

### 6.2 ✅ Done — Reduce nested traversals in `unpublish-diagnostics->list`

**Location**: `protocol/apis/document-diagnostic.sls:26-38`.

Replaced the four nested `map`/`filter` passes with a single `fold-right`
that walks `undiagnosed-paths` once, accumulating valid LSP diagnostic params.

---

### 6.3 ❌ Won't do — Re-queue publish immediately after `didChange`

**Status**: rejected.

The 1-second interval timer provides **debounce** for rapid successive edits.
If `didChange` immediately enqueued a publish task, fast typing would trigger
repeated `refresh-workspace-for` / `init-references` calls, wasting CPU and
slowing down the worker thread.

Moreover, the dominant latency is not the timer wait but the **index update**
itself (`refresh-workspace-for` → abstract interpreter → type inference).
Even if publish were triggered instantly, the client would still wait for the
analysis to finish.  The timer therefore offers a cheap, natural coalescing
point without adding extra complexity.

---

### 6.4 ✅ Done — Naming inconsistency

Aligned the internal request method and handler function to both use
`private:publish-diagnostics` (ends in **tics**), matching the LSP standard
`textDocument/publishDiagnostics`.

---

### 6.5 ✅ Done — Centralise URI-to-file-node resolution

Eight protocol API files previously contained a hard-coded fallback:

```scheme
(substring (text-document-uri text-document) 7 (string-length ...))
```

This assumed the URI prefix is exactly `file://` (7 characters).  The logic has
been extracted into `resolve-uri->file-node` in
`virtual-file-system/file-node.sls`, which tries `uri->path` first and falls
back to stripping the prefix only when the URI actually starts with `file://`.
All eight API files now use the shared helper.

---

## 7. Related files

| File | Role |
|------|------|
| `scheme-langserver.sls` | `private:publish-diagnostics` handler, interval timer setup |
| `protocol/analysis/request-queue.sls` | Dedup, cancellation, and enqueue logic for `private:publish-diagnostics` |
| `protocol/apis/document-diagnostic.sls` | `unpublish-diagnostics->list`, `diagnostic` (pull), and LSP formatting |
| `analysis/workspace.sls` | `undiagnosed-paths` management, `init-references`, `refresh-workspace-for` |
| `analysis/abstract-interpreter.sls` | `step` — identifier resolution and warning generation |
| `analysis/identifier/rules/library-import.sls` | Library-not-found diagnostics |
| `virtual-file-system/document.sls` | `document-diagnoses`, `append-new-diagnoses` |
| `virtual-file-system/file-node.sls` | `walk-file`, `resolve-uri->file-node` |
