# publish-diagnoses — Diagnostic Publication Pipeline

This document describes how scheme-langserver generates, accumulates, and
publishes diagnostics to the LSP client via `textDocument/publishDiagnostics`.

---

## 1. Overview

scheme-langserver uses a **push model** for diagnostics: the server periodically
publishes diagnostic notifications to the client.  The internal trigger is a
request method named `private:publish-diagnoses`, which is produced by an
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
      (make-request '() "private:publish-diagnoses" '())
      request-processor
      (server-workspace server-instance)))
  ...)
```

The timer callback simply pushes a `private:publish-diagnoses` request into the
queue.  The actual publication happens later when the worker thread pops and
executes it.

### 2.2 Deduplication in the queue

`request-queue-push` treats `private:publish-diagnoses` specially:

```scheme
["private:publish-diagnoses"
  (let* ([predicator ...]
      [tickal-task (find predicator (request-queue-tickal-task-list queue))])
    (when (not tickal-task)
      (make-tickal-task request queue workspace)))]
```

If a `private:publish-diagnoses` task already exists in `tickal-task-list`
(either pending in the queue or currently running), the new request is **dropped**.
This guarantees at most one publish task is alive at any moment.

### 2.3 Cancellation by `textDocument/didChange`

When a `didChange` arrives, `request-queue-push` walks `tickal-task-list` and
sets `stop? = #t` on every `private:publish-diagnoses` task it finds (among
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

When the worker thread eventually executes `private:publish-diagnoses`, it
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
            (lambda (d) (not (null? (document-diagnoses d))))
            (map file-node-document
              (map
                (lambda (s)
                  (walk-file (workspace-file-node workspace) s))
                (workspace-undiagnosed-paths workspace)))))])
    (workspace-undiagnosed-paths-set! workspace '())
    result))
```

Data transformation steps:

1. **Path → file-node**: `walk-file` locates the `file-node` for each path.
2. **file-node → document**: `file-node-document` extracts the `document`.
3. **Filter empty**: documents with no diagnoses are dropped.
4. **Raw diagnose → LSP diagnostic**: `private:document->diagnostic-vec`
   converts each 4-tuple into a JSON-serialisable alist with `range`,
   `severity`, and `message`.
5. **Clear accumulator**: `undiagnosed-paths` is reset to `'()`.

The resulting list of alists is then iterated by `private:publish-diagnostics`,
which sends one `textDocument/publishDiagnostics` notification per document:

```scheme
(map
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
│  - cancels old publish-diagnoses tasks (stop? = #t)                 │
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
│  Interval timer (1 s) → push private:publish-diagnoses              │
│  (dedup: skipped if one already exists)                             │
└─────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Worker thread pops publish-diagnoses                               │
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
`undiagnosed-paths`.  On the next timer tick `publish-diagnoses` runs, but
because the document now has zero diagnoses, it is dropped from the publish
list.  The client **never receives an update** for that document, so the old
diagnostic remains visible forever.

**Trigger scenario**:
1. Open a file with `(import (nonexistent-lib))`.
2. Server publishes `"Fail to find library:nonexistent-lib"`.
3. User fixes the import to a real library name.
4. Server re-analyses → `document-diagnoses` becomes `'()`.
5. `publish-diagnoses` sends nothing for this document.
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
3. Timer fires → `publish-diagnoses` tries to walk the stale path → **crash**.

**Fix**: guard each `walk-file` result and skip `'()` before calling
`file-node-document`.

---

### Bug 3: `undiagnosed-paths` is not cleared if publication fails

**Location**: `protocol/apis/document-diagnostic.sls`.

The reset `(workspace-undiagnosed-paths-set! workspace '())` happens **after**
`result` is fully computed.  If an exception inside the traversal aborts
execution, control never reaches the `set!`.  The stale paths remain in
`undiagnosed-paths`.

**Consequences**:
- On the next timer tick, the same paths are processed again.
- If the failure was caused by a deleted file, the server may crash-loop.

**Fix direction**: either guard the computation with `dynamic-wind`, or ensure
invalid paths are filtered out at the very beginning so the computation cannot
abort.

---

### Bug 4: Same crash in pull diagnostics (`textDocument/diagnostic`)

**Location**: `protocol/apis/document-diagnostic.sls:51-53`.

```scheme
[pre-file-node (walk-file ...)]
[file-node (if (null? pre-file-node)
               (walk-file ... (substring uri 7 ...))
               pre-file-node)]
[document (file-node-document file-node)]
```

If both `walk-file` calls return `'()`, `file-node-document` crashes.

**Scope**: this exact pattern (`walk-file` + `substring` fallback +
`file-node-document`) is copy-pasted into **eight** API files:
`hover`, `definition`, `completion`, `document-symbol`, `document-highlight`,
`formatting`, `references`, and `document-diagnostic`.  It is a systematic
foot-gun across the protocol layer.

---

## 6. Improvement opportunities

### 6.1 Use `for-each` instead of `map` when the result is discarded

**Location**: `scheme-langserver.sls:63` (`private:publish-diagnostics`).

```scheme
(map (lambda (params) (send-message ...)) ...)
```

The list produced by `map` is never used.  Per project style (AGENTS.md),
`for-each` is preferred for side-effect-only iteration.

---

### 6.2 Reduce nested traversals in `unpublish-diagnostics->list`

**Location**: `protocol/apis/document-diagnostic.sls:26-38`.

Four nested `map`/`filter` passes create intermediate lists that are immediately
discarded.  A single `fold-left` can walk `undiagnosed-paths` once, accumulating
valid LSP diagnostic params.

---

### 6.3 Re-queue publish immediately after `didChange`

Currently `didChange` only **cancels** the old publish task.  The client may
wait up to 1 second for updated diagnostics.  A small enhancement would be to
enqueue a fresh `private:publish-diagnoses` task right after `didChange`
finishes processing (or, more conservatively, coalesce it with the next timer
tick if one is already pending).

---

### 6.4 Naming inconsistency

The internal request method is `private:publish-diagnoses` (ends in **ses**),
while the handler function is `private:publish-diagnostics` (ends in **tics**).
Aligning them would reduce confusion.

---

### 6.5 Hard-coded `substring` offset in URI fallback

Eight protocol API files contain:

```scheme
(substring (text-document-uri text-document) 7 (string-length ...))
```

This assumes the URI prefix is exactly `file://` (7 characters).  If the client
sends `file:///` or a non-standard scheme, the fallback path is wrong.  A
utility function `uri->path-fallback` should centralise this logic.

---

## 7. Related files

| File | Role |
|------|------|
| `scheme-langserver.sls` | `private:publish-diagnostics` handler, interval timer setup |
| `protocol/analysis/request-queue.sls` | Dedup, cancellation, and enqueue logic for `private:publish-diagnoses` |
| `protocol/apis/document-diagnostic.sls` | `unpublish-diagnostics->list`, `diagnostic` (pull), and LSP formatting |
| `analysis/workspace.sls` | `undiagnosed-paths` management, `init-references`, `refresh-workspace-for` |
| `analysis/abstract-interpreter.sls` | `step` — identifier resolution and warning generation |
| `analysis/identifier/rules/library-import.sls` | Library-not-found diagnostics |
| `virtual-file-system/document.sls` | `document-diagnoses`, `append-new-diagnoses` |
| `virtual-file-system/file-node.sls` | `walk-file` |
