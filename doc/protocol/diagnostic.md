# Diagnostic Improvement Plan

> Status: **P0 done**; unused import done; precise undefined identifier and type-mismatch remain  
> Created: 2025-05-13  
> Updated: 2026-06-24

---

## 1. Current State

The diagnose subsystem stores raw diagnostics as lists with the following
shape (backward-compatible forms are still accepted):

```scheme
(start-bias end-bias severity message [source] [code])
```

- `start-bias/end-bias` — character offsets into the document text.
- `severity` — LSP severity integer (`1`=Error, `2`=Warning, `3`=Information, `4`=Hint).
- `message` — human-readable string.
- `source` — optional subsystem tag (e.g. `"syntax"`, `"identifier"`, `"import"`, `"load"`, `"type"`).
  When omitted, `private:make-diagnostic` falls back to `"scheme-langserver"`.
- `code` — optional diagnostic code (e.g. `"syntax-error"`, `"library-not-found"`).

LSP conversion (`protocol/apis/document-diagnostic.sls`) emits a `Diagnostic`
object with `range`, `severity`, `message`, `source`, and (when present) `code`.
The following LSP fields are still **not emitted**:

| LSP field | Current support | Impact when missing |
|-----------|----------------|---------------------|
| `source` | ✅ | Diagnostics are tagged by subsystem. |
| `code` | ✅ | Most common diagnostics have a stable code. |
| `tags` | ❌ | Cannot mark `Unnecessary` or `Deprecated`. |
| `relatedInformation` | ❌ | Cannot show "defined here" / "imported here" cross-references. |

### 1.1 Sources of diagnostics today

| Source | File | Severity | `source` | `code` | Message example |
|--------|------|----------|----------|--------|-----------------|
| Tokenizer / Parser | `analysis/tokenizer.sls` | `1` (Error) | `"syntax"` | `"syntax-error"` | `"Syntax error: ..."` |
| File not found | `analysis/tokenizer.sls` | `1` (Error) | `"syntax"` | `"file-not-found"` | `"File not found: ..."` |
| Duplicate identifier | `analysis/identifier/reference.sls` | `1` (Error) | `"identifier"` | `"duplicate-identifier"` | `"Duplicate identifier: x"` |
| Abstract interpreter | `analysis/abstract-interpreter.sls` | `2` (Warning) | `"identifier"` | `"identifier-resolution-failure"` | `"Scheme-langserver Warning: Fail to catch identifiers"` |
| Library import (r6rs) | `analysis/identifier/rules/library-import.sls` | `2` (Warning) | `"import"` | `"library-not-found"` | `"Fail to find library:..."` |
| Library import (r7rs) | `analysis/identifier/rules/r7rs/define-library-import.sls` | `2` (Warning) | `"import"` | `"library-not-found"` | `"Fail to find library:..."` |
| File load | `analysis/identifier/rules/load.sls` | `2` (Warning) | `"load"` | `"load-file-not-found"` | `"Fail to find file:..."` |
| Unused import | `analysis/workspace.sls` | `2` (Warning) | `"import"` | `"unused-import"` | `"Unused import: car"` |
| Type inference | `analysis/workspace.sls` | `2` (Warning) | `"type"` | `"type-inference-warning"` | `"Type inference warning: ..."` |
| Type rules | `analysis/type/substitutions/generator.sls` | `2` (Warning) | `"type"` | `"type-rule-warning"` | `"Type rule warning: ..."` |
| Analysis error | `analysis/workspace.sls` | `1` (Error) | `"analysis"` | `"analysis-error"` | `"Analysis error: ..."` |

### 1.2 Known gaps

1. **No precise undefined-variable diagnostic.**  When the abstract interpreter
   cannot resolve an identifier it emits a single file-level warning that does
   not name the identifier or its location.
2. **No type-mismatch diagnostics.**  The type system (`type:->?`, `type:=?`)
   exists but is only used for hover tooltips.  It is not wired into
   `publishDiagnostics`.
3. **No unused-variable diagnostic.**  `unused-import` is implemented via
   `identifier-reference-usage-count`, but local variables that are declared
   and never referenced are not yet flagged.

---

## 2. Roadmap

### P0 — Enrich diagnostic metadata (`source` + `code`) ✅ Done

**Goal:** Make diagnostics professional and filterable.

**Implementation status:**
- The internal diagnose format accepts the optional 5th `source` and 6th `code`
  fields:
  ```scheme
  ;; Backward-compatible forms
  (start-bias end-bias severity message)               ; source="scheme-langserver", code=#f
  (start-bias end-bias severity message source)        ; code=#f
  (start-bias end-bias severity message source code)   ; full
  ```
- `private:make-diagnostic` in `protocol/apis/document-diagnostic.sls` emits
  `source` and, when present, `code`.
- All `append-new-diagnoses` call sites now supply a `source`, and most common
  diagnostics also have a `code`.

**`source` values in use:**

| Subsystem | `source` string |
|-----------|-----------------|
| tokenizer / parser | `"syntax"` |
| abstract interpreter (identifier resolution) | `"identifier"` |
| library / import resolution | `"import"` |
| file load | `"load"` |
| type inference / type rules | `"type"` |
| analysis error (threaded exception fallback) | `"analysis"` |

**`code` values in use:**

| Diagnostic | `code` |
|------------|--------|
| Syntax error (tokenizer) | `"syntax-error"` |
| File not found (tokenizer) | `"file-not-found"` |
| Duplicate identifier | `"duplicate-identifier"` |
| Library not found | `"library-not-found"` |
| File not found (load) | `"load-file-not-found"` |
| Identifier resolution failure | `"identifier-resolution-failure"` |
| Unused import | `"unused-import"` |
| Type inference warning | `"type-inference-warning"` |
| Type rule warning | `"type-rule-warning"` |
| Analysis error | `"analysis-error"` |

---

### P1 — Precise undefined-identifier diagnostic

**Goal:** Replace the blanket `"Fail to catch identifiers"` with
`"Undefined identifier: foo"` at the exact symbol location.

**Implementation sketch:**
- In `abstract-interpreter.sls`, when `head-expression` is a symbol and
  `private:find-available-references-for` returns `'()`, emit a diagnostic
  with:
  - severity `1` (Error)
  - range = the symbol's index-node start/end
  - message = `(string-append "Undefined identifier: " (symbol->string head-expression))`
  - source = `"identifier"`
  - code = `"undefined-identifier"`

**Caveats:**
- Must not fire inside `quote` / `quasiquote` (already guarded by existing
  `quote?` / `quasiquote?` branches).
- Must not fire for syntax-parameters or macro auxiliary identifiers.
- Free variables valid in the REPL but not in the workspace may produce false
  positives; consider making this a Warning instead of Error, or adding a
  suppression list.

**Estimated effort:** 2–3 days.

---

### P2 — Type-mismatch diagnostics

**Goal:** Use the existing type system to emit warnings when argument types do
not match a function's signature.

**Implementation sketch:**
- In `analysis/type/substitutions/rules/application.sls`, after inferring the
  callee's type and the arguments' types, call `type:->?` for each parameter.
- If `type:->? actual expected` is `#f` and neither type is `something?`
  (unknown), emit:
  ```scheme
  (start end 2                            ; Warning
    (string-append "Type mismatch: expected " (type->string expected)
                   ", got " (type->string actual))
    "type" "type-mismatch")
  ```

**Caveats:**
- Polymorphic types (`(list? a ...)`) must be instantiated before comparison.
- When the type inferrer returns multiple possible types (Cartesian product),
  a mismatch in one branch should not necessarily produce a diagnostic unless
  *all* branches mismatch.
- Scheme is dynamically typed; severity should remain `Warning` (not `Error`).

**Estimated effort:** 1–2 weeks.

---

### P3 — Unused variable / unused import diagnostics 🔄 Partially done

**Goal:** Mark variables and imports that are declared but never referenced.

**Implementation status:**
- ✅ **Unused import** is implemented.  `abstract-interpreter.sls` increments
  `identifier-reference-usage-count` when a leaf symbol is resolved.  After
  `step`, `analysis/workspace.sls:private:check-unused-imports` walks each
  `import` clause and emits `"Unused import: ..."` for imported identifiers
  whose `usage-count` is still 0.  It supports plain, `only`, `except`, `rename`,
  and `alias` imports.
- ❌ **Unused variable** is not yet implemented.  Local bindings and top-level
  variables with zero references are not flagged.

**Remaining implementation sketch (unused variable):**
1. After analysis, walk `document-ordered-reference-list` and emit diagnostics
   for items with zero references:
   - `variable` type → `"Unused variable: foo"`
2. Skip exported top-level bindings and built-in bindings.
3. Consider adding `Unnecessary` tag once `tags` support is introduced.

**Caveats:**
- Top-level bindings exported from a library are "used" by the export, not by
  local references.
- Mutually recursive definitions may need special handling.

**Estimated effort:** 1–2 weeks (down from 2–3 weeks because the reference
infrastructure and `usage-count` already exist).

---

### P4 — `relatedInformation`

**Goal:** Cross-reference diagnostics with their definition / import sites.

**Example:** When a library is not found, include a `relatedInformation` entry
pointing to the `import` clause that requested it.

**Implementation sketch:**
- Extend `private:make-diagnostic` to accept an optional list of
  `DiagnosticRelatedInformation` objects.
- Each object is an alist with `location` (uri + range) and `message`.

**Estimated effort:** 1 week.

---

## 3. Priority Summary

| Priority | Item | Status | Effort | User value |
|----------|------|--------|--------|------------|
| **P0** | `source` + `code` metadata | ✅ Done | — | High (professionalism) |
| **P1** | Precise undefined identifier | ❌ Not started | 2–3 days | **Very high** (most-requested feature) |
| **P2** | Type-mismatch warnings | ❌ Not started | 1–2 weeks | High (core differentiator) |
| **P3** | Unused import | ✅ Done | — | Medium (code quality) |
| **P3** | Unused variable | ❌ Not started | 1–2 weeks | Medium (code quality) |
| **P4** | `relatedInformation` | ❌ Not started | 1 week | Medium (UX polish) |

---

## 4. Quick wins (no architecture changes)

If a full phase is too large, these can be done in minutes:

1. **Prefix diagnostic messages with category** (no `source` field change):
   ```scheme
   ;; Before
   "Fail to find library:rnrs-base"
   ;; After
   "[import] Fail to find library: rnrs-base"
   ```

2. **Fix severity inconsistencies:**
   - `"Fail to catch identifiers"` is currently Warning (`2`).  If it becomes
     an undefined-identifier diagnostic, it should be Error (`1`).

3. **Add tests for `load.sls`:**  The file has a `;;todo more test` comment but
   zero test coverage.
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

For each target path (after `init-references` has already cleared stale diagnostics and import/export references serially):

1. **Run the abstract interpreter** (`step`):
   - Resolves identifier references across the file graph.
   - Increments `usage-count` on every `identifier-reference` that is successfully resolved as a leaf symbol (used for unused-import detection below).
   - If resolution fails, appends a warning:
     ```scheme
     (append-new-diagnoses current-document
       `(start end 2 "Scheme-langserver Warnning: Fail to catch identifiers"))
     ```

2. **Process excluded references** (`process-library-identifier-excluded-references`):
   - Validates `import` forms.
   - If a library cannot be found, appends:
     ```scheme
     (append-new-diagnoses document
       `(start end 2 "Fail to find library:..."))
     ```
   - Similar checks exist for `load` ("Fail to find file:...").

3. **Duplicate identifier detection** (binding-rule post-processors):
   - Each binding rule (`lambda`, `let`, `do`, `define`, `case-lambda`, `let*`, `letrec`, `let-values`, etc.) calls `check-duplicate-identifiers` (in `analysis/identifier/util.sls`) after extracting parameter pairs.
   - If a duplicate is found, appends:
     ```scheme
     `(start end 1 "Duplicate identifier: x" "identifier" "duplicate-identifier")
     ```
   - Severity **1** (Error).

4. **Unused import detection** (`private:check-unused-imports`):
   - Runs after `step` and `process-library-identifier-excluded-references`.
   - Walks every `import` clause in the document.
   - For each imported `identifier-reference`, checks whether `usage-count` is 0.
   - Supports plain imports, `only`, `except`, `rename`, and `alias`.
   - If unused, appends:
     ```scheme
     `(start end 2 "Unused import: car" "identifier" "unused-import")
     ```
   - Severity **2** (Warning).
   - Built-in bindings (`library-identifier` is `'()` or built-in libraries such as `(rnrs)`) are skipped.

5. **Type inference** (optional, when `type-inference?` is enabled):
   - Runs `construct-substitutions-for`.
   - Errors during type inference are caught and logged, but currently **do not**
     produce user-visible diagnostics (they are only warnings in the server log).

6. **Mark document as non-refreshable**:
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
