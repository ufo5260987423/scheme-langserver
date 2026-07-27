# Workspace

The `analysis/workspace.sls` module is the central coordinator of the language server. It owns the mutable state that represents an opened project (the **workspace**), drives the multi-phase analysis pipeline, and handles incremental updates when files change.

---

## 1. Overview

A workspace aggregates four large sub-structures:

| Sub-structure | Module | Purpose |
|---------------|--------|---------|
| Virtual file system (VFS) | `virtual-file-system/file-node` | Tree of directories / files discovered on disk. |
| Document store | `virtual-file-system/document` | Per-file AST (`index-node`), raw text, line-length cache, and diagnostic list. |
| Library index | `virtual-file-system/library-node` | Hierarchical index of `(library ...)` identifiers extracted from files. |
| Dependency graph | `analysis/dependency/file-linkage` | Directed graph encoding *import* / *load* relationships between files. |

In addition the workspace record stores configuration flags (`threaded?`, `type-inference?`, `top-environment`), a mutex for parallel analysis, and a list of *undiagnosed paths* used by the LSP diagnostic provider.

All operations on a workspace fall into one of three categories:

1. **Initialization** – build every sub-structure from scratch.
2. **Incremental refresh** – patch a changed file and re-analyse the smallest affected transitive closure.
3. **File-system mutations** – add or remove files from the VFS.

---

## 2. Workspace Record

```scheme
(define-record-type workspace
  (fields
    (mutable file-node)          ; root of the VFS tree
    (mutable library-node)       ; root of the library tree
    (mutable file-linkage)       ; dependency graph instance
    (immutable mutex)            ; Chez Scheme mutex or '()
    (immutable facet)            ; file-filter predicate
    (immutable threaded?)        ; enable parallel analysis?
    (immutable type-inference?)  ; enable type inference?
    (immutable top-environment)  ; 'r6rs | 'r7rs | 's7 | 'goldfish
    (mutable undiagnosed-paths)))
```

The protocol creates the mutex automatically when `threaded?` is true. The three mutable fields (`file-node`, `library-node`, `file-linkage`) are replaced wholesale during a full refresh, while `undiagnosed-paths` is appended to incrementally.

---

## 3. Initialization Pipeline

### 3.1 `init-workspace`

`init-workspace` is the main entry point used by the LSP server when a folder is opened. It accepts several optional parameters and resolves defaults left-to-right:

```
path → file-filter → top-environment → threaded? → type-inference? → cache-path
```

`file-filter` controls which files are included in the virtual file system. It can be:
- `'akku` — use Akku's `.akku/list` (default).
- `'txt` — include only `.scm.txt` files.
- a list of extension strings — e.g. `'(".sls" ".scm" ".scm.txt")`.
- a predicate procedure — custom filter accepting a path and returning a boolean.

The CLI maps these as follows:
- `-p akku` / `-p txt` selects the corresponding symbol preset.
- `-f <ext>` builds an extension list; repeated `-f` options are merged, and
  leading dots may be omitted.
- `-p` and `-f` are mutually exclusive.

Typical call from the server:
```scheme
(init-workspace "/project" 'akku 'r6rs #t #f)
```

The pipeline executed inside is strictly ordered:

```
init-virtual-file-system
        ↓
init-library-node
        ↓
init-file-linkage
        ↓
get-init-reference-batches   (from file-linkage)
        ↓
init-references
```

*Why this order matters:*
- The VFS must exist before we can read files to extract library names.
- The library tree must exist before `init-file-linkage` can resolve imports.
- The linkage graph must exist before we can compute topological batches.
- References must be analysed in dependency order so that imported identifiers are already bound when a dependent file is processed.

### 3.2 `init-virtual-file-system`

Recursively walks the directory tree starting at `path`. For every path accepted by the `facet` filter it creates a `file-node`. If the path is a regular file it also creates a `document` via `init-document`.

The resulting tree is later navigated with `walk-file` (from `virtual-file-system/file-node`).

### 3.3 `init-document`

Performs the first parse of a source file:

1. Reads the file as a string (`read-string`).
2. Chooses a meta-library based on `top-environment` (`(scheme base)` for r7rs, `(chezscheme)` for r6rs, etc.).
3. Creates a `document` with URI, text, and the meta-library’s identifier table.
4. Runs `source-file->annotations` on the already-read string (not re-reading from disk) to produce a list of annotated AST nodes.
5. Wraps each annotation in an `index-node` and stores the list in the document.

If the file cannot be read, an empty document is produced.

### 3.4 `init-library-node`

Walks the VFS tree and extracts library identifiers from every file. Files that contain a `(library ...)` form are inserted under their identifier path in the hierarchical `library-node` tree. Files without a library declaration (script files) are attached directly to the root library node so that they still participate in the dependency graph and incremental refresh pipeline.

The root library node is created automatically; children are attached by `make-library-node`.

### 3.5 `init-file-linkage`

Builds the dependency graph. See `doc/file-linkage.md` for details. The result is an adjacency matrix plus bidirectional path↔id maps.

### 3.6 `init-references`

Accepts a list of **batches** (each batch is a list of file paths).

If `threaded?` is true the batch is processed inside a `with-mutex` block. Before dispatching parallel work, `init-references` first serially extracts `syntax-diagnoses` and clears per-document state (`document-diagnoses-set!` and `clear-references-for`) for every path in the batch. It then uses `threaded-map` to run `private-init-references` on each path concurrently. If false, plain `for-each` is used and the same extract-then-clear-then-analyse sequence happens serially.

`private-init-references` performs the actual per-file analysis:

1. Runs the abstract interpreter (`step`) with the current file-node, library-node, linkage, and document.
2. Runs `process-library-identifier-excluded-references` to resolve identifiers that are not covered by the library system (e.g. top-level bindings).
3. Optionally runs `construct-substitutions-for` (type inference) if `type-inference?` is enabled. Errors during type inference are caught and logged as warnings rather than crashing the server.
4. Marks the document as **not refreshable** (`document-refreshable?-set! document #f`), indicating it is up-to-date.

---

## 4. Incremental Update

When the user edits a file the LSP client sends `textDocument/didChange`. The server calls `update-file-node-with-tail` followed later by `refresh-workspace-for`.

### 4.1 `update-file-node-with-tail`

Updates a single document with new text and prepares the workspace for a minimal re-analysis.

Steps:

1. **Snapshot old library identifiers** of the target file.
2. **Parse new text** with `source-file->annotations` and replace the document’s `index-node-list` and `text`.
3. **Mark reverse dependents refreshable**: using the *old* linkage graph, every file that references the target file (via `get-reference-path-to`) is marked `refreshable? = #t`.
4. **Compare library identifiers**: if the set of `(library ...)` names changed, the file may have changed its export contract.
   - Detach the file from old library nodes (and prune empty library nodes) using `for-each`.
   - Re-attach it under the new library identifiers using `for-each`.
   - **Rebuild the entire dependency graph** (`init-file-linkage`) because the mapping from imports to files may have shifted globally.
   - Mark the new reverse dependents refreshable using the *new* linkage graph.

This is the most expensive path; in practice library-header edits are rare compared to body edits.

### 4.2 `refresh-workspace-for`

Performs the actual re-analysis after a document has been marked refreshable.

1. If the document is not refreshable, does nothing.
2. If the file has no library identifiers, appends its path to `undiagnosed-paths` and re-analyses that single file only.
3. Otherwise:
   - Calls `refresh-file-linkage&get-refresh-path` to update the linkage row for this file and obtain the transitive closure of affected files (backwards along dependency edges).
   - Restricts the closure to files that are currently marked refreshable.
   - Calls `shrink-paths` (topological batching from `file-linkage`) to order the refreshable subset into parallel-safe batches.
   - Appends all affected paths to `undiagnosed-paths` so that the diagnostic publisher knows which files need fresh diagnostics.
   - Runs `init-references` on the batches.

### 4.3 `refresh-workspace`

A blunt but safe escape hatch: rebuilds the VFS, library tree, and linkage graph from scratch, then re-analyses everything and resets `undiagnosed-paths` to the full set of analysed paths. Used when the server detects a situation that incremental logic cannot handle reliably.

---

## 5. File-System Mutations

A helper allows the VFS to reflect file creation without a full workspace rebuild.

### 5.1 `attach-new-file`

Inserts a new path into the existing VFS tree.

- If the path is rejected by the filter or does not exist, returns `'()`.
- If the path is already present, returns the existing node.
- If an intermediate directory on the path is already a child of the parent, recurses into it.
- Otherwise it looks for a prefix path in the parent directory list. If one is found, it creates the necessary intermediate directory nodes via `init-virtual-file-system` and finally the leaf file node; if not, it safely returns `'()`.

---

## 6. Threading & Type Inference

### 6.1 Threading

When `threaded?` is `#t`:

- A Chez Scheme mutex is created and stored in the workspace.
- `init-references` wraps each batch inside `(with-mutex mutex ...)`.
- Within the mutex it uses `threaded-map` to analyse files in a batch concurrently.

Because the entire batch (serial pre-phase + `threaded-map`) happens inside the mutex, editor sync and background analysis are fully isolated for the duration of the batch. Batches themselves are processed **serially** by the single-consumer request-queue, while files *inside* a batch run in parallel under the mutex. See [§7 Workspace Mutex](#7-workspace-mutex) for the design rationale.

### 6.2 Type Inference

When `type-inference?` is `#t`, `private-init-references` calls `construct-substitutions-for` after the abstract interpretation step. Any exception raised by the type engine is caught and logged with `warning`; the document is still marked up-to-date so that analysis of downstream files can proceed. This prevents a single complex file from freezing diagnostics for the whole project.

---

## 7. Call Chains & Integration

### 7.1 Server startup

```
protocol/connection.sls  or  run.ss
        ↓
init-workspace
        ↓
init-virtual-file-system → init-document → source-file->annotations
        ↓
init-library-node
        ↓
init-file-linkage
        ↓
get-init-reference-batches → shrink-ids
        ↓
init-references → private-init-references → step
                                              construct-substitutions-for (optional)
```

### 7.2 Document change (LSP `textDocument/didChange`)

```
protocol/apis/did-change.sls
        ↓
update-file-node-with-tail
        ↓ (later)
refresh-workspace-for
        ↓
refresh-file-linkage&get-refresh-path
        ↓
shrink-paths
        ↓
init-references
```

### 7.3 Diagnostic publication

`workspace-undiagnosed-paths` is consumed by the diagnostic loop (typically in the connection handler). After publishing diagnostics for a path the server removes it from the list.

---

## 8. Design Notes

- **Why rebuild linkage on library-header change?**  
  The mapping from an import clause such as `(ufo-match)` to an actual file path is resolved through the library-node tree. If a file changes its library name, every file that imports the old name (or the new name) may need to rebind symbols. Rebuilding the graph is the simplest correct strategy and header edits are rare in practice.

- **Why two-phase refresh (update + refresh)?**  
  LSP `didChange` notifications can arrive in rapid succession. By separating *text mutation* (`update-file-node-with-tail`) from *analysis* (`refresh-workspace-for`) the server can coalesce multiple keystrokes before paying the analysis cost.

- **Empty documents for unreadable files**  
  If `read-string` returns `#f` or an EOF object, `init-document` still produces a valid document with empty text. This keeps the VFS consistent and prevents null-pointer-like crashes later in the pipeline.

---

## 7. Workspace Mutex

### 7.1 Design purpose

`workspace-mutex` is **not** a generic lock that protects every mutable field in the workspace. Its purpose is specific:

> **Isolate editor document-sync operations from background analysis operations so that the workspace is never in a partially-updated state while `step` or `clear-references-for` is running.**

In other words, it is a **read/write exclusion barrier** between two actors:

| Actor | Operations | Files |
|-------|-----------|-------|
| **Editor (write)** | `didChange`, `didOpen`, `didClose`, `did-change-watched-files` | `protocol/apis/document-sync.sls`, `protocol/apis/file-change-notification.sls` |
| **Background analysis (read + derived write)** | `init-references` → `step` → write references/diagnoses | `analysis/workspace.sls` |

When `threaded?` is `#f` the mutex is `'()` and never acquired; the single thread naturally serialises everything. When `threaded?` is `#t` the mutex is created via `(make-mutex)` and used at every boundary where editor traffic and analysis could otherwise interleave.

### 7.2 Why this matters

`update-file-node-with-tail` (called by `didChange`) performs a **wholesale replacement** of a document's core state:

1. Re-tokenises the text (`source-file->annotations`), producing new `document-diagnoses`.
2. Rebuilds the AST (`document-index-node-list`).
3. Replaces `document-text` and `document-line-length-vector`.
4. Updates `file-linkage` and `library-node` if the library header changed.

If `step` (or `clear-references-for`) is traversing the old `index-node-list` while `didChange` swaps the tree out from under it, the result is a dangling pointer or a half-initialised node — exactly the kind of crash that `c752796` fixed by serialising the clear phase.

### 7.3 Critical section in `init-references`

```scheme
(if (workspace-threaded? workspace-instance)
  (with-mutex (workspace-mutex workspace-instance)
    (let ([path+syntax-pairs
        (map
          (lambda (path)
            (let* ([...]
                [syntax-diagnoses 
                  (filter (lambda (d) (string-prefix? "Syntax error:" (cadddr d))) 
                    (document-diagnoses document))])
              (document-diagnoses-set! document '())
              (clear-references-for (car index-node-list))
              (cons path syntax-diagnoses)))
          paths)])
      (threaded-map 
        (lambda (pair) (private-init-references workspace-instance (car pair) (cdr pair)))
        path+syntax-pairs)))
  ...)
```

The `with-mutex` block now covers the **entire batch**: serial extraction of `syntax-diagnoses`, clearing of per-document state, and the subsequent `threaded-map` parallel analysis. This guarantees that no editor `didChange` can interleave with `step` or `clear-references-for` at any point during the batch.

**Trade-off**: holding the mutex for the full duration of `threaded-map` means `didChange` notifications are blocked until the batch finishes. In practice the batches are small (topological slices from `shrink-paths`) and the blocking time is acceptable. The alternative — running `threaded-map` outside the mutex — left a theoretical race where `didChange` could replace `document-index-node-list` while a `default-pool` worker was still traversing the old tree via `step`.

### 7.4 Critical section in `document-sync.sls`

```scheme
(define (did-change workspace params)
  (let ([body (lambda () ... (update-file-node-with-tail workspace file-node text) ...)])
    (if (null? (workspace-mutex workspace))
      (body)
      (with-mutex (workspace-mutex workspace) (body)))))
```

The body of `did-change` mutates document text, re-parses, and rebuilds the index-node tree. Wrapping it in `workspace-mutex` ensures these mutations are atomic with respect to `init-references`.

### 7.5 Critical section in `file-change-notification.sls`

`did-change-watched-files` (file-system watcher events such as git checkout) can attach, update, or delete file nodes. It uses the same pattern:

```scheme
(if (null? (workspace-mutex workspace))
  (body)
  (with-mutex (workspace-mutex workspace) (body)))
```

### 7.6 Relationship to `request-queue-mutex`

The project deliberately maintains **two separate locks**:

| Lock | Protected resource | Held during |
|------|-------------------|-------------|
| `request-queue-mutex` | `queue` (slib queue) and `tickal-task-list` | `push`, `pop`, `remove:from-request-tickal-task-list` |
| `workspace-mutex` | Workspace mutable state (document, index-node, linkage, library-node) | `init-references` entire batch (serial pre-phase + `threaded-map`), `didChange`, `expire` callback |

Once a worker thread dequeues a task, `request-queue-pop` returns a thunk and **releases the queue mutex before the thunk is invoked**. The actual execution of `request-processor` (and the engine that wraps it) runs **outside** the queue mutex. This prevents a slow request from starving the I/O thread or the timer thread.

`workspace-mutex` is acquired only when the thunk actually touches workspace state.

### 7.7 Single-threaded fallback

When `threaded?` is `#f`, `(workspace-mutex workspace)` is `'()`. Every call site checks `(null? (workspace-mutex workspace))` and skips the lock. In this mode the request-queue still exists but has only one worker thread, so natural serialization makes the mutex unnecessary.


---

## 8. Workspace Cache Persistence

### Current implementation (`kimi` branch)

A FASL-based workspace cache is implemented and enabled via the `--cache-path`
CLI option. It persists the full workspace object graph (`file-node` tree,
`library-node` tree, documents, `identifier-reference` network, and
`file-linkage`) so that `init-references` is skipped on restart when files are
unchanged.

Benchmarks on a commodity machine:

| Fixture | Cold startup | Cached startup | Speedup |
|---------|--------------|----------------|---------|
| simple-lib | ~31 ms | ~1.3 ms | ~24x |
| two-libs | ~35 ms | ~1.4 ms | ~24x |
| Synthetic 100-copy simple-lib (200 files) | ~2484 ms | ~49 ms | ~50x |
| scheme-langserver itself (128 `.sls` files) | ~55,790 ms | ~1750 ms | ~32x |
| scheme-langserver, one file changed | ~58,846 ms | ~1900 ms | ~31x |

Key implementation details:

- Uses Chez native `fasl-read` / `fasl-write` with binary ports.
- A manifest records `format-version`, `langserver-version`, `chez-version`,
  `machine-type`, `record-fingerprint`, facet, and runtime flags; any mismatch
  triggers a cold start.
- `file-linkage-path->id-map` is an `equal-hashtable`; because Chez `fasl-write`
  only supports `eq-hashtable`, it is serialized as an alist and rebuilt on load.
- Procedure-valued fields (`index-node-expansion-generator`,
  `identifier-reference-syntax-expander`) are cleared before save and regenerated
  on demand after load.
- Runtime state (`document-diagnoses`, `workspace-undiagnosed-paths`) is cleared
  before save.
- Incremental refresh (Phase 3): when only some files differ from the cache,
  added/deleted/changed files are processed with `attach-new-file`,
  `private:delete-file-node`, and `update-file-node-with-tail` +
  `refresh-workspace-for`; unchanged files keep their cached analysis.

CLI usage:

```bash
./run --cache-path ~/.cache/scheme-langserver
```

The cache file is `<cache-path>/workspace.fasl`. It is written when the server
receives an LSP `exit` or `shutdown` request. If the process crashes, the
previous cache file is still valid on the next start. Because FASL is tied to a
specific Chez version and machine type, the cache must not be shared across
machines.

### Cache consistency on load

When `init-workspace` is called with a `cache-path`, it first loads the FASL
payload and then compares every cached `document-text` with the current disk
contents. The comparison produces three sets:

- **Changed files**: present in both cache and disk, but content differs.
- **Deleted files**: present in cache but missing on disk.
- **New files**: present on disk but missing in cache.

If all three sets are empty, the cached workspace is returned unchanged. If any
set is non-empty, the server applies an incremental refresh:

1. Deleted files are removed from the VFS, `file-linkage`, and `library-node`
   tree.
2. New files are attached to the VFS via `attach-new-file`.
3. Changed files are re-parsed with `update-file-node-with-tail`.
4. `refresh-workspace-for` is invoked for changed and new files, re-analyzing
   only the transitive closure of affected files.

If the cache file is missing, corrupted, or its manifest does not match the
running server, the load is aborted and `init-workspace` falls back to a cold
start.

### Historical: `ufo-persistence` attempt (withdrawn)

An earlier attempt used `ufo-persistence` to skip only file I/O, parsing, and VFS
construction. It persisted annotations as plain s-expressions and rebuilt
`file-linkage` from scratch, but still had to re-run `init-references`. The
result was no meaningful speedup (~1.01x). The lesson learned: **serialization
alone does not speed up startup** when the dominant cost is the abstract
interpreter; the cache must skip the dominant phase entirely.
