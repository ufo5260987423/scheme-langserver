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
path → identifier → top-environment → threaded? → type-inference? → facet
```

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
4. Runs `source-file->annotations` (the tolerant tokenizer) to produce a list of annotated AST nodes.
5. Wraps each annotation in an `index-node` and stores the list in the document.

If the file cannot be read, an empty document is produced.

### 3.4 `init-library-node`

Walks the VFS tree and, for every file that contains a `(library ...)` form, extracts its identifier list and inserts it into a hierarchical `library-node` tree. This tree is used later to map import clauses back to the files that provide them.

The root library node is created automatically; children are attached by `make-library-node`.

### 3.5 `init-file-linkage`

Builds the dependency graph. See `doc/file-linkage.md` for details. The result is an adjacency matrix plus bidirectional path↔id maps.

### 3.6 `init-references`

Accepts a list of **batches** (each batch is a list of file paths). For every batch it calls `private-init-references` on each path.

If `threaded?` is true the batch is processed with `threaded-map` inside a `with-mutex` block so that the shared workspace state is updated safely. If false, plain `for-each` is used.

`private-init-references` performs the actual per-file analysis:

1. Clears existing diagnoses.
2. Runs the abstract interpreter (`step`) with the current file-node, library-node, linkage, and document.
3. Runs `process-library-identifier-excluded-references` to resolve identifiers that are not covered by the library system (e.g. top-level bindings).
4. Optionally runs `construct-substitutions-for` (type inference) if `type-inference?` is enabled. Errors during type inference are caught and logged as warnings rather than crashing the server.
5. Marks the document as **not refreshable** (`document-refreshable?-set! document #f`), indicating it is up-to-date.

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
2. If the file has no library identifiers, re-analyses that single file only.
3. Otherwise:
   - Calls `refresh-file-linkage&get-refresh-path` to update the linkage row for this file and obtain the transitive closure of affected files (backwards along dependency edges).
   - Restricts the closure to files that are currently marked refreshable.
   - Calls `shrink-paths` (topological batching from `file-linkage`) to order the refreshable subset into parallel-safe batches.
   - Appends all affected paths to `undiagnosed-paths` so that the diagnostic publisher knows which files need fresh diagnostics.
   - Runs `init-references` on the batches.

### 4.3 `refresh-workspace`

A blunt but safe escape hatch: rebuilds the VFS, library tree, and linkage graph from scratch, then re-analyses everything. Used when the server detects a situation that incremental logic cannot handle reliably.

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

Because all shared mutable state updates happen inside the mutex, batches themselves are processed **serially** (preserving dependency order), while files *inside* a batch run in parallel.

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
