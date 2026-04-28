# AGENTS.md — scheme-langserver Agent Guide

This file contains project conventions, build steps, test patterns, and gotchas
that are useful for AI agents working on the scheme-langserver codebase.

---

## 1. Project Overview

**scheme-langserver** is a Language Server Protocol (LSP) implementation for
Scheme, written in **Chez Scheme** and managed with the **Akku** package manager.

Key subsystems:
- `virtual-file-system/` — File-node tree, library-node tree, documents, index-nodes
- `analysis/` — Tokenizer, abstract interpreter, identifier reference resolution,
  type inference, dependency graph (file-linkage)
- `protocol/` — LSP message parsing and API handlers
- `util/` — Shared utilities (matrix, dedupe, path, io, etc.)

The server supports multiple Scheme dialects: **r6rs** (default), **r7rs**, **s7**.

---

## 2. Build & Environment

### Prerequisites
- Chez Scheme (`scheme` binary)
- Akku (`akku` binary) for dependency management

### Activating the environment
```bash
# Always source this before running anything
source .akku/bin/activate
```

This sets `CHEZSCHEMELIBDIRS` so Chez can find libraries under `.akku/lib/` and
`.akku/libobj/`.

### Compiling the server
```bash
bash build.sh
```
This produces a static binary via `compile-chez-program run.ss --static`.

### Running the server
```bash
scheme --script run.ss
# or
./run
```

---

## 3. Testing Conventions

### Test framework
Tests use **SRFI-64** (`(srfi :64 testing)`).

Boilerplate at the top of every test file:
```scheme
#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    ...)
```

Basic pattern:
```scheme
(test-begin "group-name")
  (test-equal expected actual)
  (test-equal #t (predicate? value))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
```

### Running tests

**Single test file** (fast, preferred during development):
```bash
source .akku/bin/activate
scheme --script tests/analysis/dependency/test-file-linkage.sps
```

**All tests** (slow, run via `test.sh`):
```bash
bash test.sh
```

### Key gotcha: stale `.so` cache

Akku caches compiled `.so` files under `.akku/libobj/`. **After editing any
`.sls` source file, delete the corresponding `.so` cache** before running tests,
or you will see errors like:

- `incompatible fasl-object version`
- `variable <name> is not bound`
- silent use of old code

Safe incantation after editing `analysis/**/*.sls`:
```bash
rm -rf .akku/libobj/scheme-langserver
```

If the error persists, also clear workspace-level caches:
```bash
rm -f .akku/libobj/scheme-langserver/analysis/workspace.chezscheme.so
rm -f .akku/libobj/scheme-langserver/analysis/workspace.chezscheme.wpo
```

---

## 4. Fixture Structure

Workspace fixtures live under `tests/resources/workspace-fixtures/<name>/`.

A minimal fixture for testing workspace / linkage / identifier analysis:

```
tests/resources/workspace-fixtures/simple-lib/
├── lib.scm.txt          # r6rs library source (renamed to .txt for txt-filter)
└── consumer.scm.txt     # another library that imports lib
```

Use `.scm.txt` extension so `generate-txt-file-filter` accepts them.
Initialize in tests with:

```scheme
(let* ([fixture (string-append (current-directory)
                               "/tests/resources/workspace-fixtures/simple-lib")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       ...)
  ...)
```

`init-workspace` arguments:
1. `path` — absolute path to fixture directory
2. `'txt` — use `generate-txt-file-filter`
3. `'r6rs` — top environment (also `'r7rs`, `'s7`)
4. `#f` — threaded? (use `#f` in tests)
5. `#f` — type-inference? (use `#f` unless testing type inference)

Helper for locating children:
```scheme
(find (lambda (child) (string=? (file-node-path child) expected-path))
      (file-node-children root-file-node))
```

Or use `walk-file` for recursive lookup:
```scheme
(walk-file root-file-node (string-append fixture "/math.scm.txt"))
```

---

## 5. Coding Style

Observed conventions in existing code:

- **Indentation**: 2 spaces (no tabs). Indent by nesting level only; never align across lines.
  - General nesting: +2 spaces per level.
  - `let` / `let*` bindings: +4 spaces from the `let` keyword.
  - `let` / `let*` body: +2 spaces from the `let` keyword.
  - Function-call arguments inside `let` body: continue +2 per nesting level (do **not** flatten everything to the same column).
  ```scheme
  (import
    (chezscheme)
    (srfi :64 testing))

  (test-begin "group-name")
    (let* ([foo (init-foo)]
        [bar (workspace-bar foo)]
        [baz (construct-baz bar)])
      (process-baz baz)
      (test-equal #t
        (contain?
          (type:interpret-result-list baz)
          check-base)))
  (test-end)
  ```
- **Naming**:
  - Functions / variables: `kebab-case`
  - Record type accessors: `<record>-<field>` (e.g. `file-node-path`)
  - Private helpers: `private:<name>` or just internal `define`
- **Line endings**: LF.
- **Module headers**: `(library (scheme-langserver <path>) ...)`.
- **Imports**: group by subsystem, leave a blank line between groups.
- **Comments**: `;` for inline, `;;` for section dividers inside functions.

### Mutation style
Prefer `for-each` over `map` when the result is discarded (side-effect only).
This is a common fix in the codebase.

### Equality
- `string=?` for strings
- `equal?` for lists / deep structures
- `eq?` for symbols and small integers
- `=` for numeric comparison only

---

## 6. Module Dependency Rules

| Layer | May import from |
|-------|-----------------|
| `protocol/` | `analysis/`, `virtual-file-system/`, `util/` |
| `analysis/` | `virtual-file-system/`, `util/` |
| `virtual-file-system/` | `util/` only |
| `util/` | nothing inside the project (only standard libs) |

**Never** let `analysis/` import `protocol/`.

---

## 7. Common Traps & Debugging

### `directory-list` returns bare filenames
```scheme
(directory-list "/some/dir")
;; => ("foo.sls" "bar.sls")   -- NOT full paths
```
Always prepend the directory when constructing child paths:
```scheme
(string-append dir (string (directory-separator)) entry)
```

### `source-file->annotations` has two arities
- `(source-file->annotations path)` — re-reads from disk
- `(source-file->annotations source path)` — parses the provided string

Prefer the 2-arity version when you have already read the file into memory,
to avoid double I/O.

### Script files vs library files
- **Library files** have a `(library (name) ...)` header.
  `get-library-identifiers-list` returns a non-empty list.
- **Script files** have no library header.
  `get-library-identifiers-list` returns `'()`.

This distinction affects:
- `init-library-node` — script files attach directly under the root library-node
- `refresh-workspace-for` — script files bypass the linkage graph and go straight
to `undiagnosed-paths`

### `path->uri` and `uri->path`
Located in `util/path.sls`. The URI format is `file:///absolute/path`.
`path->uri` now correctly handles `.` and `..` in relative paths.

### Matrix operations in `util/matrix.sls`
- `encode` / `decode` use row-major order.
- `matrix-expand` grows a square matrix; `matrix-shrink` removes a row/column.
- Always derive dimension via `(sqrt (vector-length matrix))`.

### `ufo-match` wildcard
`ufo-match` uses `:_` as the "match anything, don't bind" wildcard, **not** `_`.
`_` is treated as a normal pattern variable.

### Finding tests that exercise a module
```bash
grep -r "library-import-process" tests/
```

### Checking if a symbol is exported
Look at the `(export ...)` list at the top of the `.sls` file.

### Pre-commit hook: never use `--no-verify`
The repository has a pre-commit hook (`.git/hooks/pre-commit`) that runs the protocol API test suite. **Do not bypass it with `git commit --no-verify`.** If the hook fails because tests are too slow or broken, fix the tests or the hook first, then commit normally.

---

## 8. Known Issues (as of current branch)

| Location | Issue | Impact |
|----------|-------|--------|
| `analysis/identifier/rules/library-import.sls` | `alias` modifier does not add refs when used inside a `(library ...)` form (script-level `import-process` works fine) | Low — `alias` is rare in library headers |
| `analysis/abstract-interpreter.sls:74` | Missing recursion guard for self-defined macro partial evaluation | Medium — can infinite-loop on certain macros |
| `protocol/apis/document-sync.sls:44` | Document sync has a TODO for optimization | Low — performance only |
| `doc/analysis/file-linkage.md:148` | Matrix shrink on file deletion is now implemented via `shrink-file-linkage!` | Resolved |

---

## 9. Useful One-Liners

```bash
# Run a single test file quickly
source .akku/bin/activate && scheme --script tests/analysis/test-workspace.sps

# Clear all compiled caches for the project
rm -rf .akku/libobj/scheme-langserver

# Find all .sls files under analysis/
find analysis -name "*.sls" | sort

# Check which tests import a specific module
grep -rl "library-import" tests/

# Count test assertions in a file
grep -c "test-equal\|test-assert" tests/analysis/dependency/test-file-linkage.sps
```

---

## 10. Architecture Cheat Sheet

### Workspace lifecycle
1. `init-virtual-file-system` — scan directory tree, create file-nodes + documents
2. `init-library-node` — build library-node tree from library headers
3. `init-file-linkage` — build dependency adjacency matrix
4. `init-references` — run abstract interpreter (`step`) over all files

### Incremental update flow
1. File changed → `update-file-node-with-tail` (or `attach-new-file`)
2. If library header changed → rebuild **entire** file-linkage + library-node tree
3. If content changed (header same) → `refresh-file-linkage&get-refresh-path`
4. `shrink-paths` produces topological batches
5. `init-references` re-runs `step` on affected batches

### Key record types
| Record | Fields (mutable marked) | Purpose |
|--------|------------------------|---------|
| `file-node` | path, name, parent, folder?, children, document | VFS node |
| `library-node` | identifier, parent, children, file-nodes | Library hierarchy |
| `document` | uri, text, index-node-list, ordered-reference-list, diagnoses | Parsed source |
| `index-node` | datum/annotations, parent, children, excluded-references, import-in-this-node, export-to-other-node | AST node |
| `file-linkage` | path->id-map, id->path-map, matrix | Dependency graph |
| `identifier-reference` | identifier, document, index-node, initialization-index-node, library-identifier, type, parents, type-expressions | Symbol reference |
