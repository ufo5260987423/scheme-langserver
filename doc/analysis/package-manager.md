# Package Manager Integration (Akku)

This document covers the interaction between `scheme-langserver` and the Akku package manager, specifically how `.akku/list` is used to build the virtual-file-system filter, and how included SRFI implementation files are attached to the correct library nodes.

---

## 1. Akku File Filter

`analysis/package-manager/akku.sls` builds an acceptable-file filter from `.akku/list`. Only files listed there are scanned into the virtual file system.

### 1.1 The percent-decode bug

Commit `8aaab25` added `private:percent-decode` when reading paths from `.akku/list`:

```scheme
(hashtable-set! path->library
                (string-append root (private:percent-decode target-path))
                target-library)
```

The intent was to handle percent-encoded library names such as `%3a13` (which decodes to `:13`). However, Akku's `.akku/list` already contains the **actual filesystem paths**, and these paths are mixed:

| Kind | Example path in `.akku/list` | Filesystem path |
|------|------------------------------|-----------------|
| R6RS library wrapper | `.akku/lib/srfi/:13/strings.chezscheme.sls` | decoded path |
| Included implementation | `.akku/lib/srfi/%3a13/srfi-13.scm` | percent-encoded path |

Decoding `%3a13/srfi-13.scm` produced `:13/srfi-13.scm`, which does not exist on disk. Consequently all percent-encoded included files were rejected by the filter and never entered the VFS.

### 1.2 Fix

The percent-decode step was removed:

```scheme
(hashtable-set! path->library
                (string-append root target-path)
                target-library)
```

`.akku/list` paths are used verbatim because they already match the filesystem.

### 1.3 Test updates

The following tests were misleading and would need correction if the filter is revisited:

- `tests/analysis/package-manager/test-akku.sps`: the path `:152/r7rs-shim.scm` is a decoded ghost path; the real file is `%3a152/r7rs-shim.scm`.
- `tests/virtual-file-system/test-vfs.sps`: `walk-file` returning `'()` and then passing through `file-node-document` raised an exception that `test-equal` caught as `#f`, producing a false positive.

---

## 2. Include/Resolve Reference Attachment

SRFI libraries often delegate their implementation to an included file:

```scheme
(SRFI-23-error->R6RS "(library (srfi :13 strings))"
  (include/resolve ("srfi" "%3a13") "srfi-13.scm"))
```

### 2.1 Problem

When `srfi-13.scm` was finally loaded into the VFS (after the filter fix), the identifiers it defined were being attached to the wrong parent node — typically the `SRFI-23-error->R6RS` macro call node instead of the enclosing `(library (srfi :13 strings))` node.

This meant that exported identifiers such as `string-prefix?` could not be resolved through normal library-scope reference lookup, so callers like `uri-is-path?` saw `string-prefix?` as an unbound external identifier and inferred its return type as `something?`.

### 2.2 Fix

`include-resolve-process` now walks up to the nearest enclosing `(library ...)` or `(define-library ...)` node before attaching references.

### 2.3 Known limitation: `uri-is-path?`

Even with the filter and attachment fixes, `uri-is-path?` in `(scheme-langserver util path)` is still inferred as:

```text
(something? <- (inner:list? something?))
```

Source:

```scheme
(define (uri-is-path? str)
  (string-prefix? str "file://"))
```

Reason: `string-prefix?` is implemented via `let-string-start+end2` and other macros inside `srfi-13.scm`. The current type-inference subsystem does **not** expand macros when inferring types, so the predicate nature of `string-prefix?` is lost. This is considered a known architectural limitation rather than a bug.

---

## 3. Impact

The Akku filter fix and the include-attachment fix together restore analysis coverage for all SRFI libraries that use `include/resolve`. Without them, large parts of the standard library appear empty to the type inference and identifier-resolution phases.
