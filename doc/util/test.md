# `(scheme-langserver util test)` — AST Search Helpers for Tests

This library provides **structure-based AST search utilities** for tests. It was introduced to eliminate brittle, hard-coded `(line, column)` coordinate lookups (via `text+position->int`) that break whenever a source file is edited.

## Motivation

Many older tests used patterns like:

```scheme
(let* ([text (document-text document)]
       [target-index-node
         (pick-index-node-from
           index-node-list
           (text+position->int text 22 6))])
  ...)
```

Any insertion of a comment, blank line, or indentation change shifts all subsequent coordinates, causing silent test failures. The helpers in this module replace coordinate-based targeting with **semantic AST searches** (e.g. "find the `define` named `encode`" or "find the `let` binding for `column-id`").

## Setup

Import the library in test files:

```scheme
(import (scheme-langserver util test))
```

> **Symlink note**: Because this module lives under the project tree (`util/test.sls`) rather than in an Akku package, Chez Scheme needs a symlink inside `.akku/lib/` to resolve it at runtime:
>
> ```bash
> ln -s ../../../../util/test.sls .akku/lib/scheme-langserver/util/test.sls
> ```
>
> If you run `akku install` and the symlink disappears, recreate it.

## API Reference

### `annotation-stripped-expression index-node → expr`

Shorthand for `(annotation-stripped (index-node-datum/annotations index-node))`.

```scheme
(annotation-stripped-expression some-node)
;; => (define (encode b1 b2 b3) ...)
```

---

### `find-index-node-recursive predicate root-node → index-node | #f`

Depth-first search over the AST starting at `root-node`. Returns the first node for which `predicate` returns true, or `#f` if none matches.

```scheme
;; Find the first 'match expression in a document
(find-index-node-recursive
  (lambda (n)
    (let ([expr (annotation-stripped-expression n)])
      (and (list? expr) (not (null? expr))
           (eq? 'match (car expr)))))
  root-index-node)
```

---

### `find-define-with-params root-node name → index-node | #f`

Find a function definition of the form `(define (name ...) ...)`.

```scheme
;; Locate (define (matrix-from rows columns) ...) in util/matrix.sls
(find-define-with-params root-index-node 'matrix-from)
```

---

### `find-define-by-name root-node name → index-node | #f`

Find a definition by name, matching **either**:
- `(define (name ...) ...)` — function definition
- `(define name ...)` — variable definition

```scheme
;; Find (define contain? ...) in util/contain.sls
(find-define-by-name root-index-node 'contain?)
```

---

### `define-node->name-node define-node → index-node | #f`

Given a `define` node (found by the functions above), extract the **name symbol node**.

Works for both shapes:
- `(define (name ...) ...)` → the `name` node inside the parameter list
- `(define name ...)` → the `name` node as the second child of `define`

```scheme
(let ([encode-node (find-define-with-params root-index-node 'encode)])
  (define-node->name-node encode-node))
;; => index-node whose stripped expression is the symbol 'encode
```

---

### `find-let-node root-node → index-node | #f`

Find the first standard `let` (i.e. `(let ((binding ...) ...) body ...)`) in the AST.

---

### `find-named-let root-node name → index-node | #f`

Find a named `let` of the form `(let name ((binding ...) ...) body ...)`.

```scheme
;; Find the "loop" inside matrix-from
(find-named-let matrix-from-node 'loop)
```

---

### `find-binding-node let-node binding-name → index-node | #f`

Inside a `let` or `let*` node, find the binding pair `(name init)` by `name`.

```scheme
;; Find the [column-id 0] binding inside a named let
(find-binding-node loop-node 'column-id)
;; => index-node for (column-id 0)
```

---

### `let-body-nodes let-node → list of index-node`

Return the body expression nodes of a `let`/`let*` (skipping the keyword, optional name, and bindings).

```scheme
(let-body-nodes loop-node)
;; => (body-expr1 body-expr2 ...)
```

---

### `find-symbol-in-body let-node symbol-name → index-node | #f`

Search the body of a `let`/`let*` for the first occurrence of `symbol-name`.

```scheme
;; Find where 'str is used inside a typed-lambda body
(find-symbol-in-body typed-lambda-node 'str)
```

## Complete Example

Refactoring a type-inference test from coordinates to AST search:

**Before (brittle):**
```scheme
(let* ([target-text (document-text target-document)]
       [target-index-node
         (pick-index-node-from
           (document-index-node-list target-document)
           (text+position->int target-text 4 12))])
  ...)
```

**After (robust):**
```scheme
(let* ([root-index-node (car (document-index-node-list target-document))]
       [encode-node (find-define-with-params root-index-node 'encode)]
       [target-index-node
         (find-index-node-recursive
           (lambda (n) (eq? 'm (annotation-stripped-expression n)))
           encode-node)])
  ...)
```

## Tips

- **Use precise predicates** when `find-index-node-recursive` could match multiple nodes. For example, match both the operator (`syntax-case`) and its arguments (`first`, `(else =>)`) to disambiguate between several `syntax-case` forms in the same file.
- **Check `annotation-stripped-expression`** before writing a predicate; it is usually easier to match against plain S-expressions than against raw annotations.
- **For parameter symbols**, first locate the surrounding `define`/`lambda`, then search recursively for the symbol name inside that subtree. This avoids accidentally matching a同名 symbol elsewhere in the file.
