# Macro Auto-Resolution via `identifier-reference-syntax-expander`

## 1. Background & Motivation

scheme-langserver handles user-defined macros through two mechanisms:

1. **Hand-written rules** in `analysis/identifier/self-defined-rules/`.  Each macro
   (e.g. `ufo-match`, `ufo-try`, `goldfish/let1`) has a dedicated processor that
   understands its binding semantics.

2. **Generic expansion** for standard `syntax-rules` / `syntax-case` macros.  When
   `step` encounters `(define-syntax id (syntax-rules ...))`, an
   *expansion generator* is attached to the identifier.  Later, when the macro is
   called, the generator produces the expanded AST and `step` recurses into it.

The current experiment (in `router.sls:57-61`) **replaces** the hand-written
`match-process` for `ufo-match` with the generic expansion mechanism.  The goal
is to verify whether generic expansion can produce correct identifier bindings
for complex pattern-matching macros without maintaining a separate rule file per
macro.

---

## 2. Principle

### 2.1 Two phases of macro life-cycle

| Phase | Where | What happens |
|-------|-------|--------------|
| **Definition** | `abstract-interpreter.sls:step` | `define-syntax` is detected → `define-syntax:attach-generator` stores an expansion generator on the identifier-reference |
| **Call site** | `router.sls:route&add` | When the macro is invoked, the stored generator is retrieved and wrapped by `expansion-generator->rule` |

### 2.2 How the expansion generator is created

1. `step` visits `(define-syntax match (syntax-rules ...))`.
2. `establish-available-rules-from` matches `(define-syntax)` and attaches
   `define-syntax-process` **before** children + `define-syntax:attach-generator`
   **after** children.
3. `syntax-rules-process` (runs before children) calls
   `syntax-rules->generator:map+expansion`, which builds a closure that knows
   how to expand any call to `match`.
4. `define-syntax:attach-generator` (runs after children) extracts the generator
   from the `syntax-rules` child node and stores it via
   `identifier-reference-syntax-expander-set!` on every exported reference of
   `match`.

### 2.3 How the expansion generator is consumed

The new code in `router.sls:57-61`:

```scheme
[(and (equal? library-identifiers '((ufo-match)))
      (equal? expressions '(match))
      (identifier-reference-syntax-expander (car top)))
  (pretty-print 'trigger)
  (add-rule-procedure rules
    `((,(expansion-generator->rule
          (identifier-reference-syntax-expander (car top))
          step file-linkage expanded+callee-list memory))
      . ,target-identifier))]
```

Instead of attaching `match-process`, it:

1. Retrieves the stored `syntax-expander` from the `match` identifier-reference.
2. Wraps it with `expansion-generator->rule`, producing a **rule lambda**.
3. Attaches that rule to the `match` identifier so `step` will execute it when
   the macro is called.

### 2.4 What `expansion-generator->rule` does at call time

```scheme
(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory)
  (lambda (root-file-node root-library-node document index-node)
    (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
           [pairs+expansion (proc root-file-node root-library-node document index-node)])
      (if pairs+expansion
        (let* ([pairs (car pairs+expansion)]
               [expansion-index-node (cdr pairs+expansion)]
               [possible-new-memory `(,expression . ,memory)])
          (if (not (contain? memory expression))
            (step root-file-node root-library-node file-linkage document
                  expansion-index-node expanded+callee-list possible-new-memory))
          (private:shallow-copy pairs expansion-index-node document index-node))
        '()))))
```

Steps:
1. **Expand** the call site using the stored generator (`proc`).
2. **Recursion guard** — if the exact expression has been seen in `memory`, skip
   to avoid infinite loops.
3. **Recurse** — run `step` on the expanded AST so identifier bindings inside the
   expansion are resolved.
4. **Copy back** — `private:shallow-copy` copies identifier references from the
   expanded tree back to the original macro call site, so LSP features (hover,
   goto-definition) work on the un-expanded source.

---

## 3. Complete Call Path

```
abstract-interpreter.sls:step
    │
    ├── visits (define-syntax match (syntax-rules ...))
    │       │
    │       └── establish-available-rules-from
    │               │
    │               ├── matches '(define-syntax)
    │               │       attaches (define-syntax-process . define-syntax:attach-generator)
    │               │
    │               ├── syntax-rules-process (pre-procedure)
    │               │       └── syntax-rules->generator:map+expansion
    │               │               builds expansion generator closure
    │               │               stores it on index-node-expansion-generator
    │               │
    │               └── define-syntax:attach-generator (post-procedure)
    │                       extracts index-node-expansion-generator
    │                       identifier-reference-syntax-expander-set!
    │                               on every exported reference of 'match
    │
    └── later, visits (match expr (pat body) ...)
            │
            ├── establish-available-rules-from
            │       │
            │       ├── built-in rules don't match
            │       │
            │       └── route&add
            │               │
            │               ├── old path (commented out):
            │               │       attaches match-process
            │               │
            │               └── new experimental path:
            │                       checks (identifier-reference-syntax-expander (car top))
            │                       is non-#f
            │                       wraps it with expansion-generator->rule
            │                       attaches the wrapped rule
            │
            ├── rule pre-procedure runs
            │       └── expansion-generator->rule lambda
            │               ├── proc expands the call site
            │               ├── checks memory for recursion
            │               ├── step recurses into expanded AST
            │               └── private:shallow-copy copies refs back
            │
            └── step continues into children of original call site
```

---

## 4. Comparison: Hand-Written Rule vs Generic Expansion

| Aspect | `match-process` (hand-written) | `expansion-generator->rule` (generic) |
|--------|-------------------------------|---------------------------------------|
| **Binding semantics** | Explicitly knows `match` patterns: `_` is wildcard, `...` is repeat, `set!`, `and`, `or`, `not`, `?`, `=` are guards | Relies on expansion — pattern variables become `let` bindings in the expanded code |
| **Position info** | References are attached directly to the original pattern AST nodes | References come from the expanded AST; `shallow-copy` maps them back, potentially losing granularity |
| **Performance** | No expansion cost; single pass over patterns | Must expand the macro, parse the expansion string back into index-nodes, then run `step` on it |
| **Maintenance** | One file per macro family | Zero extra code per `syntax-rules` macro |
| **Coverage** | Only macros with hand-written rules | Any macro defined via `syntax-rules` / `syntax-case` (if generator is attached) |

---

## 5. Known Bugs & Issues

### 5.1 Debug code left in production

`(pretty-print 'trigger)` at `router.sls:58` prints to stdout/stderr every time
a `match` macro is resolved.  This pollutes the LSP I/O stream and should be
removed.

### 5.2 Hard-coded to `ufo-match` only

The condition explicitly checks `(equal? library-identifiers '((ufo-match)))`.
If the experiment succeeds, the logic should be **generalised** to any macro
whose `identifier-reference-syntax-expander` is non-`#f`:

```scheme
[(identifier-reference-syntax-expander (car top))
  (add-rule-procedure rules
    `((,(expansion-generator->rule
          (identifier-reference-syntax-expander (car top))
          step file-linkage expanded+callee-list memory))
      . ,target-identifier))]
```

Placing this as the first `cond` clause (after the `srfi` special cases) would
let it serve as a **fallback** before falling through to `else rules`.

### 5.3 `private:shallow-copy` may map bindings imprecisely

`shallow-copy` recursively collects all `export` and `import` references from the
expanded tree and copies them to the original call site.  For a macro like
`match`, a pattern variable `x` in `(match v [(list x y) (+ x y)])` becomes a
`let` binding in the expansion.  `shallow-copy` will attach the `let`-bound
reference to the original `(list x y)` pattern node.  This is usually correct for
hover/definition, but:

- **Goto-definition on `_`** — `_` is a wildcard in `match` but may become a
  normal `let` binding in the expansion.  The hand-written rule explicitly
  excludes `_` via `private:check?`; generic expansion does not.
- **Multiple occurrences of the same pattern variable** — `shallow-copy` copies
  the same reference to every occurrence, which is fine, but if the expansion
  renames or duplicates the variable, the mapping may be one-to-many in a way
  that confuses the client.

### 5.4 Recursion guard is expression-equality only

`memory` checks `(contain? memory expression)`.  If a self-referential macro
produces a *structurally different* expression at each expansion step (e.g. by
inserting a counter or gensym), the guard fails and `step` recurses forever.

This is the same issue documented at `abstract-interpreter.sls:74`:

> TODO: in case of self-defined macro's partially evaluation leading endless
> recursions, add a recursion avoid mechanism.

A **depth limit** (e.g. max 10 expansions) should be added to
`expansion-generator->rule`.

### 5.5 Non-`syntax-rules` macros silently ignored

If a macro is defined with `syntax-case` but the `syntax-case-process` fails to
attach a generator (e.g. because the macro body is too complex), or if the macro
is defined via `define-macro` (s7) or `eval`, `identifier-reference-syntax-expander`
remains `#f`.  The new path then falls through to `else rules`, meaning the
macro call receives **no identifier analysis at all**.

The old hand-written rule guaranteed at least basic binding detection for
`ufo-match`; the new path offers nothing as a fallback when the expander is
missing.

### 5.6 Double cost of expansion

The expansion generator:
1. Uses Chez Scheme's `eval` + `syntax-case` to determine the matching clause
   (`private:confirm-clause` in `syntax-rules.sls`).
2. Re-parses the expanded S-expression into a new index-node tree via
   `source-file->annotations`.
3. Runs `step` on the new tree.

For large `match` expressions with many clauses, this is significantly slower
than the hand-written `match-process`, which only walks the pattern nodes once.

---

## 6. Improvement Opportunities

### 6.1 Generalise the auto-resolve fallback

Remove the `ufo-match` hard-coding and make `identifier-reference-syntax-expander`
the default mechanism for any macro that has it:

```scheme
(define (route&add ...)
  (let* (...)
    (cond
      ; ... srfi special cases ...

      ; Generic auto-resolution for syntax-rules / syntax-case macros
      [(identifier-reference-syntax-expander (car top))
       (add-rule-procedure rules
         `((,(expansion-generator->rule
               (identifier-reference-syntax-expander (car top))
               step file-linkage expanded+callee-list memory))
           . ,target-identifier))]

      ; Legacy hand-written rules (kept for macros without generators)
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
       (add-rule-procedure rules `((,try-process) . ,target-identifier))]

      [else rules])))
```

### 6.2 Add expansion-depth limit

In `expansion-generator->rule`, track depth and abort after a threshold:

```scheme
(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory)
  (lambda (root-file-node root-library-node document index-node)
    (let* ([depth (or (assq 'expansion-depth memory) 0)]
           [...])
      (if (> depth 10)
        '()
        (let ([new-memory (cons `(expansion-depth . ,(+ 1 depth)) memory)])
          ...)))))
```

### 6.3 Remove debug print

Delete `(pretty-print 'trigger)` from `router.sls`.

### 6.4 Cache expansions

Many macro calls in a file use the same macro with identical or similar
arguments.  The expansion generator could cache `(expression → expanded-index-node)`
pairs per document to avoid re-evaluating `syntax-case` repeatedly.

### 6.5 Improve `private:shallow-copy` for pattern macros

For pattern-matching macros specifically, `shallow-copy` could be enhanced to
understand that symbols matching `syntax-parameter` references in the template
should be mapped back to the original callee nodes, while other bindings (introduced
by the expansion itself, e.g. helper `let`s inside the macro) should **not** be
copied back.  This would prevent hover on a `match` clause from jumping to an
internal helper variable.

---

## 7. Files Involved

| File | Role |
|------|------|
| `analysis/identifier/self-defined-rules/router.sls` | Decides whether to use hand-written rule or generic expansion |
| `analysis/identifier/expanders/expansion-wrap.sls` | Wraps an expansion generator as a `step` rule |
| `analysis/identifier/expanders/syntax-rules.sls` | Builds expansion generators for `syntax-rules` macros |
| `analysis/identifier/rules/define-syntax.sls` | Attaches generators to identifier-references at definition time |
| `analysis/abstract-interpreter.sls` | Core `step` function; orchestrates rules and expansion |
| `analysis/identifier/macro-expander.sls` | Utilities for step-by-step expansion and callee/expanded pairing |
| `analysis/identifier/self-defined-rules/ufo-match/match.sls` | Hand-written rule (currently commented out in router) |
