# Macro Auto-Resolution via `identifier-reference-syntax-expander`

## 1. Overview & Architecture

### 1.1 Background & Motivation

scheme-langserver handles user-defined macros through two mechanisms:

1. **Hand-written rules** in `analysis/identifier/self-defined-rules/`. Each macro
   (e.g. `ufo-match`, `ufo-try`, `goldfish/let1`) has a dedicated processor that
   understands its binding semantics.

2. **Generic expansion** for standard `syntax-rules` / `syntax-case` macros. When
   `step` encounters `(define-syntax id (syntax-rules ...))`, an
   *expansion generator* is attached to the identifier. Later, when the macro is
   called, the generator produces the expanded AST and `step` recurses into it.

The generic expansion mechanism can be activated in `router.sls` to replace
hand-written rules on a per-macro or per-library basis. The goal is to verify
whether generic expansion can produce correct identifier bindings for complex
pattern-matching macros without maintaining a separate rule file per macro.

### 1.2 Two Phases of Macro Life-Cycle

| Phase | Where | What happens |
|-------|-------|--------------|
| **Definition** | `abstract-interpreter.sls:step` | `define-syntax` is detected → `define-syntax:attach-generator` stores an expansion generator on the identifier-reference |
| **Call site** | `router.sls:route&add` | When the macro is invoked, the stored generator is retrieved and wrapped by `expansion-generator->rule` |

### 1.3 How the Expansion Generator Is Created

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

### 1.4 How the Expansion Generator Is Consumed

In `router.sls`, instead of attaching `match-process`, the auto-resolve path:

```scheme
[(and (equal? library-identifiers '((ufo-match))) (equal? expressions '(match)))
  (add-rule-procedure rules
    `((,(expansion-generator->rule
          (identifier-reference-syntax-expander target-identifier)
          step file-linkage expanded+callee-list memory target-identifier))
      . ,target-identifier))]
```

1. Retrieves the stored `syntax-expander` from the `match` identifier-reference.
2. Wraps it with `expansion-generator->rule`, producing a **rule lambda**.
3. Attaches that rule to the `match` identifier so `step` will execute it when
   the macro is called.

### 1.5 What `expansion-generator->rule` Does at Call Time

```scheme
(define (expansion-generator->rule proc step file-linkage expanded+callee-list memory . maybe-expander-ref)
  (let ([expander-ref (if (null? maybe-expander-ref) #f (car maybe-expander-ref))])
    (lambda (root-file-node root-library-node document index-node)
      (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
             [pairs+expansion (proc root-file-node root-library-node document index-node)])
        (if pairs+expansion
          (let* ([pairs (car pairs+expansion)]
                 [expansion-index-node (cdr pairs+expansion)]
                 [possible-new-memory `(,expression . ,memory)]
                 [expander-doc (if expander-ref (identifier-reference-document expander-ref) #f)]
                 [new-expanded+callee-list
                   (cons `(,expansion-index-node ,index-node ,expander-doc) expanded+callee-list)])
            ; Guard 1: prevent re-expansion of the exact same expression.
            ; Guard 2: cap memory chain length to avoid infinite cascades.
            (if (and (not (contain? memory expression))
                     (< (length memory) 10))
              (step root-file-node root-library-node file-linkage document
                    expansion-index-node new-expanded+callee-list possible-new-memory))
            (private:shallow-copy pairs expansion-index-node document index-node))
          '())))))
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

## 2. Complete Call Path

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
            │               ├── hand-written path:
            │               │       attaches match-process
            │               │
            │               └── auto-resolve path:
            │                       checks (identifier-reference-syntax-expander target-identifier)
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

## 3. Match Macro Expansion in Detail

### 3.1 Match Syntax-Rules Definition

```scheme
(define-syntax match
  (syntax-rules ()
    ((match atom (pat . body) ...)
     (let ((v atom))
       (match-next v (atom (set! atom)) (pat . body) ...)))
    ...))
```

### 3.2 Chez Scheme Expansion Walkthrough

`match` is a pattern-matching macro that expands declarative `pat → body`
syntax into procedural `if`/`let` nesting, turning pattern variables into
`let` bindings.

#### Example 1: `(match '(1) [(s) s])`

**Layer 1 — match:**
```scheme
(let ((v '(1)))
  (match-next v ('(1) (set! '(1))) ((s) s)))
```

**Layer 2 — match-next:**
Wraps each clause with an anonymous failure continuation:
```scheme
(let ((v '(1)))
  (let ((failure (lambda () (error 'match "no matching pattern"))))
    (match-one v (s) ('(1) (set! '(1)))
               (match-drop-ids (begin s))
               (failure)
               ())))
```

**Layer 3 — match-one:**
Checks ellipsis (`...`). `(s)` is not ellipsis, passes through:
```scheme
(let ((v '(1)))
  (let ((failure ...))
    (if (and (pair? v) (null? (cdr v)))
        (let ((w (car v)))
          (match-one w s ((car v) (set-car! v))
                     (match-drop-ids (begin s))
                     (failure)
                     ()))
        (failure))))
```

**Layer 4 — match-two (symbol pattern):**
```scheme
(match-check-identifier
  s
  (let-syntax
      ((new-sym?
        (syntax-rules ()
          ((new-sym? s sk2 fk2) sk2)
          ((new-sym? y sk2 fk2) fk2))))
    (new-sym? random-sym-to-match
              (let ((s w)) (begin s))
              (if (equal? w s) (begin s) (failure))))
  (if (equal? w s) (begin s) (failure)))
```

`new-sym?` uses the `syntax-rules` literal list `(id ...)` (currently empty) to
distinguish "already bound" from "new variable":
- `s` is not in `id ...` → pattern variable → `random-sym-to-match` matches → `sk2`
- Result: `(let ((s w)) (begin s))`

**Final expanded form:**
```scheme
(let ((v '(1)))
  (if (and (pair? v) (null? (cdr v)))
      (let ((s (car v)))
        s)
      (error 'match "no matching pattern")))
```

#### Example 2: `(match '(1 2) [(a b) (+ a b)])`

Layer 3 uses pair pattern `(p . q)`:
```scheme
(if (pair? v)
    (let ((w (car v)) (x (cdr v)))
      (match-one w a ... (match-one x b ... (match-drop-ids (begin (+ a b))) ...) ...))
    (failure))
```

Recursive `match-two` on `a` and `b` produces:
```scheme
(let ((v '(1 2)))
  (if (pair? v)
      (let ((a (car v))
            (b (cadr v)))
        (+ a b))
      (error 'match "no matching pattern")))
```

### 3.3 Auto-Resolver Substitution Walkthrough

This section traces the auto-resolver's treatment of the same `match` call.

**Call site AST:**
```scheme
(match expression
  [(_ (? string? path)) (let (...) ...)]
  [else '()])
```

#### Step 1 — `syntax-rules->generator:map+expansion`

**Input:** `local-index-node` = the `match` call index-node.

**Extract clause:** `private:confirm-clause` matches clause
`(_ atom (pat . body) ...)` and returns:
```scheme
(0 . (let ((v atom))
       (match-next v (atom (set! atom)) (pat . body) ...)))
```

**Bindings:**
| var | value |
|-----|-------|
| `atom` | `expression` index-node |
| `pat` | `(_ (? string? path))` `else` |
| `body` | `(let (...) ...)` `(quote ())` |

**Template pattern tree:**
```
list-form: (let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))
  ├── list-form: ((v atom))
  │     └── pair-form: (v . atom)
  └── list-form: (match-next v (atom (set! atom)) (pat . body) ...)
        ├── match-next
        ├── v
        ├── list-form: (atom (set! atom))
        └── ellipse-list-form: ((pat . body) ...)
              ├── pair-form: (pat . body)
              └── ellipse: ...
```

**Compound list (substitution result):**
```scheme
(let ((v expression-index-node))
  (match-next v
    (expression-index-node (set! expression-index-node))
    ((pat1 . body1) (pat2 . body2))))
```

#### Step 2 — `source-file->annotations` → `expansion-index-node`

The expansion expression is pretty-printed and re-parsed into a fresh AST:
```scheme
(let ((v expression))
  (match-next v
    (expression (set! expression))
    ((_ (? string? path)) (let (...) ...))
    (else (quote ()))))
```

#### Step 3 — `private:expansion+index-node->pairs`

Pairs map expansion AST nodes back to original/callee nodes:
- `let-node` ↔ `let-node`
- `match-next` ↔ `match-next`
- `v` ↔ `v`
- `((v expression))` ↔ `(((v expression)))`
- etc.

For ellipsed clauses, the `compound-list` contains N expanded pairs while the
`expansion-index-node` contains a flat list of N children. The current code
truncates both sides to the shorter length to avoid `map` length-mismatch
crashes (`private:take`).

#### Step 4 — `step` on expansion AST

`step` recursively visits the expanded tree:
- Processes `let`, establishes `v` binding
- Processes `match-next`, discovers it is a macro call, attempts expansion
- `match-next` expansion may produce new `match` calls, but `memory` contains
  the original call, so `step` skips

#### Step 5 — `private:shallow-copy`

```scheme
(private:shallow-copy pairs expansion-index-node document index-node)
```

1. `private:recursive-collect` gathers all `references-export-to-other-node` from
   `expansion-index-node` and its children.
2. For each identifier reference, looks up the corresponding node in `pairs`.
3. Creates new `identifier-reference` and appends it to the original node's
   `references-export-to-other-node`.
4. Updates `ordered-reference-list` for `compound-import-list` nodes.

---

## 4. Comparison: Auto-Resolve vs Hand-Written Rule

| Aspect | `match-process` (hand-written) | `expansion-generator->rule` (generic) |
|--------|-------------------------------|---------------------------------------|
| **Binding semantics** | Explicitly knows `match` patterns: `_` is wildcard, `...` is repeat, `set!`, `and`, `or`, `not`, `?`, `=` are guards | Relies on expansion — pattern variables become `let` bindings in the expanded code |
| **Position info** | References are attached directly to the original pattern AST nodes | References come from the expanded AST; `shallow-copy` maps them back, potentially losing granularity |
| **Performance** | No expansion cost; single pass over patterns | Must expand the macro, parse the expansion string back into index-nodes, then run `step` on it |
| **Maintenance** | One file per macro family | Zero extra code per `syntax-rules` macro |
| **Coverage** | Only macros with hand-written rules | Any macro defined via `syntax-rules` / `syntax-case` (if generator is attached) |

---

## 5. Known Defects & Limitations

### 5.1 Nested-Macro Rejection (`match-two` is blocked)

`match-two`'s template contains `let-syntax` with an inner `syntax-rules`:

```scheme
((match-two v x g+s (sk ...) fk (id ...))
 (match-check-identifier
  x
  (let-syntax                      ; ← nested macro definition!
      ((new-sym?
        (syntax-rules (id ...)
          ((new-sym? x sk2 fk2) sk2)
          ((new-sym? y sk2 fk2) fk2))))
    (new-sym? random-sym-to-match
              (let ((x v)) (sk ... (id ... x)))
              (if (equal? v x) (sk ... (id ...)) fk)))
  ...))
```

The conservative `private:template-has-nested-macro?` rejects any template
containing `define-syntax`. Templates with `let-syntax` / `letrec-syntax` are
now allowed (they work fine with the existing `let-syntax-process`), but
`define-syntax` in templates is still rejected.

**Result:** `match-two`, `match-check-identifier`, and `match-check-ellipsis`
all get no-op generators (return `#f`). When auto-resolve reaches
`match-one → match-two`, the chain breaks.

### 5.2 Cascade Depth Limit (memory cap)

A complete `(match '(1) [(s) s])` cascade needs 6 layers:

| Layer | Macro | Expansion contains |
|-------|-------|-------------------|
| 1 | `match` | `match-next` |
| 2 | `match-next` | `match-one` |
| 3 | `match-one` (for `(s)`) | `match-two` |
| 4 | `match-two` (for `(s)`) | `match-one` |
| 5 | `match-one` (for `s`) | `match-two` |
| 6 | `match-two` (for `s`) | `(let ((s w)) (begin s))` |

`expansion-generator->rule` caps memory at 10. More complex examples like
`(match '(1 2) [(a b) (+ a b)])` need 8+ layers.

### 5.3 `shallow-copy` Single-Layer Limit

`shallow-copy` only copies references **one level** back — from the current
expansion to the direct macro call node. For cascaded macros like `match`,
the deepest `let` binding (e.g. `s` in layer 6) is copied back to the layer-5
`match-two` node, not to the original `(match '(1) [(s) s])` call node.

```
Original: (match '(1) [(s) s])
    ↑ shallow-copy (layer 1)
    sees: let, v, match-next

    Layer 1: (let ((v '(1))) (match-next v ...))
                    ↑ shallow-copy (layer 2)
                    sees: failure, match-one

        Layer 2: (let ((failure ...)) (match-one v (s) ...))
                                ↑ shallow-copy (layer 3)
                                sees: match-two

            Layer 3: (match-two v (s) ...)
                                    ↑ shallow-copy (layer 4)
                                    sees: if, let, w

                ... (continues) ...

                        Layer 6: (let ((s w)) (begin s))
                                    ↑ s binding is here!
```

Each layer's `shallow-copy` only propagates to its direct parent macro call.
The original `match` call never sees the deepest `s` binding.

This is a **design limitation**, not a bug. For `match`-like cascaded macros,
`match-process` remains necessary because it can analyze pattern variables
**without expanding**.

### 5.4 Historical Bug Fixes

These bugs have been fixed in the current codebase. They are documented here
for reference.

#### Bug 1: `ellipse-*-form` loop did not skip pseudo-children

**Location:** `analysis/identifier/expanders/pattern.sls`

`make-pattern` inserts `...` itself as an `ellipse` child and an `equal?-datum`
child into `ellipse-*-form` nodes. The `loop` in `expand->index-node-compound-list`
treated these pseudo-children as real children, generating extra `'()` elements.

**Fix:** Skip `ellipse`-typed children in the `else` branch of `loop`.

#### Bug 2: `private:expansion+index-node->pairs` length mismatch

**Location:** `analysis/identifier/expanders/syntax-rules.sls`

When `compound-list` and `children` differed in length (e.g. due to ellipsis
expansion), `map` crashed with "lists differ in length".

**Fix:** Added `private:take` to truncate both lists to the shorter length
before `map`. The pairer still does not understand ellipsis semantics, but it
is now fault-tolerant.

#### Bug 3: Symbols did not generate pairs

**Location:** `analysis/identifier/expanders/syntax-rules.sls`

`private:expansion+index-node->pairs` returned `'()` for atom symbols, so
`shallow-copy` could not map expanded `let` bindings back to original pattern
variables.

**Fix:** Added a `(and (symbol? compound-list) (symbol? expression))` branch
that generates `((index-node . compound-list))`.

#### Bug 4: `private:recursive-collect` did not recurse into children

**Location:** `analysis/identifier/expanders/expansion-wrap.sls`

`private:recursive-collect` only checked the root node's exports, missing
exports from child nodes deeper in the expansion tree.

**Fix:** Added `(apply append (map (lambda (child) (private:recursive-collect child proc)) (index-node-children expansion-index-node)))`.

---

## 6. Performance Optimization

### 6.1 Signature-Based Clause Filtering

Implemented in `analysis/identifier/expanders/syntax-rules.sls`.

**Scheme A:** Pre-compute `clause-index-nodes`, `clause-vector`, and
`signatures-vector` once at initialization time, storing them as closure
variables in the generator lambda. This avoids re-computing
`(cddr (index-node-children input-index-node))` and the signature list on every
generator invocation.

**Scheme B:** Add lightweight signature-based clause filtering before `eval` +
`syntax-case` in `private:confirm-clause`.

A signature is derived from each clause's pattern: `(min-param-count . param-shapes)`.
`param-shape` is one of `symbol`/`null`/`pair`/`vector`/`other`.

The filter requires the input expression's param count to be `>= min-param-count`
and the leading params to match the fixed-prefix shapes (`symbol` matches
anything, `pair` matches lists/null, etc.).

This is a conservative filter: it never rejects a clause that `syntax-case`
might match, but it can skip many non-matching clauses before the expensive
`eval` call.

### 6.2 Benchmark Results

A full 12-layer auto-resolve cascade:

| | Baseline | Optimized (A+B) | Speedup |
|---|---|---|---|
| **Total** | ~32.6 ms | ~18.6 ms | **1.75x** |

The biggest win is Layer 11 (`match-two` with a symbol dispatch key): from
**13.57 ms → 1.92 ms** (7.07x). `match-two` has 28 clauses; the signature
filter rejects 27 immediately, leaving only 1 for `eval`.

---

## 7. Improvement Opportunities

### 7.1 Generalise the Auto-Resolve Fallback

Remove the `ufo-match` hard-coding and make `identifier-reference-syntax-expander`
the default mechanism for any macro that has it:

```scheme
(define (route&add ...)
  (let* (...)
    (cond
      ; ... srfi special cases ...

      ; Generic auto-resolution for syntax-rules / syntax-case macros
      [(identifier-reference-syntax-expander target-identifier)
       (add-rule-procedure rules
         `((,(expansion-generator->rule
               (identifier-reference-syntax-expander target-identifier)
               step file-linkage expanded+callee-list memory target-identifier))
           . ,target-identifier))]

      ; Legacy hand-written rules (kept for macros without generators)
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
       (add-rule-procedure rules `((,try-process) . ,target-identifier))]

      [else rules])))
```

Placing this as a fallback before `else` would auto-resolve any macro whose
generator is available, while preserving hand-written rules for special cases.

**Current blocker:** If enabled without filtering, `.akku/lib/` external library
macros would all get auto-resolve rules, causing severe `init-workspace`
performance degradation.

### 7.2 Cache Expansions

Many macro calls in a file use the same macro with identical or similar
arguments. The expansion generator could cache `(expression → expanded-index-node)`
pairs per document to avoid re-evaluating `syntax-case` repeatedly.

### 7.3 Distinguish Pattern-Variable from Helper Bindings

For pattern-matching macros specifically, `shallow-copy` could be enhanced to
understand that symbols matching `syntax-parameter` references in the template
should be mapped back to the original callee nodes, while other bindings
(introduced by the expansion itself, e.g. helper `let`s inside the macro)
should **not** be copied back. This would prevent hover on a `match` clause
from jumping to an internal helper variable.

---

## 8. Files Involved

| File | Role |
|------|------|
| `analysis/identifier/self-defined-rules/router.sls` | Decides whether to use hand-written rule or generic expansion |
| `analysis/identifier/expanders/expansion-wrap.sls` | Wraps an expansion generator as a `step` rule; implements `shallow-copy` |
| `analysis/identifier/expanders/syntax-rules.sls` | Builds expansion generators for `syntax-rules` macros |
| `analysis/identifier/expanders/pattern.sls` | Pattern parsing, binding generation, and compound-list expansion |
| `analysis/identifier/rules/define-syntax.sls` | Attaches generators to identifier-references at definition time |
| `analysis/abstract-interpreter.sls` | Core `step` function; orchestrates rules and expansion |
| `analysis/identifier/macro-expander.sls` | Utilities for step-by-step expansion and callee/expanded pairing |
| `analysis/identifier/self-defined-rules/ufo-match/match.sls` | Hand-written rule for `match` and auxiliary macros |
