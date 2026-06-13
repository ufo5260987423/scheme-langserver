# Identifier System

## How dose scheme-langserver catch identifier bindings?

Procedures and variable bindings are the fundamental building blocks of Scheme programs. In fact, most part of functionalities including auto-completion, goto definition, document symbol etc., all dependent on them. In this document, I'll describe what scheme-langserver do in analysis/identifier/rules directory. Following forms are from [the summary from csug 9.5](https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0). These form should be caught by define-record-type.sls, lambda.sls, let.sls and so one. 

### Abstract Interpreter
Scheme-langserver's whole identifier catching mechanism is basically fully framed with [abstract interpreter](https://en.wikipedia.org/wiki/Abstract_interpretation). The most essential code located in [this file](../../analysis/abstract-interpreter.sls) now has initially exhibited my design in version 1.2.0. Its main purpose is to allow self-defined macros to introduce new library or identifiers in code, which is rare in main stream programming languages and deeply attract most scheme language programmers. 

An example is from the macro in [try.sls](../../analysis/identifier/self-defined-rules/ufo-try/try.sls). It allows catching exceptions like in Java without any introduction of keywords:
```scheme
(try
    ...
    (except current-exception 
        [condition do]
        ...
        [else ...]))
```

As you may see, `current-exception` is claimed as an identifier, and is bound with any exception. This identifier maybe used in rest body of `except`, and we all suppose scheme-langserver can help. And this feature will be implemented soon.

### Identifier binding forms in r6rs standard 
In practice, these forms would produce [identifier-reference](../../analysis/identifier/reference.sls) and attach them to [index-nodes](../../virtual-file-system/index-node.sls). Specifically, one form would produce one unique identifier and attach it to `index-node-export-to-other-node`, `index-node-import-in-this-node`, and `index-node-excluded-references`. 

| Head                    | Form                                                               |
|-------------------------|--------------------------------------------------------------------|
| case-lambda             | (case-lambda clause ...)                                           |
| define                  | (define var expr)                                                  |
| define                  | (define var)                                                       |
| define                  | (define (var0 var1 ...) body1 body2 ...)                           |
| define                  | (define (var0 . varr) body1 body2 ...)                             |
| define                  | (define (var0 var1 var2 ... . varr) body1 body2 ...)               |
| define-condition-type   | (define-condition-type name parent constructor pred field ...)     |
| define-enumeration      | (define-enumeration name (symbol ...) constructor)                 |
| define-ftype            | (define-ftype ftype-name ftype)                                    |
| define-ftype            | (define-ftype (ftype-name ftype) ...)                              |
| define-property         | (define-property id key expr)                                      |
| define-record           | (define-record name (fld1 ...) ((fld2 init) ...) (opt ...))        |
| define-record           | (define-record name parent (fld1 ...) ((fld2 init) ...) (opt ...)) |
| define-record-type      | (define-record-type record-name clause ...)                        |
| define-record-type      | (define-record-type (record-name constructor pred) clause ...)     |
| define-structure        | (define-structure (name id1 ...) ((id2 expr) ...))                 |
| define-syntax           | (define-syntax keyword expr)                                       |
| define-top-level-syntax | (define-top-level-syntax symbol obj)                               |
| define-top-level-syntax | (define-top-level-syntax symbol obj env)                           |
| define-top-level-value  | (define-top-level-value symbol obj)                                |
| define-top-level-value  | (define-top-level-value symbol obj env)                            |
| top-level-value-set!    | (top-leve-value-set! symbol obj)                                   |
| top-level-value-set!    | (top-leve-value-set! symbol obj env)                               |
| top-level-syntax-set!   | (top-leve-syntax-set! symbol obj)                                  |
| top-level-syntax-set!   | (top-leve-syntax-set! symbol obj env)                              |
| set!                    | (set! symbol obj env)                                              |
| fluid-let               | (fluid-let ((var expr) ...) body1 body2 ...)                       |
| fluid-let-syntax        | (fluid-let-syntax ((keyword expr) ...) form1 form2 ...)            |
| identifier-syntax       | (identifier-syntax tmpl)                                           |
| identifier-syntax       | (identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))              |
| lambda                  | (lambda formals body1 body2 ...)                                   |
| let                     | (let ((var expr) ...) body1 body2 ...)                             |
| let                     | (let name ((var expr) ...) body1 body2 ...)                        |
| let*                    | (let* ((var expr) ...) body1 body2 ...)                            |
| let*-values             | (let*-values ((formals expr) ...) body1 body2 ...)                 |
| let-syntax              | (let-syntax ((keyword expr) ...) form1 form2 ...)                  |
| let-values              | (let-values ((formals expr) ...) body1 body2 ...)                  |
| letrec                  | (letrec ((var expr) ...) body1 body2 ...)                          |
| letrec*                 | (letrec* ((var expr) ...) body1 body2 ...)                         |
| letrec-syntax           | (letrec-syntax ((keyword expr) ...) form1 form2 ...)               |
| syntax                  | (syntax template)                                                  |
| syntax-case             | (syntax-case expr (literal ...) clause ...)                        |
| syntax-rules            | (syntax-rules (literal ...) clause ...)                            |
| with-syntax             | (with-syntax ((pattern expr) ...) body1 body2 ...)                 |

NOTE: 
1. `define-top-level-syntax`, `define-top-level-value`,`top-level-value-set!`,`top-level-syntax-set!`,`set!` bind top-level identifiers within a sequential process. And I just claim their availability within document scope.
2. I don't implement `environment` mechanism, because it's actually a dynamic scope.

### Identifier binding export/import/load in r6rs standard 
Based on library framework, `export` and `import` would transfer identifier-references across libraries files. Specially, `load` will bind identifiers dynamically, I just try my best to analysis corresponding static code and roughly attach references to caller files.

| Head                    | Form                                 |
|-------------------------|--------------------------------------|
| export                  | (export export-spec ...)             |
| implicit-exports        | (implicit-exports #t)                |
| implicit-exports        | (implicit-exports #f)                |
| import                  | (import import-spec ...)             |
| import-only             | (import-only import-spec ...)        |
| indirect-export         | (indirect-export id indirect-id ...) |
| load                    | (load path)                          |
| load                    | (load path eval-proc)                |
| load-compiled-from-port | (load-compiled-from-port input-port) |
| load-library            | (load-library path)                  |
| load-library            | (load-library path eval-proc)        |
| load-program            | (load-program path)                  |
| load-program            | (load-program path eval-proc)        |
| load-shared-object      | (load-shared-object path)            |

### Identifier binding for self-made macro

As like [ufo-try](https://github.com/ufo5260987423/ufo-try), they also produce identifiers as expanding macros. An example may help understanding. In following code:
```lisp
;;a self-defined try-except macro here is used to handle possible exceptions
(try 
  ;;some works
  todo 
  ;;to catch exception c
  (except c 
  ;; a branch to handle
    [else c]))
```

Many programmers want to know the `c` in handling branch is relating to the `c` exception after `except`. It's called "goto-definition" in LSP(Language Server Protocol). However, nested macros usually cause undecidable(if I use this word correctly) time-consuming, because "goto-definition" requiring an abstract interpreter evaluates all expansions and backwarding identifier claiments from expansion to macro callee, which interleaves macro expanding.

An alternative solution is to directly write rules handling requests, as you may see [here](../../analysis/identifier/self-defined-rules/ufo-try). It's registered in [this file](../../analysis/identifier/self-defined-rules/router.sls).

### Why does identifier catching interleave index fully updating?
This question is somehow asked maybe they think would speed up auto-complete for long scheme code programming. Their basic idea is that whether the old caught identifiers can be smoothly transformed from old indexed code to newly updated indexed code. Apparently, copy and paste seems to cost less than `match` macro. However, it faced a serious problem that any seemed-locally updating may globally affect whole syntax tree, because code editing is orthogonal with index-nodes tree, and practically, is orthogonal with identifier catching. 

In relation with this, all index node in old code, before it transformed its caught identifiers to new code, two criterions must be followed:
1. Its ancestors must prove similar context with old code, because identifiers imported by ancestors may affect the catching mechanism.
2. Its sibling and children must be consisted with old identifiers. Because some identifier definition macros (like `define`) beyond themselves but fully overlap within their common parent node. And children may affect identifiers' detail.

To be a simple conclusion, these criterions are not more efficient than fully updated. But maybe more work can be done.

---

## Macro Auto-Resolution

## Macro Auto-Resolution via `identifier-reference-syntax-expander`

### 1. Overview & Architecture

#### 1.1 Background & Motivation

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

#### 1.2 Two Phases of Macro Life-Cycle

| Phase | Where | What happens |
|-------|-------|--------------|
| **Definition** | `abstract-interpreter.sls:step` | `define-syntax` is detected → `define-syntax:attach-generator` stores an expansion generator on the identifier-reference |
| **Call site** | `router.sls:route&add` | When the macro is invoked, the stored generator is retrieved and wrapped by `expansion-generator->rule` |

#### 1.3 How the Expansion Generator Is Created

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

#### 1.4 How the Expansion Generator Is Consumed

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

#### 1.5 What `expansion-generator->rule` Does at Call Time

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

### 2. Complete Call Path

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

### 3. Match Macro Expansion in Detail

#### 3.1 Match Syntax-Rules Definition

```scheme
(define-syntax match
  (syntax-rules ()
    ((match atom (pat . body) ...)
     (let ((v atom))
       (match-next v (atom (set! atom)) (pat . body) ...)))
    ...))
```

#### 3.2 Chez Scheme Expansion Walkthrough

`match` is a pattern-matching macro that expands declarative `pat → body`
syntax into procedural `if`/`let` nesting, turning pattern variables into
`let` bindings.

##### Example 1: `(match '(1) [(s) s])`

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

##### Example 2: `(match '(1 2) [(a b) (+ a b)])`

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

#### 3.3 Auto-Resolver Substitution Walkthrough

This section traces the auto-resolver's treatment of the same `match` call.

**Call site AST:**
```scheme
(match expression
  [(_ (? string? path)) (let (...) ...)]
  [else '()])
```

##### Step 1 — `syntax-rules->generator:map+expansion`

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

##### Step 2 — `source-file->annotations` → `expansion-index-node`

The expansion expression is pretty-printed and re-parsed into a fresh AST:
```scheme
(let ((v expression))
  (match-next v
    (expression (set! expression))
    ((_ (? string? path)) (let (...) ...))
    (else (quote ()))))
```

##### Step 3 — `private:expansion+index-node->pairs`

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

##### Step 4 — `step` on expansion AST

`step` recursively visits the expanded tree:
- Processes `let`, establishes `v` binding
- Processes `match-next`, discovers it is a macro call, attempts expansion
- `match-next` expansion may produce new `match` calls, but `memory` contains
  the original call, so `step` skips

##### Step 5 — `private:shallow-copy`

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

### 4. Comparison: Auto-Resolve vs Hand-Written Rule

| Aspect | `match-process` (hand-written) | `expansion-generator->rule` (generic) |
|--------|-------------------------------|---------------------------------------|
| **Binding semantics** | Explicitly knows `match` patterns: `_` is wildcard, `...` is repeat, `set!`, `and`, `or`, `not`, `?`, `=` are guards | Relies on expansion — pattern variables become `let` bindings in the expanded code |
| **Position info** | References are attached directly to the original pattern AST nodes | References come from the expanded AST; `shallow-copy` maps them back, potentially losing granularity |
| **Performance** | No expansion cost; single pass over patterns | Must expand the macro, parse the expansion string back into index-nodes, then run `step` on it |
| **Maintenance** | One file per macro family | Zero extra code per `syntax-rules` macro |
| **Coverage** | Only macros with hand-written rules | Any macro defined via `syntax-rules` / `syntax-case` (if generator is attached) |

---

### 5. Known Defects & Limitations

#### 5.1 Nested-Macro Rejection (`match-two` is blocked)

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

#### 5.2 Cascade Depth Limit (memory cap)

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

#### 5.3 `shallow-copy` Single-Layer Limit

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

#### 5.4 Historical Bug Fixes

These bugs have been fixed in the current codebase. They are documented here
for reference.

##### Bug 1: `ellipse-*-form` loop did not skip pseudo-children

**Location:** `analysis/identifier/expanders/pattern.sls`

`make-pattern` inserts `...` itself as an `ellipse` child and an `equal?-datum`
child into `ellipse-*-form` nodes. The `loop` in `expand->index-node-compound-list`
treated these pseudo-children as real children, generating extra `'()` elements.

**Fix:** Skip `ellipse`-typed children in the `else` branch of `loop`.

##### Bug 2: `private:expansion+index-node->pairs` length mismatch

**Location:** `analysis/identifier/expanders/syntax-rules.sls`

When `compound-list` and `children` differed in length (e.g. due to ellipsis
expansion), `map` crashed with "lists differ in length".

**Fix:** Added `private:take` to truncate both lists to the shorter length
before `map`. The pairer still does not understand ellipsis semantics, but it
is now fault-tolerant.

##### Bug 3: Symbols did not generate pairs

**Location:** `analysis/identifier/expanders/syntax-rules.sls`

`private:expansion+index-node->pairs` returned `'()` for atom symbols, so
`shallow-copy` could not map expanded `let` bindings back to original pattern
variables.

**Fix:** Added a `(and (symbol? compound-list) (symbol? expression))` branch
that generates `((index-node . compound-list))`.

##### Bug 4: `private:recursive-collect` did not recurse into children

**Location:** `analysis/identifier/expanders/expansion-wrap.sls`

`private:recursive-collect` only checked the root node's exports, missing
exports from child nodes deeper in the expansion tree.

**Fix:** Added `(apply append (map (lambda (child) (private:recursive-collect child proc)) (index-node-children expansion-index-node)))`.

---

### 6. Performance Optimization

#### 6.1 Signature-Based Clause Filtering

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

#### 6.2 Benchmark Results

A full 12-layer auto-resolve cascade:

| | Baseline | Optimized (A+B) | Speedup |
|---|---|---|---|
| **Total** | ~32.6 ms | ~18.6 ms | **1.75x** |

The biggest win is Layer 11 (`match-two` with a symbol dispatch key): from
**13.57 ms → 1.92 ms** (7.07x). `match-two` has 28 clauses; the signature
filter rejects 27 immediately, leaving only 1 for `eval`.

---

### 7. Improvement Opportunities

#### 7.1 Generalise the Auto-Resolve Fallback

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

#### 7.2 Cache Expansions

Many macro calls in a file use the same macro with identical or similar
arguments. The expansion generator could cache `(expression → expanded-index-node)`
pairs per document to avoid re-evaluating `syntax-case` repeatedly.

#### 7.3 Distinguish Pattern-Variable from Helper Bindings

For pattern-matching macros specifically, `shallow-copy` could be enhanced to
understand that symbols matching `syntax-parameter` references in the template
should be mapped back to the original callee nodes, while other bindings
(introduced by the expansion itself, e.g. helper `let`s inside the macro)
should **not** be copied back. This would prevent hover on a `match` clause
from jumping to an internal helper variable.

---

### 8. Files Involved

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

---

## Macro Expansion Performance

## Task: Auto Macro Expansion Performance Optimization

### Background

`scheme-langserver` uses `expansion-generator->rule` in `analysis/identifier/expanders/expansion-wrap.sls` to auto-resolve macro expansion references. The core cascade (`match` -> `match-next` -> `match-one` -> `match-two` -> ...) is now functional after previous fixes to:

- `abstract-interpreter.sls`: expander-doc fallback for identifier resolution inside expansion trees
- `expansion-wrap.sls`: enhanced shallow-copy with reverse-map sync and all-pairs fallback
- `pattern.sls`: pair-form rest-variable binding and proper list splicing
- `syntax-rules.sls`: min-len alignment and symbol fallback for extra children

However, the full test suite (`bash test.sh`) is extremely slow because `.akku/libobj/scheme-langserver` compiled cache is missing and the auto-macro code itself has algorithmic hotspots.

### Performance Bottleneck Analysis

#### 1. `private:find-expander-doc-for-node` — repeated parent-chain traversal (BIGGEST)

**Location**: `analysis/abstract-interpreter.sls:270-278`

`step` processes every AST node. Each call to `find-available-references-for` that fails in `current-document` falls back to `private:find-expander-doc-for-node`, which walks up the `index-node-parent` chain until it finds an `expanded+callee-list` entry. For deep ASTs this is O(depth) per node, invoked thousands of times.

**Fix**: cache expander-doc directly on index-nodes (or in a hashtable keyed by node) so lookup is O(1).

#### 2. `build-reverse-map` + `assoc` — O(n) lookup in alist

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:74-77, 79-101`

`private:shallow-copy` builds `reverse-map` as an alist. `private:sync-to-parent-expansion` uses `(assoc target-node reverse-map)` which scans the entire list. When cascade expansion produces hundreds of pairs, this becomes expensive.

**Fix**: use `eq-hashtable` instead of alist; lookup drops from O(n) to O(1).

#### 3. `apply append` recursion explosion

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:43-64`

`private:recursive-collect`, `private:recursive-filter`, and `private:find-nodes-by-symbol` all use `apply append` to merge child results. Each recursive call constructs intermediate lists, creating GC pressure on large expansion trees.

**Fix**: rewrite as tail-recursive accumulator + `reverse`.

#### 4. `syntax-rules.sls` — repeated `length` + nested `append`

**Location**: `analysis/identifier/expanders/syntax-rules.sls:163-211`

`private:expansion+index-node->pairs` calls `(length compound-children)` and `(length children)` multiple times. `length` is O(n) on lists. It also nests `apply append` inside `map`.

**Fix**: cache lengths in `let*`; collect pairs with tail-recursive accumulator.

#### 5. `extract-all-pairs` — full scan of `expanded+callee-list`

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:71-72`

Every `private:shallow-copy` call scans the entire `expanded+callee-list` to extract all pairs. The list grows linearly with cascade depth.

**Fix**: incrementally maintain `all-pairs` inside `expanded+callee-list` entries.

### Execution Plan

- [x] **OPT-1** Cache expander-doc to eliminate parent-chain traversal
  - `analysis/abstract-interpreter.sls`: `private:find-expander-doc-for-node` now uses an `eq-hashtable` keyed by `node` to cache (expanded+callee-list, result) pairs. Repeated lookups for the same node within the same expansion context avoid traversing the parent chain.
- [x] **OPT-2** Convert `build-reverse-map` to `eq-hashtable`
  - `analysis/identifier/expanders/expansion-wrap.sls`: `build-reverse-map` now returns an `eq-hashtable` instead of an alist. `private:sync-to-parent-expansion` uses `hashtable-ref` instead of `assoc`, dropping lookup from O(n) to O(1).
- [x] **OPT-3** Replace `apply append` recursion with tail-recursive accumulators
  - `analysis/identifier/expanders/expansion-wrap.sls`: `private:recursive-collect`, `private:find-nodes-by-symbol`, and `private:recursive-filter` rewritten to use explicit stack + accumulator, eliminating `apply append` and intermediate list construction.
- [x] **OPT-4** Cache `length` results and use accumulator in `syntax-rules.sls`
  - `analysis/identifier/expanders/syntax-rules.sls`: `private:expansion+index-node->pairs` now caches `(length compound-children)` and `(length children)` in `let*`; inner `map` + `apply append` replaced with tail-recursive accumulator loop. Added helper `private:expansion+index-node->pairs-rev` for accumulator-passing style.
- [x] **OPT-5** Incrementally maintain `all-pairs` in `expanded+callee-list`
  - `analysis/identifier/expanders/expansion-wrap.sls`: Changed entry format from 4-element to 5-element, appending cumulative `all-pairs`. `extract-all-pairs` now reads `(list-ref (car expanded+callee-list) 4)` instead of scanning the entire list, dropping from O(n) to O(1). `expansion-generator->rule` computes `new-all-pairs` incrementally via `(append pairs previous-all-pairs)` when constructing each new entry.

---

### Files Involved

- `analysis/abstract-interpreter.sls`
- `analysis/identifier/expanders/expansion-wrap.sls`
- `analysis/identifier/expanders/syntax-rules.sls`
- `analysis/identifier/expanders/pattern.sls`

---

## Memory Investigation: Auto Macro Expansion

### Background

While running the auto-macro test suite, multiple `scheme --script` processes each consume **800 MB+ RAM**, forcing manual kills. The issue is not limited to parallel test runs; even single-process `performance.sps` (full-project init) stays resident for 10+ minutes with high memory usage.

### Memory Bottleneck Analysis

#### MEM-1: `append-references-into-ordered-references-for` — O(n² log n) reference maintenance

**Location**: `analysis/identifier/reference.sls:182`

Every call performs:
1. `(append old-list new-items)` — copies the **entire** existing list
2. `sort-identifier-references` — O(n log n) sort over the merged list
3. `ordered-dedupe` — O(n) recursive dedupe (no hashtable optimization despite the module comment)

Called **83 times** across the project. Inside `private:shallow-copy` (`expansion-wrap.sls:197`) it updates `document-ordered-reference-list` and `index-node-references-import-in-this-node`.

As the document accumulates thousands of references, each insertion re-allocates and sorts the whole list. This is the single largest source of GC pressure and intermediate list allocation.

**Fix**: defer sorting/deduping until read time (dirty-flag pattern), or replace `append` with `cons` and only sort on first query.

#### MEM-2: `index-node-references-export-to-other-node` built with `append`

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:194` and 20+ other files

Pattern everywhere:
```scheme
(index-node-references-export-to-other-node-set!
  node
  (append (index-node-references-export-to-other-node node) `(,ni)))
```

Each insertion copies the whole export list. In `private:shallow-copy` this is compounded by:
- `find` linear scan for duplicates (line 190) before appending
- `private:sync-to-parent-expansion` (line 135) doing the same `append` again
- cascade depth adds the same logical reference multiple times to the same node

**Fix**: use `cons` instead of `append`; reverse only when the list is read. Or maintain an `eq-hashtable` of already-added identifiers per node.

#### MEM-3: `private:expander-doc-cache-ht` — global cache never cleared

**Location**: `analysis/abstract-interpreter.sls:270`

```scheme
(define private:expander-doc-cache-ht (make-eq-hashtable))
```

Only `hashtable-set!`, never cleared. During full-project analysis `step` visits thousands of nodes; every miss is cached. After a large workspace init the hashtable may hold tens of thousands of `(node . (expanded+callee-list . result))` entries.

**Fix**: clear the hashtable at the start of `init-references` / `private-init-references`.

#### MEM-4: `make-identifier-reference` creates duplicate objects

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:179` and `:123`

`private:shallow-copy` and `private:sync-to-parent-expansion` create brand-new `identifier-reference` records (10 fields each) for every export/import pair. The same logical binding may be re-created dozens of times during cascade expansion.

**Fix**: intern / reuse identical references via a weak eq-hashtable keyed by `(identifier document index-node init-node library-id type top-env)`.

#### MEM-5: `build-reverse-map` temporary hashtable per `shallow-copy`

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:108`

Every `private:shallow-copy` call allocates a fresh `eq-hashtable`, fills it from `all-pairs`, then discards it. With hundreds of pairs and deep cascades this creates many short-lived hashtables.

**Fix**: since OPT-5 already incrementally maintains `all-pairs`, consider also incrementally maintaining `reverse-map` inside `expanded+callee-list` entries (same 6th-slot technique), turning per-call allocation into per-entry update.

#### MEM-6: `clear-references-for` defined but never called

**Location**: `virtual-file-system/index-node.sls:198`

```scheme
(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (for-each clear-references-for (index-node-children index-node)))
```

`private-init-references` (`analysis/workspace.sls:144`) never clears old references before re-running `step`. On incremental refresh (`refresh-workspace-for`), new references are appended on top of stale ones, causing a memory leak.

**Fix**: call `(clear-references-for (car (document-index-node-list document)))` at the top of `private-init-references`.

### Execution Plan

- [ ] **MEM-1** Defer `sort`+`dedupe` in `append-references-into-ordered-references-for`
- [ ] **MEM-2** Replace `append` with `cons` (or eq-hashtable guard) for export lists
- [ ] **MEM-3** Clear `private:expander-doc-cache-ht` on each `init-references`
- [ ] **MEM-4** Intern / deduplicate `make-identifier-reference` in shallow-copy
- [ ] **MEM-5** Incrementally maintain `reverse-map` (optional, lower priority)
- [ ] **MEM-6** Call `clear-references-for` at start of `private-init-references`

---

## Appendix: Chez Scheme Memory Profiling Investigation Plan

### Available Tools

#### 1. Runtime statistics (`statistics` / `sstats`)

```scheme
(import (chezscheme))
(define s (statistics))
(sstats-bytes s)      ; cumulative bytes allocated
(sstats-gc-count s)   ; number of GCs
(sstats-gc-cpu s)     ; GC CPU time
(sstats-gc-real s)    ; GC real time
(bytes-allocated)     ; shorthand for sstats-bytes
(collect)             ; force full GC
(display-statistics)  ; pretty-print summary
```

**Usage**: wrap a thunk with before/after `(statistics)` snapshots to compute delta bytes / delta GC time.

#### 2. Object-type census (`object-counts`)

```scheme
(object-counts)
;; => ((flvector (static 1 . 16))
;;     (pair (static 112336 . 1797376))
;;     (vector (static 23937 . 1465568))
;;     ...)
```

Returns per-generation counts and byte sizes for each primitive object type (`pair`, `vector`, `string`, `symbol`, `closure`, `hashtable`, etc.).

**Limitation**: cannot distinguish user record types (e.g. `identifier-reference` vs generic `vector`).

#### 3. Guardian-based object tracking (`make-guardian`)

```scheme
(define g (make-guardian))
(g (make-identifier-reference ...))
(collect)
(g)  ;; => #f if reclaimed, or the object if still alive
```

Weak-reference guardian. Useful for observing whether short-lived objects are actually being reclaimed between GCs.

#### 4. Process-level measurement (GNU `time -v`)

Already used in testing:
```bash
/usr/bin/time -v scheme --script test.sps
## => Maximum resident set size (kbytes): 446580
```

**Limitation**: only gives final peak RSS, not per-function attribution.

### Missing Tools (Chez Scheme does not provide)

| Tool | Status | Impact |
|------|--------|--------|
| Heap dump (`dump-memory`) | ❌ not bound | Cannot inspect individual object graphs |
| Per-record-type counts | ❌ `object-counts` only sees primitives | Cannot count `identifier-reference` instances directly |
| Line-level allocation profiler | ❌ not available | Cannot attribute bytes to specific source lines |
| Heap snapshot diff | ❌ not available | Must manually sample before/after |

### Investigation Plan

Given the tool constraints, the practical approach is **delta sampling** at key boundaries.

#### Phase 1: Function-level allocation attribution

**Goal**: pinpoint which function is responsible for the most byte allocation during cascade expansion.

**Method**:
1. Add a helper `with-memory-sampling` macro in `bin/memory-investigation.ss`:
   ```scheme
   (define-syntax with-memory-sampling
     (syntax-rules ()
       [(_ label body ...)
        (let ([s0 (statistics)]
              [t0 (current-time)])
          (let ([result (begin body ...)])
            (let ([s1 (statistics)])
              (printf "[~a] alloc=~a gc-cpu=~a ms\n"
                label
                (- (sstats-bytes s1) (sstats-bytes s0))
                (gc-time-diff s1 s0)))
            result))]))
   ```
2. Instrument the four hotspots inside `expansion-wrap.sls`:
   - `private:shallow-copy` (total)
   - `private:sync-to-parent-expansion`
   - `build-reverse-map`
   - the inner `append-references-into-ordered-references-for` loop
3. Run `test-match-cascade-auto-resolve.sps` under this instrumented build.
4. Output: a ranked list of which function allocates the most bytes per invocation.

#### Phase 2: Object-type census snapshots

**Goal**: determine whether the explosion is dominated by `pair` (list copying) or `vector` (hashtables / records).

**Method**:
1. Force GC (`collect`) before and after the critical section.
2. Capture `(object-counts)` at three checkpoints:
   - After `init-workspace`
   - Before first cascade `shallow-copy`
   - After last cascade `shallow-copy`
3. Compute delta for:
   - `pair` (lists)
   - `vector` (hashtables, record internals)
   - `string` (symbol→string conversions in `identifier-compare?`)
4. Output: which primitive type grows the most during expansion.

#### Phase 3: Guardian-based object survival check

**Goal**: verify whether the 1.45GB peak is due to *retained* objects or just *transient* allocations that GC hasn't reclaimed yet.

**Method**:
1. In `reference.sls`, wrap `make-identifier-reference` to register each new instance in a global guardian:
   ```scheme
   (define identifier-reference-guardian (make-guardian))
   (define (track-identifier-reference ref)
     (identifier-reference-guardian ref)
     ref)
   ```
2. After each cascade level, force `(collect)` and drain the guardian:
   ```scheme
   (let count-reclaimed ([n 0])
     (if (identifier-reference-guardian)
         (count-reclaimed (+ n 1))
         n))
   ```
3. Output: how many `identifier-reference` objects survive vs. how many are reclaimed. If most survive, the leak is structural (retained in index-node lists). If most are reclaimed but peak is still high, the problem is allocation churn (temporary lists during `append`/`sort`).

#### Phase 4: Process-level baseline with controlled memory cap

**Goal**: get a reproducible RSS peak for regression testing.

**Method**:
1. Use `ulimit -v` to set a virtual-memory ceiling (e.g. 2GB).
2. Run `test-match-cascade-auto-resolve.sps` with GNU `time -v`.
3. Record:
   - `Maximum resident set size (kbytes)`
   - `Minor (reclaiming a frame) page faults`
   - Elapsed wall-clock time
4. If the test completes within the cap, success. If it OOMs, the cap is the baseline threshold.

### Deliverable

A new file `bin/memory-investigation.ss` that:
1. Imports the above helpers.
2. Wraps `test-match-cascade-auto-resolve.sps` (or `performance.sps`) with Phase 1–3 instrumentation.
3. Prints a structured report (text or JSON) showing:
   - Per-function allocation deltas
   - Object-type census deltas
   - Guardian survival counts
   - Final RSS from GNU `time`

This script should be checked into the repo (not committed to `kimi` yet) so it can be reused for future MEM optimization verification.

---

## Macro Resolution Notes

## 目标
本次工作需要调试scheme-langserver的宏解析功能，以捕获对identifier的声明过程，便于通过LSP（Language Server Protocol）提供goto definition、find references、auto complete功能。你需要完整阅读本文件，然后使用后文所提到的脚本和调试方法，去完成调试。

### 待解析宏的案例
akku包管理器下的(ufo-try)库，try宏的调用可以采用下列方式：
```
(try
    do something here
    (except condition
        [branch to process condition here]
        other branches ...
    )
)
```
其中的c就是try宏声明的一个identifier，它将捕获程序抛出的异常。

### 原理
用户自定义宏经过scheme REPL展开、转换后，将形成完全以scheme primitive宏和 primitive过程（procedure）组织的代码。在这些primitive宏中，let\define等将能够声明identifier。

### scheme-langserver的思路
scheme-langserver希望在branch中提供对c的goto definition、find references、auto complete等服务。为了实现这一点，需要理清用户自定义宏的调用，和宏调用的展开之间的对应关系。
仍然以(ufo-try)，其定义是
```
(define-syntax try
  (lambda (x)
    (syntax-case x (except)
      [(try body0 body1 ... (except condition clause0 clause1 ...))
        #`((call/1cc
        (lambda (escape)
          (with-exception-handler
            (lambda (c)
          (let ([condition c])     ;; clauses may set! this
            #,(let loop ([first #'clause0] [rest #'(clause1 ...)])
                (if (null? rest)
                (syntax-case first (else =>)
                  [(else h0 h1 ...) #'(escape (lambda () h0 h1 ...))]
                  [(tst) #'(let ([t tst]) (if t (escape (lambda () t)) (raise c)))]
                  [(tst => l) #'(let ([t tst]) (if t (escape (lambda () (l t))) (raise c)))]
                  [(tst h0 h1 ...) #'(if tst (escape (lambda () h0 h1 ...)) (raise c))])
                (syntax-case first (=>)
                  [(tst) #`(let ([t tst]) (if t (escape (lambda () t)) #,(loop (car rest) (cdr rest))))]
                  [(tst => l) #`(let ([t tst]) (if t (escape (lambda () (l t))) #,(loop (car rest) (cdr rest))))]
                  [(tst h0 h1 ...) #`(if tst (escape (lambda () h0 h1 ...)) #,(loop (car rest) (cdr rest)))])))))
            (lambda ()
          ;; cater for multiple return values
          (call-with-values
              (lambda () body0 body1 ...)
            (lambda args
              (escape (lambda ()
                    (apply values args))))))))))])))
```
显然，其中的(let ([condition c]) ...)代码指出，对try的宏调用，展开后将通过let宏声明一个identifier名为condition，并且可以被调用。这就形成了展开后的condition和调用中的condition的对应关系，识别出来即可。

## 现状

### 已有功能
通过阅读README.md和doc/目录下的各个文件夹可知目前scheme-langserver已经实现了对primitive宏声明identifier的支持，
1. 核心机制是/analysis/abstract-interpreter.sls
2. 对于primitive宏的处理规则主要在/analysis/identifier/rules下面
3. 处理结果形成了索引，通过/virtual-file-system数据结构保存
4. LSP相关服务支持，在/protocol文件夹下面
5. 主文件是scheme-langserver.sls

### 开发状态
正在开发scheme-langserver的宏解析功能，包括
1.  /analysis/identifier/self-defined-rules/router.sls，负责由abstract-interpreter岔开程序执行通路，将用户自定义宏另例处理；
2.  router.sls将转发akku (ufo-match)中对match宏的调用供调试，也就是说我们调试的时候，目前只看match宏是否被正常处理；
3.  match宏的定义全部由syntax-rules实现，这个和try宏不一样，但是scheme-langserver处理的原理不会改变；
4.  /analysis/identifier/expanders/expansion-wrap.sls的expansion-generator->rule将处理match宏的调用，这边为什么是由这个过程处理，你可以看一下/analysis/identifier/rules/syntax-rules.sls和define-syntax.sls；
5.  /analysis/identifier/expanders包含了宏解析的主要功能。

### 难点
宏定义往往是嵌套的。仍然以ufo-match宏为例，它的定义是大量的syntax-rules宏互相调用和递归调用。scheme-langserver处理的方法是，对每一层嵌套：
1. 识别这层嵌套展开前后的代码对应关系；
2. 调用abstract-interpreter.sls识别展开后代码中的identifier声明；
3. expansion-wrap.sls中有个shallow-copy，基于对应关系将展开后的声明代入到展开前的宏调用代码。

这样，就形成了递归。

### 调试脚本

#### 调试环境的准备
bash .akku/env

#### 执行脚本
scheme --script ./tests/analysis/identifier/self-defined-rules/test-router.sps

#### 脚本输出和问题

当前脚本输出包含下面这段
```
enter-wrap0
(match expression
  [(_ (fuzzy0 **1) fuzzy1 ...)
   (fold-left
     (lambda (exclude-list identifier-parent-index-node)
       (let* ([identifier-index-node (car (index-node-children
                                            identifier-parent-index-node))]
              [target-identifier-reference (let-parameter-process index-node
                                             identifier-index-node
                                             index-node document type)]
              [extended-exclude-list (append
                                       exclude-list
                                       target-identifier-reference)])
         (index-node-excluded-references-set!
           (cadr (index-node-children index-node))
           extended-exclude-list)
         extended-exclude-list))
     '()
     (filter
       (lambda (i) (not (null? (index-node-children i))))
       (index-node-children
         (cadr (index-node-children index-node)))))]
  [else '()])
enter-wrap1
trigger
```
后面按道理应当出现step-into-next-level-step?，但是没有出现。问题是什么？

#### 调试方法
在代码中适当通过pretty-print、display等方法打印信息，通过观察程序输出，确定代码分支是否正常执行。

---

## 自动宏解析的验证与当前决策（2026-05-04）

### 新增测试文件

| 测试文件 | 目的 |
|---|---|
| `tests/analysis/identifier/test-match-expansion-compare.sps` | 比较 scheme-langserver 与 Chez Scheme 对 `match` 宏第一层展开的结果 |
| `tests/analysis/identifier/test-ufo-match-auxiliary-expansion.sps` | 验证 `match-next`、`match-one`、`match-two`、`match-drop-ids`、`match-gen-or-step` 等辅助宏的一层展开正确性 |

### 验证结论

#### 1. 第一层展开正确
`scheme-langserver` 的 `syntax-rules->generator:map+expansion`（通过 `confirm-clause` 验证）对 `ufo-match` 的**第一层**展开结果与 Chez Scheme `syntax-case` 的匹配结果完全一致。例如 `match` 宏对 `(match atom (pat . body) ...)` 展开为 `(let ((v atom)) (match-next v ...))`。

#### 2. 辅助宏展开同样正确
对 `ufo-match` 的 17 个代表性辅助宏调用场景进行测试，**全部通过**。测试覆盖：
- `match-next` 的 3 种 clauses（无 clauses、named failure、anonymous failure）
- `match-one` 的 2 种 clauses（ellipsis 检查、catch-all）
- `match-two` 的 8 种 clauses（空列表、`quote`、`and`、`or`、`not`、pair 模式等）
- `match-drop-ids`、`match-gen-or-step`

这说明 `syntax-rules->generator:map+expansion` **本身对各层辅助宏的展开逻辑是正确的**。

#### 3. 超时根因：级联展开无深度限制
`test-auto-macro-resolve.sps` 的超时问题**不是展开逻辑错误**，而是 `expansion-generator->rule`（位于 `analysis/identifier/expanders/expansion-wrap.sls`）在展开后**无限制地调用 `step`** 继续处理下一层宏调用：

```
match → match-next → match-one → match-two → match-check-ellipsis → match-extract-vars → ...
```

`ufo-match` 是一个**"宏家族"**：`match` 展开后产生 `match-next` 调用，`match-next` 展开后产生 `match-one` 调用，`match-one` 展开后产生 `match-two` 调用……每一层展开都会引入新的宏调用，形成深度数十层的级联。`expansion-generator->rule` 中的 `memory` 只防止**同一表达式**的重复展开，但无法阻止**不同表达式**的链式展开。

作为对比，`simple-let`（`test-auto-resolve-basic.sps` 中保留了原 `test-simple-macro-auto-resolve.sps` 的测试用例）展开后直接变成 primitive `let`，不再引入新宏调用，因此不会级联。

#### 4. 已知限制：`ellipse-pair-form` 处理
`syntax-rules->generator:map+expansion` 中的 `private:expansion+index-node->pairs` 对 `ellipse-pair-form`（如 `(p ...)` 展开为多个元素）存在长度不匹配问题。这会导致某些含 ellipsis 的宏调用无法正确建立展开前后的节点对应关系，进而影响 `shallow-copy` 的引用回传。

**已修复**：`private:expansion+index-node->pairs` 现已添加 `private:take` 截断逻辑，当 `compound-list` 与 `children` 长度不匹配时，截断较短的列表继续配对，避免 `map` 直接 crash。

### 当前决策

**暂时不启用 `router.sls` 中的自动宏解析路径**（即保持 `match-process` 等手写规则，不启用 `expansion-generator->rule` 的通用自动展开）。

原因：
1. 对 `ufo-match` 这种宏家族，自动展开会导致级联超时。
2. `ellipse-pair-form` 的节点对应关系尚未完善。

`router.sls` 中相关代码已注释保留（见 `analysis/identifier/self-defined-rules/router.sls:51-65`），以便未来有新办法时重新开发。

### 未来可能的开发方向

1. **展开深度限制**：在 `expansion-generator->rule` 中增加 `depth` 参数（如最大 2-3 层），超过后停止自动展开，回退到手写规则或保守处理。
2. **宏家族白名单/黑名单**：识别 `ufo-match` 这类宏家族，对它们直接跳过自动展开，始终使用手写规则。
3. **完善 `ellipse-pair-form`**：修复 `private:expansion+index-node->pairs` 对 ellipsis 展开的长度不匹配问题。
4. **按需展开**：只在需要解析 identifier 声明的特定宏调用处触发展开，而非对整个文件的所有宏调用都展开。

---

## Rules Refactoring with Macros

## 用宏重构 identifier rules 的方案

### 1. 问题陈述

`analysis/identifier/rules/` 目录下有 **30 个 `.sls` 文件**，每个文件实现一个特殊形式的解析规则（如 `let-process`、`define-process`、`lambda-process`）。这些文件以及 `abstract-interpreter.sls` 中的规则注册代码存在大量机械重复。

#### 1.1 规则文件层面的重复

每个规则文件都遵循完全相同的模板：

```scheme
(library (scheme-langserver analysis identifier rules <name>)
  (export <name>-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (<name>-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ ...) ...]
      [else '()]))))
```

重复内容包括：
- Library header 和 imports（仅名称不同）
- `let*` + `annotation-stripped` + `match expression` 骨架
- `make-identifier-reference` 的 8 个位置参数
- `index-node-references-export-to-other-node-set!` + `append` + `` `(,reference) `` 的引用附着模式
- `append-references-into-ordered-references-for` 的重复调用
- `[else '()]` fallback

#### 1.2 `abstract-interpreter.sls` 中的规则注册重复

`establish-available-rules-from` 中有 40+ 行 `cond` 分支，每行都是：

```scheme
[(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
[(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
[(equal? r '(let-values)) (private-add-rule rules `((,let-values-process) . ,identifier))]
...
```

这些行的结构完全一致，只有标识符名称和对应的 process 函数不同。

#### 1.3 为什么需要用宏

- **减少维护负担**：新增一个特殊形式（如 `when`、`unless`）时，只需写核心逻辑，不用复制 30 行 boilerplate
- **统一错误模式**：reference 的创建和附着目前有 4-5 种略有不同的写法，宏可以强制统一
- **降低认知负担**：新开发者只需理解规则的核心语义（"这个形式如何绑定变量"），不用被 boilerplate 分散注意力
- **规则注册可表驱动**：将 `cond` 分支转换为数据表，更易于扩展和审查

---

### 2. 方案设计

提供三个宏，分别解决三个层面的重复：

| 宏 | 解决什么问题 | 位置 |
|----|-----------|------|
| `define-identifier-rule` | 规则文件的 boilerplate | `analysis/identifier/rules/common-macros.sls` |
| `with-reference` | `make-identifier-reference` + attach 的重复 | 同上，作为辅助宏 |
| `register-rules` | `abstract-interpreter.sls` 中的 `cond` 分支 | `analysis/identifier/rules/common-macros.sls` |

---

#### 2.1 `define-identifier-rule`

##### 作用

定义一个标识符解析规则，自动生成 library header、imports 和 `let*` + `annotation-stripped` 骨架。

##### 语法

```scheme
(define-identifier-rule <process-name>
  [export <extra-export> ...]
  [import <extra-import> ...]
  <body>)
```

- `<process-name>`：如 `let-process`、`define-process`
- `export` 子句（可选）：额外导出的符号（如 `let-parameter-process`）
- `import` 子句（可选）：额外的导入（如 `(scheme-langserver analysis identifier util)`）
- `<body>`：规则的函数体，可以引用自动绑定的变量 `expression`

##### 展开示例

输入（改造后的 `lambda.sls`）：

```scheme
(define-identifier-rule lambda-process
  [export parameter-process]
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ...)]
    [else '()]))
```

展开为：

```scheme
(library (scheme-langserver analysis identifier rules lambda)
  (export lambda-process parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))
  (define (lambda-process root-file-node root-library-node document index-node)
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [(_ (identifier **1) fuzzy ... ) 
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            ...)]
        [else '()]))))
```

##### 实现要点

```scheme
(define-syntax define-identifier-rule
  (syntax-rules (export import)
    [(_ name [export extra ...] [import extra-imports ...] body)
     (let ([module-name (symbol->library-name 'name)])
       (library module-name
         (export name extra ...)
         (import 
           (chezscheme) (ufo-match)
           (scheme-langserver analysis identifier reference)
           (scheme-langserver virtual-file-system index-node)
           (scheme-langserver virtual-file-system library-node)
           (scheme-langserver virtual-file-system document)
           (scheme-langserver virtual-file-system file-node)
           extra-imports ...)
         (define (name root-file-node root-library-node document index-node)
           (let* ([ann (index-node-datum/annotations index-node)]
               [expression (annotation-stripped ann)])
             body))))
    [(_ name body)
     (define-identifier-rule name [export] [import] body)]))
```

---

#### 2.2 `with-reference`

##### 作用

封装 `make-identifier-reference` + `export-to-other-node-set!` + `append-references-into-ordered-references-for` 的重复模式。

##### 语法

```scheme
(with-reference (identifier document index-node initialization-index-node library-id type)
  [attach-to target-node]
  [import-into import-node]
  [exclude-from exclude-node]
  [return? #t])
```

##### 展开示例

输入：

```scheme
(with-reference (expression document index-node initialization-index-node '() 'variable)
  [attach-to (index-node-parent index-node)])
```

展开为：

```scheme
(let ([reference (make-identifier-reference
                   expression document index-node initialization-index-node
                   '() 'variable '() '())])
  (index-node-references-export-to-other-node-set!
    index-node
    (append (index-node-references-export-to-other-node index-node) `(,reference)))
  (append-references-into-ordered-references-for document (index-node-parent index-node) `(,reference))
  `(,reference))
```

##### 适用场景

改造前的 `let-parameter-process`（17 行）：

```scheme
(let ([reference 
        (make-identifier-reference
          expression document index-node initialization-index-node
          '() type '() '())])
  (index-node-references-export-to-other-node-set! 
    index-node
    (append (index-node-references-export-to-other-node index-node) `(,reference)))
  (append-references-into-ordered-references-for document let-node `(,reference))
  `(,reference))
```

改造后（5 行）：

```scheme
(with-reference (expression document index-node initialization-index-node '() type)
  [attach-to let-node])
```

---

#### 2.3 `register-rules`

##### 作用

将 `abstract-interpreter.sls` 中 `establish-available-rules-from` 的 `cond` 分支转换为声明式表。

##### 语法

```scheme
(register-rules rules identifier top
  (let let-process)
  (let* let*-process)
  (let-values let-values-process)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion)
  (begin do-nothing begin-process)
  ...)
```

规则：
- `(name process)` → 只有 pre-process：`((,process) . ,identifier)`
- `(name process post-process)` → pre + post：`((,process . ,post-process) . ,identifier)`

##### 展开示例

输入：

```scheme
(register-rules rules identifier top
  (let let-process)
  (let* let*-process)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion))
```

展开为：

```scheme
(cond 
  [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
  [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
  [(equal? r '(define-syntax)) (private-add-rule rules `((,define-syntax-process . ,define-syntax:attach-generator) . ,identifier))]
  [(equal? r '(syntax-rules)) (private-add-rule rules `((,syntax-rules-process . ,syntax-rules->generator:map+expansion) . ,identifier))]
  [else rules])
```

##### 环境约束的处理

对于需要检查 `top-environment` 的规则（如 `(define define-process r6rs)`），语法扩展为：

```scheme
(register-rules rules identifier top
  (define define-process r6rs)
  (define define-r7rs-process r7rs)
  ...)
```

展开为：

```scheme
(cond 
  [(and (equal? r '(define)) (private:top-env=? 'r6rs top))
    (private-add-rule rules `((,define-process) . ,identifier))]
  [(and (equal? r '(define)) (private:top-env=? 'r7rs top))
    (private-add-rule rules `((,define-r7rs-process) . ,identifier))]
  ...)
```

---

### 3. 改造前后对比

#### 3.1 `lambda.sls`（改造前 vs 改造后）

**改造前（121 行）**：Library header + imports（15 行）+ `lambda-process`（65 行）+ `parameter-process`（40 行），大量 `make-identifier-reference` + `index-node-references-export-to-other-node-set!` 重复。

**改造后（约 50 行）**：

```scheme
(define-identifier-rule lambda-process
  [export parameter-process]
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        (if (not (null? rest))
          (begin
            (parameter-process index-node (car rest) index-node '() document)
            (loop (cdr rest)))))]
    [(_ (? symbol? identifier) fuzzy ... ) 
      (parameter-process index-node (cadr (index-node-children index-node)) index-node '() document)]
    [(_ (identifier . rest) fuzzy ... ) 
      (with-reference (identifier document omg-index-node index-node '() 'parameter)
        [attach-to index-node])
      (let loop ([rest rest])
        ...)]
    [else '()]))

(define (parameter-process initialization-index-node index-node lambda-node exclude document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (with-reference (expression document index-node initialization-index-node '() 'parameter)
        [attach-to lambda-node]
        [exclude-from (index-node-parent index-node) exclude])
      '())))
```

#### 3.2 `abstract-interpreter.sls`（改造前 vs 改造后）

**改造前（lines 180-252，约 70 行 `cond`）**：

```scheme
(cond 
  [(and (equal? r '(define)) (private:top-env=? 'r6rs top))
    (private-add-rule rules `((,define-process) . ,identifier))]
  [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
  [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
  ...
  [else rules])
```

**改造后（约 15 行）**：

```scheme
(register-rules rules identifier top
  (define define-process r6rs)
  (define define-r7rs-process r7rs)
  (define define-s7-process s7)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (define-record-type define-record-type-process)
  (do do-process)
  (case-lambda case-lambda-process)
  (lambda lambda-process)
  (set! define-top-level-value-process)
  (let let-process)
  (let* let*-process)
  (let-values let-values-process)
  (let*-values let*-values-process)
  (let-syntax let-syntax-process)
  (letrec letrec-process)
  (letrec* letrec*-process)
  (letrec-syntax letrec-syntax-process)
  (syntax-case syntax-case-process r6rs)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion)
  (identifier-syntax identifier-syntax-process r6rs)
  (with-syntax with-syntax-process r6rs)
  (library library-import-process export-process)
  (import import-process)
  (begin do-nothing begin-process)
  (define-library library-import-process-r7rs export-process-r7rs))
```

---

### 4. 实施步骤

#### Phase 1：基础设施（1 个文件）

新建 `analysis/identifier/rules/common-macros.sls`：
- 定义 `define-identifier-rule`
- 定义 `with-reference`
- 定义 `register-rules`

#### Phase 2：试点改造（2-3 个文件）

选择最简单的规则文件进行试点：
1. `lambda.sls`（中等复杂度，有 `parameter-process` 辅助函数）
2. `let.sls`（稍复杂，有 `generate-naive-let-process-with` 工厂函数）
3. `define.sls`（最复杂，多种 pattern）

验证改造后的文件行为与改造前完全一致（运行相关测试）。

#### Phase 3：批量改造（剩余 27 个文件）

按以下优先级分批改造：
1. **简单绑定规则**：`let*`、`letrec`、`letrec*`、`let-values`、`let*-values`、`fluid-let`
2. **参数绑定规则**：`case-lambda`、`lambda*`、`define*`、`define-macro`
3. **语法规则**：`let-syntax`、`letrec-syntax`、`fluid-let-syntax`、`syntax-case`、`with-syntax`、`identifier-syntax`
4. **顶级定义**：`define`、`define-record-type`、`define-top-level-value`、`define-top-level-syntax`
5. **库相关**：`library-import`、`import`、`invoke-library`、`load`、`load-program`、`load-library`
6. **控制流**：`do`、`begin`

#### Phase 4：`abstract-interpreter.sls` 改造

将 `establish-available-rules-from` 中的 `cond` 替换为 `register-rules`。

#### Phase 5：清理和文档

- 删除每个规则文件顶部的 `; reference-identifier-type include` 注释块（宏已内置类型说明）
- 更新 AGENTS.md 中的编码规范
- 确保所有测试通过

---

### 5. 风险评估

#### 5.1 宏的调试难度

**风险**：如果 `define-identifier-rule` 展开后有 bug，错误信息会指向展开后的代码，而不是原始宏调用。

**缓解**：
- 在 `common-macros.sls` 中提供 `define-identifier-rule-debug` 变体，展开时打印生成的代码
- 在 `AGENTS.md` 中记录宏展开的检查方法：`scheme --script` + `(expand 'expr)`
- 保持宏足够简单（只做结构展开，不做复杂逻辑），降低出错概率

#### 5.2 对现有测试的影响

**风险**：改造后的文件必须行为完全一致，否则会影响 LSP 的语义分析结果。

**缓解**：
- 每改造一个文件，运行其对应的测试文件
- 改造前后做 diff（包括 `ordered-reference-list` 和 `export-to-other-node` 的内容）
- 优先改造测试覆盖率高的文件

#### 5.3 编译时开销

**风险**：宏在编译时展开，如果 `define-identifier-rule` 很复杂，可能增加编译时间。

**缓解**：
- 宏只做简单的语法转换（拼接 library header + 插入 body），不执行复杂计算
- 实测编译时间差异，如果超过 10% 则优化宏实现

#### 5.4 与 `ufo-match` 的兼容性

**风险**：`define-identifier-rule` 内部使用 `match`，而 body 中也可能使用 `match`，需要确保语法不冲突。

**缓解**：
- 使用 `syntax-rules` 或 `syntax-case` 实现 `define-identifier-rule`，不用 `ufo-match`
- body 中的 `match` 保持原样，由 Chez Scheme 的 reader 正常解析

---

### 6. 附录：完整改造后的 `lambda.sls` 示例

```scheme
(library (scheme-langserver analysis identifier rules lambda)
  (export lambda-process parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules common-macros)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define-identifier-rule lambda-process
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        (if (not (null? rest))
          (begin
            (parameter-process index-node (car rest) index-node '() document)
            (loop (cdr rest)))))]
    [(_ (? symbol? identifier) fuzzy ... ) 
      (parameter-process index-node (cadr (index-node-children index-node)) index-node '() document)]
    [(_ (identifier . rest) fuzzy ... ) 
      (let* ([omg-index-node (cadr (index-node-children index-node))])
        (with-reference (identifier document omg-index-node index-node '() 'parameter)
          [attach-to index-node])
        (let loop ([rest rest])
          (cond 
            [(pair? rest) 
              (with-reference ((car rest) document omg-index-node index-node '() 'parameter)
                [attach-to index-node])
              (loop (cdr rest))]
            [(not (null? rest)) 
              (with-reference (rest document omg-index-node index-node '() 'parameter)
                [attach-to index-node])]
            [else '()])))]
    [else '()]))

(define (parameter-process initialization-index-node index-node lambda-node exclude document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (with-reference (expression document index-node initialization-index-node '() 'parameter)
        [attach-to lambda-node]
        [exclude-from (index-node-parent index-node) exclude])
      '())))
)
```

对比原始 121 行，改造后约 50 行，核心语义逻辑完全保留，boilerplate 被宏吸收。
