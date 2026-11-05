# Type System & Inference

> This document replaces the older `type-inference.md` and `type-inference.cn.md`. It describes the complete type-inference pipeline implemented in `analysis/type/`.

## 1. Design Philosophy

Scheme is an untyped language. Unlike TypeScript or Typed Racket, it does not require programmers to annotate parameters or return values. Consequently, a traditional Hindley–Milner (HM) or System-F-based approach cannot be applied directly: there are no explicit type annotations to seed the inference process.

`scheme-langserver` takes a different path. It is built on the following assumptions and observations:

1. **Executable code is self-describing.** If a program runs correctly under R6RS, then every primitive call (`+`, `car`, `vector-ref`, …) already carries strong type constraints. For example, in `(lambda (a) (+ 1 a))` the parameter `a` must be a `number?` because `+` demands it.
2. **Gradual typing is sufficient for IDE diagnostics.** We do not need a sound, machine-verified proof. We need useful information for hover-tooltips, jump-to-definition, and basic error detection. If we occasionally over-approximate a type as `something?`, the user still gets a working language server.
3. **The AST itself can act as a type variable.** Instead of inventing a separate layer of type variables (`α`, `β`, …), every `index-node` in the virtual file system doubles as a placeholder for an unknown type. This makes the substitution list (the equation store) physically attached to the syntax tree, which simplifies debugging and caching.

The result is a two-phase pipeline:

| Phase | Module | What it does |
|-------|--------|--------------|
| **Substitution Generation** | `substitutions/generator.sls` + `substitutions/rules/*.sls` | Walks the AST and builds a set of equations (substitutions) that relate AST nodes to type expressions. |
| **Interpretation / Unification** | `domain-specific-language/interpreter.sls` | Evaluates the equations like a small interpreter, expanding macros, applying λ-abstractions, and resolving `index-node` placeholders until concrete types are reached. |

Both phases are expressed in a small **domain-specific language (DSL)** that lives inside the type subsystem.

> For the list-pattern matcher used when applying lambda types and macro heads, see §12.

---

## 2. The Type Expression DSL

Type expressions are ordinary Scheme S-expressions. There is no separate parser. The DSL is validated and manipulated by predicates in `domain-specific-language/inner-type-checker.sls`.

### 2.1 Atoms

| Form | Meaning |
|------|---------|
| `something?` | The top type (universal). Every value inhabits it. |
| `inner:void?` | The type of `(void)`. |
| `<-` | Function-arrow marker (used in infix position). |
| `...` | Kleene-star: the preceding element may appear **zero or more** times. |
| `**1` | Kleene-plus: the preceding element may appear **one or more** times. |

### 2.2 Composite Forms

| Form | Meaning |
|------|---------|
| `(inner:list? τ₁ τ₂ …)` | Proper list whose elements have types `τ₁`, `τ₂`, … |
| `(inner:vector? τ₁ τ₂ …)` | Vector whose elements have types `τ₁`, `τ₂`, … |
| `(inner:pair? τ-car τ-cdr)` | Pair (dotted or improper list). |
| `(τ-ret <- (inner:list? τ-arg1 τ-arg2 …))` | Function type: returns `τ-ret`, accepts arguments `τ-arg1` … |
| `(inner:record? pred (inner:pair? getter τ-field) …)` | *Recogniser exists but not yet generated as a type value.* Record types are currently represented indirectly via the `predicator` identifier and per-field getter/setter signatures. |

### 2.3 Identifier References as Types

Wherever a raw symbol such as `number?` appears, the system actually stores an `identifier-reference` record (see `analysis/identifier/reference.sls`). This preserves library scoping: `number?` from `(rnrs)` and a user-defined `number?` are distinct types. The helper `construct-type-expression-with-meta` performs this wrapping.

### 2.4 Index-Nodes as Placeholders

An `index-node` (an AST node from the VFS) may appear *inside* a type expression. This is how the system represents unknowns. During interpretation the interpreter looks up the `index-node-substitution-list` of that node and continues evaluating. In HM terminology, an `index-node` is both a **type variable** and the **term** it describes.

### 2.5 Example Type Expressions

```scheme
;; + under R6RS
(number? <- (inner:list? number? ...))

;; car, expressed as a macro (see §6)
((with ((a b c **1))
   (with-equal? inner:list? a b))
 (inner:list? fixnum? number?))

;; Record types are not yet emitted as unified values.
;; Instead, the predicate `point?` and accessors carry separate signatures:
;;   point?    : (boolean? <- (inner:list? something?))
;;   point-x   : (something? <- (inner:list? point?))
;;   make-point: (point? <- (inner:list? something? ...))
```

---

## 3. Phase I – Substitution Generation

The entry point is `construct-substitutions-for` in `substitutions/generator.sls`. It maps `step` over every top-level `index-node` in a `document`.

### 3.1 The `step` Dispatcher

`step` is a case-lambda with two arities:

1. **Normal mode** `(document index-node expanded+callee-list)` – used for the main AST.
2. **Quasi-quoted mode** `(document index-node available-identifiers quasi-quoted-syntaxed expanded+callee-list)` – used when descending inside `` ` `` or `#'` forms, so that `unquote`/`unsyntax` boundaries switch back to normal mode.

The dispatcher distinguishes five coarse shapes:

| Shape | Action |
|-------|--------|
| Leaf node | `trivial-process` (literals, symbols, self-evaluating values) |
| `quote` / `quasiquote` / `syntax` / `quasisyntax` | Special handling; `syntax` is skipped entirely because it is not a value. |
| Non-leaf list whose `car` is a symbol | Look up available identifier references, route to a rule, then recurse into children. |
| Non-leaf list whose `car` is *not* a symbol | Must be an application (e.g. `((lambda (x) x) 42)`). Run `application-process`, then recurse. |

### 3.2 Rule Routing

`establish-available-rules-from` takes the list of identifier references that are visible at the current `index-node` and returns a list of `(rule-procedure . identifier-reference)` pairs. It filters out `parameter` and `syntax-parameter` bindings (those are handled by the enclosing lambda/let rules) and then classifies each identifier by its `identifier-reference-type`:

| `identifier-reference-type` | Matched rule(s) |
|-----------------------------|-----------------|
| `procedure`, `variable`, `getter`, `setter`, `predicator`, `constructor` | `application-process` |
| `meta` + symbol `define` | `define-process` |
| `meta` + symbol `lambda` | `lambda-process` |
| `meta` + symbol `let` / `let*` / `letrec` / `letrec*` | `let-process` / `let*-process` (`let*.sls` covers `letrec`/`letrec*`) |
| `meta` + symbol `if` | `if-process` |
| `meta` + symbol `cond` | `cond-process` |
| `meta` + symbol `case-lambda` | `case-lambda-process` |
| `meta` + symbol `begin` | `begin-process` |
| `meta` + symbol `do` | `do-process` |
| others (self-defined libraries) | `route&add` in `self-defined-rules/router.sls` |

If an identifier is a meta-form, the corresponding rule runs **before** recursion. If it is an ordinary procedure, `application-process` runs. In all cases `step` then recurses into the children.

### 3.3 What Is a Substitution?

A substitution is simply an S-expression stored in an `index-node`'s `index-node-substitution-list`. It represents one possible type for that node. Because an expression may legitimately have multiple types (overloaded primitives, union types), the substitution list is a **list of alternatives**, not a single mapping.

For example, after running the generator on `(+ 1 2)`, the node for the whole expression may contain:

```scheme
;; substitution list of the (+ 1 2) node
((number? <- (inner:list? number? ...)) number? number?)
```

The first element is the type of `+`; the remaining elements are the argument nodes. During interpretation the interpreter sees that this is an `inner:executable?` (a function applied to arguments) and performs β-reduction.

---

## 4. Rule Reference (Detailed)

Rules live in `analysis/type/substitutions/rules/` and a few self-defined extensions.

### 4.1 `trivial.sls` – Literals, Symbols, and Implicit Conversions

`trivial-process` is the most complex rule because it handles the bottom of the AST.

**Literals**

| Literal form | Type attached |
|--------------|---------------|
| char? | `char?` |
| string? | `string?` |
| boolean? | `boolean?` |
| fixnum? | `fixnum?` |
| bignum? | `bignum?` |
| integer? | `integer?` |
| cflonum? | `cflonum?` |
| flonum? | `flonum?` |
| rational? | `rational?` |
| real? | `real?` |
| complex? / number? | `number?` |

**Composite literals**

- `(e₁ e₂ …)` → `(inner:list? node₁ node₂ …)` where each `nodeᵢ` is either the child `index-node` or a virtual node created for unquote-splicing.
- `#(e₁ e₂ …)` → `(inner:vector? node₁ node₂ …)`.
- Dotted pair `(a . b)` → `(inner:pair? node-a node-b)`.

**Symbols (identifiers)**

For a symbol reference, `trivial-process` looks up all available `identifier-reference`s and generates substitutions depending on the binding kind:

1. **Constructor / Getter / Setter** – attach the identifier itself (its `type-expressions` will carry the real type).
2. **Predicator** – attach `(boolean? <- (inner:list? something?))`.
3. **Imported from another document** – attach the target `index-node` (or its `type-expressions` if already known).
4. **Local variable** – attach the `index-node` of the initialization site. This creates a direct edge in the equation graph.

**Gradual Typing / Implicit Conversion**

This is where the system converts "the code is executable" into "the parameter has the expected type". When a local variable occurs as an argument in a function application, and that variable is a `parameter`, `trivial-process` generates a **macro substitution** on the variable's definition site:

```scheme
;; If `a` is the i-th parameter of a call to `+`
;; the substitution attached to `a`'s binding node becomes:
((with ((a b c))
   ((with ((x d0 d1 d2))
      di)            ;; di is the symbol for the i-th parameter
    c))
 +)
```

After macro expansion (see §6), this effectively says: "the type of `a` is whatever the type of `+`'s i-th parameter is". This is the mechanism that turns `(lambda (a) (+ 1 a))` into `a : number?` without any explicit annotation.

### 4.2 `application.sls` – Function Application

The simplest rule:

```scheme
(define (application-process document index-node)
  (extend-index-node-substitution-list index-node (index-node-children index-node)))
```

It records that the type of the whole application is "the function node applied to the argument nodes". The interpreter later reduces this.

### 4.3 `lambda.sls` – Lambda Abstraction

For `(lambda (p₁ p₂ …) body)`:

1. Collect the parameter `index-node`s.
2. Build `(inner:list? p₁ p₂ …)` via `construct-parameter-index-nodes-products-with`.
3. Build the function type `(body <- (inner:list? p₁ p₂ …))` via `construct-lambdas-with`.
4. Attach it to the lambda node.

Because `body` and each `pᵢ` are themselves `index-node`s, the type remains symbolic until interpretation.

### 4.4 `let.sls` / `let*.sls` – Binding Forms

`let` is treated as syntactic sugar for lambda application:

```scheme
(let ((x init)) body)
;; ≈
((lambda (x) body) init)
```

The rule therefore:

1. Attaches the `return-index-node` (the last body expression) to the `let` node, and vice-versa.
2. For each binding pair `(identifier init)`, attaches `init` to `identifier` and `identifier` to `init` (bidirectional substitution).

Named let receives additional handling: the loop identifier is given a function type constructed from the binding parameters and the body, exactly like `lambda`.

`let*-process` additionally covers `letrec`/`letrec*` (the standalone `letrec.sls` was removed and its dispatch unified into `let*.sls`); the key difference from plain `let` is that the loop identifier is not present, so no extra lambda wrapper is built.

### 4.5 `if.sls` – Conditional

For `(if cond then else)`:

- `cond` gets `something?` (we do not attempt boolean-truthiness analysis).
- `then` and `else` are each attached to the `if` node, and the `if` node is attached back to both branches. This encodes a **union type**: the value of the `if` may be either branch.

For two-arm `if`, the union has two members; for one-arm `if`, only the `then` branch is attached.

### 4.6 `define.sls` – Top-Level & Internal Definitions

Three shapes:

1. `(define (f p₁ p₂ …) body)` – like `lambda`, but the resulting function type is attached to the *identifier* node of `f`, not to the `define` node.
2. `(define (f p₁ p₂ … . rest) body)` – rest parameter is represented as `(inner:list? something? …)`.
3. `(define var init)` – bidirectional link between `var` and `init`.

#### Rest / dotted parameter support

2026-06-18 修复：`define-process` 之前只匹配正规参数列表 `(f p₁ p₂ …)`，导致 `(define (make-alist . args) …)` 这类变长参数函数无法获得函数类型签名。

修复后在 `analysis/type/substitutions/rules/define.sls` 中增加了匹配模式：

```scheme
[(_ ((? symbol? identifier) . rest) tail)
  ...
  (construct-lambdas-with (list tail-index-node) '((inner:list? something? ...)))]
```

`rest` 参数被保守地表示为 `(inner:list? something? …)`。这能让 `make-alist` 等函数被正确识别为函数，但返回类型仍依赖函数体推断。

### 4.7 `case-lambda.sls` – Overloaded Arity

Each clause `(((p₁ p₂ …) body))` generates an independent function type. All clauses are attached to the root `case-lambda` node, producing a list of alternative types. During interpretation the interpreter may resolve to any of them; in practice the call site arity usually selects the correct one.

### 4.8 `record.sls` – `define-record-type`

Collects the `constructor`, `predicator`, `getter`s and `setter`s exported by the record definition and assigns them canonical types:

- `predicator` → `(boolean? <- (inner:list? something?))`
- `constructor` → `(predicator <- (inner:list? something? …))`
- `getter` → `(something? <- (inner:list? predicator))`
- `setter` → `(inner:void? <- (inner:list? predicator something?))`

These are stored directly in the `identifier-reference-type-expressions` field so that they are available even when the record definition is imported from another file.

> **Note:** Although `inner-type-checker.sls` defines an `inner:record?` recogniser of the form `(inner:record? pred (inner:pair? getter τ) …)`, no rule currently generates such a unified record-type value. The record's type is therefore represented *indirectly* through its predicate identifier and the per-field accessor signatures above.

### 4.9 `cond.sls` – Multi-Branch Conditional

Each clause `(test expr₁ expr₂ …)` is processed independently:

- `test` gets `something?`.
- The last expression of the clause is attached to the root `cond` node (and back).

### 4.10 `begin.sls` – Sequencing

The type of a `begin` form is the type of its last expression:

```scheme
(extend-index-node-substitution-list index-node (car (reverse (index-node-children index-node))))
```

### 4.11 Self-Defined Rules & Macro Call Sites (`ufo-try`, generic macro arm)

The router in `self-defined-rules/router.sls` has two arms today:

- **`ufo-try`** – `try-process` keeps a hand-written rule because `try` is a control-flow form, not a binder pattern; it attaches union types for exception clauses.
- **Generic macro arm** – any call site whose head resolves to a `define-syntax` macro (root-ancestor type `syntax-variable`) is routed to `(macro-infer root-library-node document call-node)`, i.e. `type:expand-and-infer` in `analysis/type/substitutions/macro-expansion.sls`. `run-generator` (the generator's own `construct-substitutions-for`) is injected by `generator.sls` to break the import cycle generator → router → macro-expansion → generator.

  The expansion-driven pipeline: Chez expands the enclosing define (via `ir-auto-route:expand-call-site`, the same forms/env machinery as the identifier side) → the annotated expansion is rebuilt into a virtual index-node document that inherits the caller's reference table → the real generator runs over it → substitutions are copied back onto the original call site's nodes by source position. No hand-written pattern knowledge; macro-agnostic by construction. A per-expansion node budget (`private:max-expansion-nodes = 100000`) aborts pathological expansions during the rebuild, and unexpandable call sites are negatively cached per document + text identity.

  > History: the earlier hand-written `ufo-match` rule > (`self-defined-rules/ufo-match/match.sls`) and the IR flow-language > extractor were retired on 2026-10-27 in favour of this architecture. > The IR flow extractor survives as the differential-test oracle > (`tests/analysis/type/substitutions/test-macro-rules-vs-ir.sps`). > Mechanism walkthrough: `doc/analysis/ir-auto-route-walkthrough.md`.

---

## 5. Phase II – The DSL Interpreter

Once substitutions are generated, every `index-node` carries a list of raw type expressions. The interpreter resolves these into concrete types.

### 5.1 Entry Points

| Function | Purpose |
|----------|---------|
| `type:interpret` | Core interpreter. Returns a `type:environment` whose `result-list` contains the resolved types. |
| `type:interpret-result-list` | Convenience wrapper that extracts the `result-list` from `type:interpret`. |
| `type:depature&interpret->result-list` | First expands any macros inside the expression, then interprets. |
| `type:recursive-interpret-result-list` | Iteratively extends the substitution set to break recursive cycles (see §5.5). |
| `type:interpret->strings` | Pretty-prints results as human-readable strings. |

### 5.2 The `type:environment` Record

```scheme
(define-record-type type:environment
  (fields
    (mutable substitution-list)  ; global equations that may be extended
    (mutable result-list)))      ; output accumulator
```

The environment is passed through recursive calls so that macro expansions can consult and even extend the substitution list.

### 5.3 `type:interpret` – Case Analysis

`type:interpret` receives `(expression env memory max-depth)`. `memory` is a list of already-visited expressions (loop detection). `max-depth` defaults to `PRIVATE-MAX-DEPTH` (10).

The interpreter proceeds by structural case analysis:

| Expression shape | Action |
|------------------|--------|
| `null?` | Return `'()` |
| Depth exceeded or expression in `memory` | Return the expression unchanged (give up). |
| `inner:executable?` with macro head | Expand the macro (see §6), interpret each expansion. |
| `inner:executable?` with lambda head | β-reduce: match arguments against parameter template, substitute into body, interpret body. |
| `index-node?` | Look up its `substitution-list`; interpret each substitution. |
| `inner:macro?` | Return unchanged (it needs an outer application context to expand). |
| `inner:list?`, `inner:vector?`, `inner:pair?`, `inner:lambda?` | Interpret each element independently, then take the **Cartesian product** of all element results. |
| Plain `list?` (not an inner type) | Interpret the `car`; if it becomes a lambda or macro, re-assemble and interpret again. |
| `identifier-reference?` | Return its `type-expressions` (for constructors/getters/setters) or the identifier itself. |
| Anything else | Return as-is (concrete atom). |

### 5.4 Cartesian Product & Pruning

When a list type has multiple possible types for each element, the interpreter uses Cartesian product to enumerate all combinations. This is the main performance bottleneck.

`private-generate-cartesian-product-procedure` implements a **tiered pruning strategy**:

1. Try full Cartesian product.
2. If the product size exceeds `PRIVATE-MAX-CARTESIAN-PRODUCT` (50 000), filter each dimension to keep only `type:partially-solved?` items.
3. Still too large? Increase the minimum solved-leaves threshold (2, then 3).
4. Still too large? Keep only fully `type:solved?` items.
5. Absolute fallback: return an empty list (effectively `something?`).

This graceful degradation prevents the interpreter from hanging on pathological code while still giving useful results for common cases.

### 5.5 Solving Recursion & The Halting Problem

Recursion is ubiquitous in Scheme. A naïve interpreter would loop forever on:

```scheme
(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))
```

`type:recursive-interpret-result-list` solves this with an **iterative fixed-point loop**:

```scheme
(let loop ([i 0] [targets '(expr)] [env env] [result '()])
  (if (or (>= i max-recursion) (too-many-targets?))
      (dedupe result targets)
      (let* ([r0 (map (lambda (e) (type:depature&interpret->result-list e env)) targets)]
             [r1 (filter type:solved? r0)])
        (loop (+ i 1)
              (filter (lambda (x) (not (type:solved? x))) r0)
              env
              (append result r1)))))
```

- **Iteration 0**: interpret every target expression.
- Collect expressions that are already `type:solved?` into the result set.
- Re-interpret the unsolved expressions. Because the environment's substitution list is mutable, some `index-node` placeholders may have accumulated new solved substitutions from siblings.
- Repeat up to `PRIVATE-MAX-RECURSION` (2) times or until the target set grows beyond `PRIVATE-MAX-RECURSION-SET-SIZE` (400).

This is not a full fixed-point solver, but in practice it resolves most self-referential function types because the base case (`if`) introduces a union type that eventually becomes solved.

### 5.6 Solved vs. Unsolved

```scheme
(define (type:solved? expression)
  …)
```

An expression is **solved** when:
- It is an atom (symbol, identifier-reference, `'something?`, `'inner:void?`).
- It is a list, and every element is solved, **and** the list itself is an `inner:trivial?` type expression (i.e. not a raw application waiting for reduction).

`type:partially-solved?` counts how many leaf nodes are solved; it is used by the Cartesian-product pruner.

---

## 6. The Macro Engine

Some R6RS primitives cannot be expressed as simple function types. `car`, for example, must return the *first element type* of a list, but the list's length is unknown at compile time. The system solves this with a small macro language embedded in the type DSL.

### 6.1 `with` – Pattern Abstraction

`with` has two syntactic forms depending on whether the denotions (pattern templates) are wrapped in an extra list.

**Form A — flat denotions (most common)**
```scheme
((with (a b c)                ; denotions = (a b c)
     body)
  x y z)                      ; inputs    = (x y z)
```

**Form B — wrapped denotions (single-input scenarios)**
```scheme
((with ((a b c **1))          ; denotions = ((a b c **1))
     body)
  (inner:list? x y z))        ; inputs    = ((inner:list? x y z))
```

When the interpreter sees a macro application, it:

1. Interprets the arguments to obtain their type-level values.
2. Calls `candy:matchable?` to see if the templates match.
3. Uses `candy:match-left` to produce a list of `((symbol . value) …)` pairs.
4. Substitutes these pairs into `body` via `private-with`.
5. Interprets the resulting body.

> **Critical invariant**: `execute-macro` calls `candy:matchable?` with `denotions` on the left and `expanded-inputs` on the right. The two lists must have the **same nesting level** — flat against flat, wrapped against wrapped. Mismatching them causes `candy:matchable?` to return `#f` and the macro is silently left unexpanded. See §6.7 Trap 1.

### 6.2 Built-In Macro Helpers

| Macro | Semantics |
|-------|-----------|
| `with-append` | `(with-append list1 list2)` → `(append list1 list2)`, then recursively execute |
| `with-equal?` | `(with-equal? a b body)` → `body` if `a` equals `b`, else the **original macro expression unchanged** |

`with-equal?` is the primary guard mechanism. When the guard fails, the entire expression is returned as-is; `type:interpret` then tries the next signature in the substitution list.

### 6.3 Macro Execution Internals

#### 6.3.1 `macro-head-execute-with` — Gatekeeper

```scheme
(define (macro-head-execute-with expression interpreted-inputs)
  (match expression
    [(('with ((? inner:macro-template? denotions) **1) body)
      (? inner:trivial? inputs) **1)
     (execute-macro `((with ,denotions ,body) ,@interpreted-inputs))]
    [else (raise 'macro-not-match:macro-head-execute-with)]))
```

The match pattern `((? inner:trivial? inputs) **1)` uses **ufo-match** semantics: `**1` means "one or more occurrences of the preceding pattern", and the predicate `inner:trivial?` is checked **per element**. `inputs` binds to the entire list of matched elements.

| Input shape | Match result | Why |
|-------------|--------------|-----|
| `(x)` | ✅ `inputs = (x)` | Single element satisfies `inner:trivial?` |
| `(x y z)` | ✅ `inputs = (x y z)` | All three satisfy `inner:trivial?` |
| `(x y z)` where `y` is bare `number?` | ❌ match fails | Bare symbols fail `inner:trivial?` |
| `(x y z)` where `y` is `identifier-reference` | ✅ match passes | `identifier-reference?` is accepted |

> **Field note**: In real substitution generation every type predicate (`fixnum?`, `string?`, etc.) is wrapped as an `identifier-reference` by `construct-type-expression-with-meta`. Bare symbols fail the guard, which is why synthetic repros using raw symbols sometimes look like macro bugs when they are actually data-format mismatches.

#### 6.3.2 `execute-macro` — The Expander

```scheme
(define (execute-macro expression)
  (match expression
    [(('with ((? inner:macro-template? denotions) **1) body) inputs ...)
     (let ([expanded-inputs (map execute-macro inputs)])
       (if (and (andmap inner:trivial? expanded-inputs)
                (candy:matchable? denotions expanded-inputs))
           (execute-macro (private-with body (candy:match-left denotions expanded-inputs)))
           expression))]
    [('with-append (? list? a) (? list? b)) (execute-macro (append a b))]
    [('with-equal? a b body) (if (equal? a b) (execute-macro body) expression)]
    [else expression]))
```

Three checks before expansion:

1. `andmap inner:trivial? expanded-inputs` — every argument must be a valid type expression.
2. `candy:matchable? denotions expanded-inputs` — shapes must align (see §6.7 Trap 1 for the wrapping-level requirement).
3. If both pass, `candy:match-left` produces bindings and `private-with` substitutes them into `body`.

On failure, `execute-macro` returns the **original expression unchanged**. This residue then flows back into `type:interpret-result-list` and participates in Cartesian products downstream.

#### 6.3.3 `private-with` — Substitution Engine

```scheme
(define (private-with body match-pairs)
  (fold-left
    (lambda (left pair)
      (let ([denotion (car pair)] [input (cdr pair)])
        (cond 
          [(symbol? denotion) (private-substitute left denotion input)]
          [(index-node? denotion) (private-substitute left denotion input)]
          [(identifier-reference? denotion) left]  ; deliberately ignored
          [(and (list? denotion) (list? input))
           (if (candy:matchable? denotion input)
               (private-with body (candy:match-left denotion input))
               (raise 'macro-not-match:private-with-list?))]
          [else (raise 'macro-not-match:private-with-else)])))
    body 
    match-pairs))
```

`private-substitute` is a naïve tree walk that replaces **every** occurrence of `from` with `to`. When `denotion` is an `identifier-reference`, `private-with` returns `left` unchanged — an `identifier-reference` in pattern position acts as a **wildcard that matches anything but binds nothing**.

When both `denotion` and `input` are lists, `private-with` recurses with `candy:match-left`. The previous code used `candy:match-right` when `input` contained `**1` or `...`, but that path was broken: `match-right` expands list-valued bindings into multiple flat pairs, and `fold-left` + `private-substitute` can only consume the first one. See §6.7 Trap 3 for details.

### 6.4 Example: `car`

`car` carries **two** signatures in `rnrs-meta-rules.sls`:

```scheme
(car (with ((a b c **1)) (with-equal? inner:list? a b)))
(car (something? <- (inner:list? (inner:pair? something? something?))))
```

The first is a **macro** for `inner:list?` arguments; the second is a conservative function type for `inner:pair?` arguments. `type:interpret` tries them in order.

#### 6.4.1 Macro execution trace

Apply the macro to `(inner:list? fixnum? number?)`:

1. `execute-macro` sees a `with` application:
   ```scheme
   ((with ((a b c **1)) (with-equal? inner:list? a b))
    (inner:list? fixnum? number?))
   ```
2. `candy:matchable?` checks template `(a b c **1)` against argument `(inner:list? fixnum? number?)`.
   - Segment `a` matches `inner:list?`.
   - Segment `b` matches `fixnum?`.
   - Segment `c **1` matches `(number?)` (one occurrence).
   - Match succeeds.
3. `candy:match-left` produces bindings:
   ```scheme
   ((a . inner:list?) (b . fixnum?) (c . (number?)))
   ```
4. `private-with` substitutes into body `(with-equal? inner:list? a b)`:
   ```scheme
   (with-equal? inner:list? inner:list? fixnum?)
   ```
5. `execute-macro` evaluates `with-equal?`:
   - `(equal? inner:list? inner:list?)` → `#t`.
   - Result is `fixnum?`.

#### 6.4.2 Match-failure & fallback

If the argument is **not** an `inner:list?` (e.g. an `inner:pair?`):

- Step 2 still succeeds (any list-shaped argument matches the template).
- Step 5 fails because `a` ≠ `inner:list?`.
- `with-equal?` returns the **original macro expression** unchanged.
- `type:interpret` sees the macro was not reduced, tries the next signature (the pair version), and returns `something?`.

#### 6.4.3 Template length requirement

The template `(a b c **1)` requires the argument to contain **at least three** elements (`a`, `b`, and at least one `c`). Therefore:

| Argument shape | Macro matches? | Result |
|---|---|---|
| `(inner:list? fixnum? number?)` (3 elems) | ✅ | `fixnum?` |
| `(inner:list? fixnum? number? string?)` (4 elems) | ✅ | `fixnum?` |
| `(inner:list? fixnum?)` (2 elems) | ❌ | falls back to pair version → `something?` |
| `(inner:pair? something? something?)` | ❌ (guard fails) | `something?` |

The same pattern applies to all `c*r` macros listed below.

### 6.5 Extending the `c*r` Macro Family

The `cdr`/`cddr`/`cdddr`/`cddddr` macros already exist in `rnrs-meta-rules.sls`. They use `with-append (inner:list?)` to reconstruct the tail type:

```scheme
(cdr    (with ((a b c **1))       (with-equal? inner:list? a (with-append (inner:list?) c))))
(cddr   (with ((a b c d **1))     (with-equal? inner:list? a (with-append (inner:list?) d))))
(cdddr  (with ((a b c d e **1))   (with-equal? inner:list? a (with-append (inner:list?) e))))
(cddddr (with ((a b c d e f **1)) (with-equal? inner:list? a (with-append (inner:list?) f))))
```

The following `inner:list?` macros have been added to `rnrs-meta-rules.sls`. They improve hover-tooltip precision for code that uses `cadr`, `caddr`, etc. on known-length lists.

#### 6.5.1 `cadr` – second element

```scheme
(cadr (with ((a b c d **1)) (with-equal? inner:list? a c)))
```

| Binding | Value for `(inner:list? τ₁ τ₂ τ₃ τ₄ …)` |
|---|---|
| `a` | `inner:list?` |
| `b` | `τ₁` |
| `c` | `τ₂` ← **returned** |
| `d` | `(τ₃ τ₄ …)` |

Requires the argument to contain at least **four** elements. Shorter lists fall back to the pair version.

#### 6.5.2 `caddr` – third element

```scheme
(caddr (with ((a b c d e **1)) (with-equal? inner:list? a d)))
```

Requires at least **five** elements. `d` binds `τ₃`.

#### 6.5.3 `cadddr` – fourth element

```scheme
(cadddr (with ((a b c d e f **1)) (with-equal? inner:list? a e)))
```

Requires at least **six** elements. `e` binds `τ₄`.

#### 6.5.4 Nested-list macros (`caar`, `caaar`, `cadar`, `cddar`, …)

Macros that decompose **nested** list types are written by nesting `with` macro applications inside the macro body. `execute-macro` recursively expands them, and since the interpreter pre-expands macro arguments (including `with-append` and `with-equal?`) before matching a macro head, inner `cdr` steps work as well.

```scheme
(caar (with ((a b c ...))
        (with-equal? inner:list? a
          ((with ((d e f **1)) (with-equal? inner:list? d e)) b))))

(caaar (with ((a b c ...))
         (with-equal? inner:list? a
           ((with ((d e f ...))
              (with-equal? inner:list? d
                ((with ((g h i **1)) (with-equal? inner:list? g h)) e)))
            b))))

(cadar (with ((a b c ...))
         (with-equal? inner:list? a
           ((with ((d e f ...))
              (with-equal? inner:list? d
                ((with ((g h i ...)) (with-equal? inner:list? g h))
                 (with-append (inner:list?) f))))
            b))))

(cddar (with ((a b c ...))
         (with-equal? inner:list? a
           ((with ((d e f ...))
              (with-equal? inner:list? d
                ((with ((g h i ...)) (with-equal? inner:list? g (with-append (inner:list?) i)))
                 (with-append (inner:list?) f))))
            b))))
```

The nesting depth is limited only by readability. In practice only `caar`, `cadar`, `cddar`, and `caaar` appear often enough in real code to justify the extra signature lines.

#### 6.5.5 Summary table – all `c*r` functions

| Function | List macro? | Kind | Min elems | Notes |
|---|---|---|---|---|
| `car` | ✅ | flat `(a b c **1)` | 3 | returns `b` |
| `cdr` | ✅ | flat `(a b c **1)` | 3 | returns `(with-append (inner:list?) c)` |
| `caar` | ✅ | nested `with` | 3 (inner) | `car` of `car` |
| `cadr` | ✅ | flat `(a b c d **1)` | 4 | returns `c` |
| `cdar` | ✅ | nested `with` | 3 (inner) | `cdr` of `car` |
| `cddr` | ✅ | flat `(a b c d **1)` | 4 | returns `(with-append (inner:list?) d)` |
| `caaar` | ✅ | nested `with` | 3 (inner) | `car` of `car` of `car` |
| `caadr` | ❌ | — | — | feasible (3-layer nested `with`), not yet added |
| `cadar` | ✅ | nested `with` | 3 (inner) | `car` of `cdr` of `car` |
| `caddr` | ✅ | flat `(a b c d e **1)` | 5 | returns `d` |
| `cdaar` | ❌ | — | — | feasible, not yet added |
| `cdadr` | ❌ | — | — | feasible, not yet added |
| `cddar` | ✅ | nested `with` | 3 (inner) | `cdr` of `cdr` of `car` |
| `cdddr` | ✅ | flat `(a b c d e **1)` | 5 | returns `(with-append (inner:list?) e)` |
| `caaaar` | ❌ | — | — | feasible (4-layer nested `with`), extremely rare in practice |
| `caaadr` | ❌ | — | — | feasible, not yet added |
| `caadar` | ❌ | — | — | feasible, not yet added |
| `caaddr` | ❌ | — | — | feasible, not yet added |
| `cadaar` | ❌ | — | — | feasible, not yet added |
| `cadadr` | ❌ | — | — | feasible, not yet added |
| `caddar` | ❌ | — | — | feasible, not yet added |
| `cadddr` | ✅ | flat `(a b c d e f **1)` | 6 | returns `e` |
| `cdaaar` | ❌ | — | — | feasible, not yet added |
| `cdaadr` | ❌ | — | — | feasible, not yet added |
| `cdadar` | ❌ | — | — | feasible, not yet added |
| `cdaddr` | ❌ | — | — | feasible, not yet added |
| `cddaar` | ❌ | — | — | feasible, not yet added |
| `cddadr` | ❌ | — | — | feasible, not yet added |
| `cdddar` | ❌ | — | — | feasible, not yet added |
| `cddddr` | ✅ | flat `(a b c d e f **1)` | 6 | returns `(with-append (inner:list?) f)` |

> **Note:** For every function in the table, an additional fallback signature `(something? <- (inner:list? (inner:pair? something? something?)))` is kept in `rnrs-meta-rules.sls` so that short lists and generic pairs still type-check conservatively.

#### 6.5.6 Why some `c*r` functions are not added

After the `execute-macro` fix (§6.5.4), **all** `c*r` functions can theoretically receive a list macro—there is no longer a technical block. The remaining ones are omitted for two pragmatic reasons:

1. **Readability.** A 4-layer nested `with` (e.g. `caaaar`) is ~10 lines of dense parenthesis soup. It is easy to get a bracket wrong and hard to review.
2. **Frequency.** Functions beyond 3 layers (`caaaar`, `caaadr`, `caadar`, …) appear so rarely in real Scheme code that the pair fallback `(something? <- (inner:list? (inner:pair? something? something?)))` is sufficient for IDE use.

If a project genuinely needs `caadr` or `cdaar`, the pattern is identical to `cadar`/`cddar`: nest `with` macros, use `with-append (inner:list?)` for every `cdr` step, and keep the outer `with-equal? inner:list?` guard at each level.

### 6.6 `candy:matchable?` – Dynamic-Programming Matcher

The matcher lives in `syntax-candy.sls`. It segments a list into alternating **fixed** and **repeatable** sections. A segment is delimited by `...` or `**1`.

Matching is performed by filling a 2-D matrix (rows = template segments, columns = argument segments) with three statuses:

- `'matched` – both segments align.
- `'skipped` – one side is skipped (used for zero-or-more).
- `'unused` – unreachable cell.

The matrix is explored depth-first, allowing paths that step right or down but never diagonally. This is essentially a restricted regular-expression matcher for heterogeneous lists. It supports overlapping repeatable segments, which is needed for types like `(inner:list? number? ... string? ...)`.

### 6.7 Known Traps & Invariants

#### Trap 1: Wrapping-Level Mismatch

`execute-macro` passes `denotions` and `expanded-inputs` to `candy:matchable?` **without any unwrapping**. The two must share the same nesting level.

| Denotions | Inputs | Result |
|---|---|---|
| `(a b c)` (flat) | `(x y z)` (flat) | ✅ match |
| `((a b c **1))` (wrapped) | `((x y z))` (wrapped) | ✅ match |
| `(a b c)` (flat) | `((x y z))` (wrapped) | ❌ mismatch — `candy:matchable?` returns `#f` |
| `((a b c **1))` (wrapped) | `(x y z)` (flat) | ❌ mismatch |

If you write a macro with wrapped denotions (Form B), the caller must also wrap the arguments. The `c*r` macros that use `inner:list?` (§6.5) rely on this wrapping because the entire list type `(inner:list? τ₁ τ₂ …)` is treated as a single macro argument.

#### Trap 2: Symbol-Category Mismatch

`execute-macro` requires `(andmap inner:trivial? expanded-inputs)`. `inner:trivial?` accepts `index-node?`, `identifier-reference?`, `something?`, and `inner:void?`. It **rejects bare symbols** such as `number?` or `fixnum?`.

In real substitution rules all predicates are `identifier-reference` records produced by `construct-type-expression-with-meta`. However, hand-written unit tests that feed raw symbols will silently fail to match any macro, leaving the `with` expression unexpanded. If that residue later flows into a Cartesian product, the product includes the raw `with` expression as one of its branches, which usually produces a meaningless type.

> **Rule of thumb**: never use bare symbols in synthetic repros. Wrap them with `construct-type-expression-with-meta` or at least ensure they satisfy `inner:trivial?`.

#### Trap 3: `candy:match-right` & `private-with` Interaction (Fixed)

When a template contains repeated segments (`**1` or `...`), `candy:match-right` returns a flat list of pairs:

```scheme
;; template: (a b c **1)   argument: (x y z w)
;; candy:match-right produces:
((a . x) (b . y) (c . z) (c . w))
```

`private-with` then iterates over these pairs with `fold-left` and `private-substitute`. Because `private-substitute` replaces **every** occurrence of the denotion symbol, the two `(c . _)` pairs overwrite each other rather than accumulating into a list. This means repeated-segment substitution inside `private-with` was **incomplete** for the `candy:match-right` path.

**A more severe variant** was discovered: even when the *template* has no repeated segments, if the *argument* itself is a list type expression (e.g. `(inner:list? something? ...)` which contains `...` or `**1`), `match-right` would still expand the matched binding into multiple pairs:

```scheme
;; template: (x d0 d1)
;; argument: (inner:list? (something? <- ...) (inner:list? something? ...) **1)
;; match-right incorrectly produces:
((x . inner:list?)
 (d0 . (something? <- ...))
 (d1 . inner:list?)       <-- d1 bound to first element only
 (d1 . something?)
 (d1 . ...))
```

`private-with` would then substitute `d1` with `inner:list?` and discard `something?` and `...`, causing the parameter type to collapse from `(inner:list? something? ...)` to bare `inner:list?`.

**Fix (2025-05-11)**: `private-with` now always uses `candy:match-left`, which preserves list-valued bindings as a single pair:

```scheme
;; match-left correctly produces:
((x . inner:list?)
 (d0 . (something? <- ...))
 (d1 . (inner:list? something? ...)))   <-- d1 keeps the full list
```

> **User concern**: `interpreted-inputs` passed to macros (e.g. the type signature of `map`) can themselves contain `**1` or `...`. The old `match-right` branch would trigger on *any* `**1` in the input, not just repeated-segment templates. Using `match-left` universally is safer because it never fragments a binding.

#### Trap 4: Macro Residue in Cartesian Products

When `execute-macro` fails to expand (guard fails or mismatch), it returns the **original macro expression unchanged**. If this residue is fed back into `type:interpret-result-list`, it participates in the Cartesian product of type combinations. The product generator does not know how to reduce a `with` expression, so the resulting branch is typically discarded as an invalid type, but it wastes time and may confuse downstream consumers.

> **Lesson learned**: `application.sls` was briefly modified to inject `with` macros into every call site. Even when the macro did not expand, the residue broke `test-define` and `test-let` because unexpanded `with` terms appeared inside substitution lists that the interpreter tried to fold into type sets. The fix was to revert `application.sls` to its simple form.

#### Trap 5: `with-append` Is Not `append`

`with-append` is **only** recognised inside `execute-macro`. It is not a general function. You cannot write:

```scheme
;; WRONG — with-append is not in scope outside a macro body
(let ([tail (with-append (inner:list?) (number?))]) ...)
```

It must appear as the immediate expression being interpreted, and its arguments must be literal lists known at macro-expansion time.

#### Trap 6: Template Length vs Runtime Length

A template like `(a b c **1)` requires the argument to contain **at least three** elements. If the list is shorter at runtime, the macro does not match and falls back to the next signature. This is by design — `car` on a two-element list cannot statically know whether the first element is the "head" or the "tail" in a type sense — but it means short lists always degrade to the conservative pair fallback (`something?`).

#### Trap 7: `identifier-reference` As Wildcard

In `private-with`, if the denotion is an `identifier-reference?`, the branch returns `left` unchanged:

```scheme
[(identifier-reference? denotion) left]  ; deliberately ignored
```

This means an `identifier-reference` in pattern position acts as a **wildcard that matches anything but binds nothing**. It is useful when you want a segment to match without capturing its value (e.g. forcing a shape check but discarding the matched predicate). However, if you intended to capture the matched value into a variable, using an `identifier-reference` instead of a symbol will silently drop it.

---

## 7. Type Relations

`domain-specific-language/interpreter.sls` exports three relation predicates:

### 7.1 `type:->?` – Subtype

`(type:->? left right env)` asks whether `left` is a subtype of `right`.

Rules (in order):
1. `equal?` → `#t`.
2. `left` is `something?` → `#f` (top is supertype of nothing concrete).
3. `right` is `something?` → `#t`.
4. Both are `identifier-reference?`s → walk the `identifier-reference-parents` chain (e.g. `integer?` → `rational?` → `real?` → `number?`).
5. Both are lists → convert `inner:list?` to `inner:pair?` via `inner:?->pair`, then element-wise `type:->?`.
6. Fallback → check whether `right` appears in `(type:interpret-result-list left)`.

### 7.2 `type:<-?` – Supertype

Reverse of `type:->?`.

### 7.3 `type:=?` – Type Equality

`(and (type:->? left right) (type:<-? left right))`.

---

## 8. Built-In Type Signatures (`rnrs-meta-rules`)

`analysis/type/substitutions/rnrs-meta-rules.sls` contains a large sorted alist that maps primitive names to their type signatures. It covers R6RS base, R6RS standard libraries, and Chez Scheme extensions—more than 1 100 entries.

Examples:

```scheme
(+ (number? <- (inner:list? number? ...)))
(- (number? <- (inner:list? number? **1)))
(map ((inner:list? something? ...) <-
       (inner:list?
         (something? <- (inner:list? something?))
         (inner:list? something? ...) **1)))
(apply (something? <-
         (inner:list?
           (something? <- (inner:list? something?))
           something? ...
           (inner:list? something? ...))))
```

During workspace initialization (`init-type-expressions` in `analysis/identifier/meta.sls`), these signatures are loaded and attached to the corresponding `identifier-reference` records in the top-level environment. This is the seed data that makes the entire inference engine work: without knowing that `+` expects `number?`, the implicit conversion in `trivial-process` would have no target type.

---

## 9. Integration with the Rest of the Server

### 9.1 When Type Inference Runs

Type inference is gated by the `type-inference?` flag passed to `init-workspace`. When enabled:

1. `init-references` (in `analysis/workspace.sls`) runs `construct-substitutions-for` on every document in topological order.
2. After substitution generation, `identifier-reference-type-expressions` are populated by calling `type:recursive-interpret-result-list` on each exported identifier.

The P1 recursive-return fixpoint (`fixpoint.sls`) and the P3 dotted-rest element inference (`rest-elements.sls`) sit behind the `type-inference-advanced?` parameter (exported from `generator.sls`, 2026-11-05, commit `9b99a8d7`). Since the 2026-11-05 refactor the two passes differ in structure: fixpoint *collection* rides the generator walk -- the define/let type rules note recursion groups as they fire (the cddr half of the rule chain, the same two-phase idiom as the identifier side's `(do-nothing . begin-process)`) -- and is always on; only the *solve* phase is gated. `rest-elements` stays a whole gated post-pass. Servers opt in with `run.ss -t advance` (`-t` is tri-state: `enable` (default) / `advance` / `disable`); `init-server`'s 11-argument form sets the parameter before the thread pool spawns. Measured on this repo: full init with type inference is ~11 s basic vs ~31 s advance, and a pathological single argument interpretation in srfi-115's `regexp-impl.scm` (~22 s, uninterruptible by the passes' between-calls budgets) only runs under `advance`. See `AGENTS.md` §2 "Expensive type-inference gate".

### 9.2 Incremental Updates

When a file is changed:

1. `update-file-node-with-tail` rebuilds the document's `index-node-list`.
2. `refresh-workspace-for` determines whether the library header changed. If not, only the affected file's substitutions are regenerated.
3. `shrink-paths` computes topological batches; `init-references` re-runs `step` on the affected batch.

### 9.3 From Types to LSP Diagnostics

Currently the type system is used primarily for **hover information** (not yet for full `publishDiagnostics`). The `textDocument/hover` handler can look up an identifier's `type-expressions`, map them through `type:interpret->strings`, and display them to the user. The old documents listed "TODO: Diagnostic based on Type System"; the infrastructure is in place, but the actual diagnostic rules (type-mismatch errors) are not yet implemented.

### 9.4 Why Type Inference Must Stay Out of High-Frequency APIs

The two-phase pipeline exists precisely because **Phase II (interpretation / unification) is expensive**. Substitution generation (Phase I) is a single AST walk and is comparatively cheap, but resolving those substitutions through the DSL interpreter involves macro expansion, β-reduction, Cartesian products, and iterative fixed-point loops. On pathological recursive functions the interpreter can produce tens of thousands of intermediate type expressions and take minutes to return.

This has a direct consequence for LSP API design:

> **Do not invoke type inference on every request path.**

`textDocument/documentSymbol` is a good example of an API that must **not** call `type:interpret-result-list` (or any helper that forces Phase II resolution) while building its response. Users open files constantly; if opening a file triggers even one `type:interpret-result-list` call per exported identifier, editors will stutter or hang on large files. The correct pattern is:

- **Hover** — user explicitly positions the cursor; latency of a single `type:interpret-result-list` call is acceptable.
- **Completion** — already pays for type inference because the user asked for completions; using the cached `type-expressions` field is fine.
- **Document Symbol / Workspace Symbol / Diagnostics** — these fire on file open, save, or incremental sync. Keep them **type-inference-free** unless the type has already been solved and cached during workspace initialization.

If a future feature (e.g. `SymbolKind` from inferred types) seems to require Phase II on every `documentSymbol` request, the correct fix is either to cache the result during `init-workspace` or to fall back to a cheap syntactic heuristic (e.g. `define.sls` already distinguishes `(define (f ...) ...)` from `(define x ...)`). Calling the interpreter from `protocol/apis/document-symbol.sls` is an architectural mistake and should be rejected in review.

---

## 10. Performance Characteristics & Known Limitations

### 10.1 Strengths

- **No separate type-annotation syntax** – works on unmodified R6RS/R7RS code.
- **Modular rules** – adding support for a new special form only requires a new `xxx-process` rule.
- **Graceful degradation** – when inference is too expensive, the pruner falls back to `something?`; the server never hangs.

### 10.2 Weaknesses

1. **Cartesian-product explosion**
   A function with 4 parameters each having 4 possible types generates 4⁴ = 256 combinations. In the worst case this is factorial growth. The tiered pruner mitigates but does not eliminate the problem.

2. **Approximate recursion handling**
   `type:recursive-interpret-result-list` uses a hard iteration limit (2). Deeply nested recursive types may not fully resolve.

3. **No polymorphic generalisation**
   The system does not implement HM's `∀α.τ` generalisation. A function like `(lambda (x) x)` gets type `(something? <- (inner:list? something?))`, not a true parametric type. This is usually sufficient for IDE use but can lose precision.

4. **`call/cc` and control effects**
   Continuations, dynamic-wind, and other control operators are inherently beyond the scope of this type system. They are typed as `something? <- …`.

5. **Mutability is not tracked**
   `set!` and mutable data structures are not reflected in types. `(define x 1)` followed by `(set! x "hello")` may leave `x` with type `fixnum?` because the substitution graph does not model temporal mutation.

6. **`max-depth` is not propagated through recursive calls**
   `type:interpret` accepts a `max-depth` parameter, but every internal recursive call invokes the 3-arity `(type:interpret-result-list expr env memory)` which silently falls back to the hard-coded `PRIVATE-MAX-DEPTH` (10). Consequently:
   - Specifying a shallow `max-depth` from the outside has no effect on nested expansions.
   - Recursive functions with complex bodies (e.g. `binary-search` in `util/binary-search.sls`) are expanded to depth 10 regardless of the caller's intent.
   - Because `memory`-based cycle detection only records expressions on the current call chain, cross-function recursion (e.g. `binary-search` → `private-collect` → self-call) is not caught when the recursive target is reached through a substitution rather than a direct `type:interpret-result-list` call.
   - The combination of depth-10 expansion and missed cycle detection causes a **Cartesian-product explosion** that can produce millions of intermediate type expressions and hang the interpreter for minutes.
   - `natural-order-compare` survives the same mechanism because its body is simple (`let` + `if` + one recursive call); `binary-search` fails because its clause-2 body contains `cond` with three branches, `let*`, `if`, multiple recursive call sites, and a mutual call to `private-collect`.
   
   **Short-term mitigation:** Lower `PRIVATE-MAX-DEPTH` from 10 to a smaller value (e.g. 3–4).  
   **Proper fix:** Thread `max-depth` through every internal recursive call in `interpreter.sls` (lines 224, 231, 238, 258, 268, 278, 285).

---

## 10.3 Performance Optimization Attempts (Field Notes)

This section records a systematic benchmarking and optimization effort on the type-inference subsystem. It is intended to prevent future developers from repeating the same dead ends.

### Benchmarking Infrastructure

A standalone benchmark runner was added at `bin/benchmark-type-inference.ss`. It measures wall-clock time, CPU time, GC time, and result-list size for:
- `init-workspace` (with and without type inference)
- `construct-substitutions-for` (Phase I)
- `type:interpret-result-list` on specific functions (`binary-search`, `natural-order-compare`, `assq-ref`)
- End-to-end API latency (`hover`)

> **Baseline note (2026-10-29):** all numbers in §10.3 are the 2026-06 pre-optimization baseline (single-function `binary-search cl` measurements). After the 2026-10-28 resource-control work (macro-expansion caches, expansion budget, walker memoization), whole-repo init **with** type inference measures ~474 ms / 471 MB peak. The attempt-by-attempt failure records below remain valid as methodology; the baseline table does not describe the current system.
>
> **2026-11-05 note (post-P1/P3, commit `9b99a8d7` gate):** full-repo init with type inference measures **~11 s basic / ~32 s advance** (min-of-3, this repo, 834 documents). Phase split under `advance`: generator walk ~3.2 s, fixpoint ~3.5-5 s, rest-elements ~18.5-25 s — of which **~22 s is a single uninterruptible `type:interpret-result-list` call** on one call-site argument in srfi-115's `regexp-impl.scm` (the between-calls 50 ms budgets cannot interrupt a running interpretation; argument-level size guards do not help because the blow-up is combinatorial, not closure-size). The two passes are therefore gated behind `-t advance` (default basic). Also fixed the same day: a proper-cyclic-literal hang (`#0=(1 2 3 . #0#)`) in the fixpoint pass's `collect-define-names`, the only graph walk in `fixpoint.sls` without a visited set.

Key baseline numbers (2026-06, before any optimization):

| Benchmark | Real | Results |
|-----------|------|---------|
| `type:interpret binary-search cl` | **158.7 s** | **21,603** |
| `type:interpret natural-order-compare cl` | 12.9 s | 218 |
| `init-workspace (with ti)` | >600 s (timeout) | — |
| `hover assq-ref (with ti)` | 72.3 s | 0 |

The extreme gap between `binary-search` (21k results, 158s) and `natural-order-compare` (218 results, 13s) shows that the problem is not type inference per se, but the **branching factor** of recursive functions under the current depth limit.

### Attempt 1 — Hash-Set `dedupe` (Success)

**Change:** Rewrite `util/dedupe.sls` to use `make-hashtable` with `equal-hash` / `equal?` when the equality predicate is `equal?`. Custom predicates fall back to the original O(n²) filter.

**Result:**
- `type:interpret binary-search cl`: 158.7 s → **100.0 s** (−37%)
- `init-workspace (no ti)`: 44.1 s → 38.7 s (−12%)

**Lesson:** Deduplication is a real bottleneck when result lists grow to 20k+ items, but it is not the root cause. Even with O(n) dedupe, `binary-search` still takes ~100 s because the interpreter still **generates** 21k results.

### Attempt 2 — Thread `max-depth` Through Internal Calls (Failure)

**Hypothesis:** `type:interpret-result-list` drops `max-depth` in 7 internal recursive call sites, causing all nested expansions to fall back to `PRIVATE-MAX-DEPTH = 10`. Fixing this would cap the explosion.

**Change:** Pass `max-depth` explicitly at lines 224, 231, 238, 258, 268, 278, 285.

**Result:** **No measurable effect.** `binary-search cl` remained at ~100 s with 21,603 results.

**Root cause analysis:** The external call `(type:interpret-result-list target-node)` already uses the default `PRIVATE-MAX-DEPTH = 10`. Internal calls falling back to 10 is therefore a no-op in this scenario. The real problem is that **depth 10 itself is too deep** for a highly branching recursive function.

**Lesson:** Passing `max-depth` is a correct code hygiene fix, but it does not solve the `binary-search` timeout on its own.

### Attempt 3 — Memoization in `type:interpret` (Failure → False Start → Safe but Ineffective)

**Hypothesis:** The same sub-expressions are re-interpreted hundreds of times across cartesian-product branches. Caching `(expression, env) → result-list` should eliminate redundant work.

**First implementation (at `type:interpret` level):** On cache hit, set `env.result-list` directly and skip `private:interpret-core`.

**Result:** **Catastrophic.** Result count ballooned from 21,603 → **32,403**, and runtime increased from 100 s → **181 s**.

**Root cause analysis:** `env` is a mutable record shared across the entire call chain. When an internal cache hit mutated `env.result-list`, outer `fold-left append` calls in `private:interpret-core` collected stale or duplicated values, corrupting the aggregation.

**Second implementation (at `type:interpret-result-list` level):** Cache only the **returned value**, do not mutate `env`. Only `index-node?` expressions are cached (using `make-eq-hashtable` for O(1) identity lookup).

**Result:** Semantically safe — result count stayed at **21,603**. But speedup was negligible (~1%).

**Root cause analysis:** `binary-search` is deeply recursive. After the first encounter of an `index-node`, it is immediately appended to `memory`. Subsequent recursive references to the same node find it in `memory` and are truncated, so the cache condition `(not (contain? memory expression))` fails. Cache hits only occur for top-level expressions referenced from multiple distinct parents *before* they enter `memory` — a rare scenario for recursive functions.

**Lesson:** Memoization is safe when implemented as a value cache outside the mutable `env`, but for recursive functions the `memory`-based truncation mechanism defeats it.

### Attempt 4 — `private:interpret-core` Extraction (Unexpected Side Effect)

**Change:** Refactor `type:interpret` by extracting its body into `private:interpret-core` and simplifying the `case-lambda` fallback chain so that 1-arity and 2-arity calls reach the 4-arity body directly instead of hopping through intermediate arities.

**Result:** No semantic change, but `type:interpret binary-search cl` dropped from ~100 s → **82.8 s** and the full benchmark suite completed for the first time (previously it timed out at 600 s because earlier cases consumed too much time).

**Lesson:** The improvement is likely attributable to reduced call-stack overhead and/or better compiler optimisation from smaller functions, not a fundamental algorithmic win.

### Synthesis — What Actually Works

Based on the above attempts, the following interventions are known to have real, measured impact:

| Intervention | Status | Verified Impact |
|--------------|--------|-----------------|
| Hash-set `dedupe` | ✅ Merged | −37% on `binary-search cl` |
| Simplify `case-lambda` fallbacks | ✅ Merged (as part of memoization revert) | ~−15–20% on `binary-search cl` |
| Thread `max-depth` | ❌ Reverted | None (default depth = 10 anyway) |
| Memoization (`type:interpret`) | ❌ Reverted | Negative (pollutes shared `env`) |
| Memoization (`type:interpret-result-list`) | ❌ Reverted | ~0% (blocked by `memory` truncation) |

The remaining bottleneck is the **depth-10 expansion of highly branching recursive functions**. The mitigations and their status:

1. **Lower `PRIVATE-MAX-DEPTH`** — not taken; kept at 10.
2. **Result-size budget** — ✅ **implemented** (`PRIVATE-MAX-RESULTS = 500` in `interpreter.sls`; the 2026-06 threshold-tuning data that motivated it was retired with the old evaluation report).
3. **Cache `type:partially-solved?`** — the cartesian-product pruner calls this predicate for every element on every tier; memoizing it would avoid repeated tree traversals.
4. **Global visited set** — replace (or augment) chain-based `memory` with a per-top-level-call hash set so that cross-function recursion (`binary-search` ↔ `private-collect`) is detected regardless of call path.

---

## 11. File Map

| File | Responsibility |
|------|----------------|
| `analysis/type/substitutions/generator.sls` | `construct-substitutions-for`, `step` dispatcher, rule routing. |
| `analysis/type/substitutions/util.sls` | `construct-lambdas-with`, `construct-parameter-index-nodes-products-with`. |
| `analysis/type/substitutions/rnrs-meta-rules.sls` | Large alist of primitive type signatures. |
| `analysis/type/substitutions/rules/trivial.sls` | Literals, symbols, implicit conversion (Gradual Typing). |
| `analysis/type/substitutions/rules/application.sls` | Vanilla function application. |
| `analysis/type/substitutions/rules/lambda.sls` | Lambda abstraction types. |
| `analysis/type/substitutions/rules/let.sls` | `let`, `let*`, named `let`. |
| `analysis/type/substitutions/rules/let*.sls` | `let*`, `letrec`, `letrec*` (named-let dispatch unified here; standalone `letrec.sls` removed). |
| `analysis/type/substitutions/rules/if.sls` | Conditional union types. |
| `analysis/type/substitutions/rules/define.sls` | `define` for variables and procedures. |
| `analysis/type/substitutions/rules/case-lambda.sls` | Overloaded arities. |
| `analysis/type/substitutions/rules/record.sls` | `define-record-type` types. |
| `analysis/type/substitutions/rules/cond.sls` | Multi-branch conditional. |
| `analysis/type/substitutions/rules/begin.sls` | Sequencing. |
| `analysis/type/substitutions/rules/unless.sls` | `unless` (negated conditional). |
| `analysis/type/substitutions/rules/define*.sls` / `lambda*.sls` | S7 `define*` / `lambda*`. |
| `analysis/type/substitutions/self-defined-rules/router.sls` | Extensible rule dispatcher (`ufo-try` arm + generic macro arm). |
| `analysis/type/substitutions/macro-expansion.sls` | Expansion-driven macro type inference (`type:expand-and-infer`; §4.11). |
| `analysis/type/domain-specific-language/interpreter.sls` | `type:interpret`, `type:->?`, macro expander, Cartesian-product pruner. |
| `analysis/type/domain-specific-language/inner-type-checker.sls` | Predicates: `inner:trivial?`, `inner:lambda?`, `inner:macro?`, `inner:executable?`, `inner:?->pair`. |
| `analysis/type/domain-specific-language/syntax-candy.sls` | `candy:matchable?`, `candy:match-left`, `candy:match-right`, segment-based DP matcher. |
| `analysis/identifier/meta.sls` | `construct-type-expression-with-meta`, loader for `rnrs-meta-rules`. |

---

### 11.1 Field Notes — `car` Macro & Synthetic-Repro Pitfall (2026-04)

> **Retrospective note:** The original write-up of this section contained an incorrect root-cause analysis. The symptoms described below were real, but they stemmed from a flawed synthetic repro rather than from bugs in `interpreter.sls`. The section has been corrected to reflect what actually happened.

### What Was Observed

A hand-written test (`test-car-macro-bug.sps`) constructed a raw `car` macro application using **bare symbols** for type predicates:

```scheme
(type:interpret-result-list
  `((with ((a b c **1)) (with-equal? inner:list? a b))
    (inner:list? fixnum? string?))
  env)
```

The interpreter returned the macro expression unchanged instead of `fixnum?`.

### The Actual Root Cause

**The synthetic repro was unrealistic.** In real substitution generation, every type predicate (`fixnum?`, `string?`, `number?`, etc.) is wrapped as an `identifier-reference` record by `construct-type-expression-with-meta`. `inner:trivial?` accepts `identifier-reference` objects, so real macro arguments pass the `(? inner:trivial? inputs) **1` guard in `macro-head-execute-with` without issue.

In the synthetic repro, `fixnum?` and `string?` are **bare symbols**. They fail `inner:trivial?`, so `ufo-match` rejects the argument list **before** any macro logic runs. The macro never gets a chance to call `candy:matchable?`.

### What the `syntax-candy.sls` Hardening Actually Fixed

The `syntax-candy.sls` changes made during this investigation did not fix a `car` macro bug — the macro already worked in real code — but they did eliminate several latent risks in the matcher:

| Change | Risk eliminated |
|---|---|
| `private-segment` now rejects templates with **more than one** repeat marker (`...` / `**1`) | Prevents ambiguous DP-matcher states that could silently produce wrong bindings. |
| `candy:match` 2-arity now returns **grouped symbol results** directly | Removes the fragile `fold-left` grouping logic from `candy:match-left`; repeated segments can no longer be split into multiple bindings by accident. |
| `private:group-match-pairs` compares **`segment-type`** instead of `equal?` on record objects | Removes an implicit dependency on Chez Scheme's default record equality, which could break if the `segment` record layout ever changes. |
| `candy:match-right` expands grouped results back to flat `(rest-type . ready-type)` pairs | Keeps backward compatibility with `private-with` in `interpreter.sls`. |

### Verified Status

| Test | Status | Notes |
|------|--------|-------|
| `test-inner-type-checker.sps` — direct `car`/`cdr` macro assertions | ✅ Pass | `car` returns first element type; `cdr` returns tail type. |
| `test-lambda.sps` — parameter-index-node type access | ✅ Pass | Implicitly exercises the same `with`/`with-equal?` path. |
| `test-syntax-candy.sps` | ✅ Pass | No regressions after matcher hardening. |

### Lesson

When debugging macro expansion, always feed the interpreter **real type expressions** (via `construct-type-expression-with-meta` or through the full substitution generator). Hand-constructing bare-symbol repros is fast, but it bypasses the `identifier-reference` wrapping that the rest of the pipeline assumes, producing false negatives that look like macro bugs but are actually data-format mismatches.

## 12. DSL 引擎：Syntax Candy（list-pattern matcher）


> Module: `analysis/type/domain-specific-language/syntax-candy.sls`

This module implements a small **list-pattern matcher** for the type-system DSL. It is the engine that decides whether a parameter template (e.g. `(a b ... c)`) can match a concrete argument list (e.g. `(1 2 3 4)`) and, if so, produces the variable bindings.

It is used heavily by:
- `interpreter.sls` — when applying `inner:lambda?` types to argument lists.
- Macro expansion — when a macro head contains repeated segments (`...` / `**1`).

---

### 1. Core Concepts

#### 1.1 Segment

A **segment** is the atomic unit of a pattern. It is a record with two fields:

| Field | Mutability | Meaning |
|-------|------------|---------|
| `type` | immutable | The literal symbol that appears in the source list (e.g. `number?`, `a`, `inner:list?`). |
| `tail` | mutable | Either `'()` (plain), `'...` (Kleene-star, zero-or-more), or `'**1` (Kleene-plus, one-or-more). |

A list such as `(number? boolean? ...)` is parsed into three segments:

```
#(#(segment number?  ())
  #(segment boolean? ())
  #(segment boolean? ...))
```

Notice that `...` (and `**1`) does **not** create a new segment by itself; it mutates the `tail` of the *preceding* segment. This is why `tail` is mutable and why the parser rejects patterns such as `(...)` or `(a **1 **1)`. 

Additionally, a template may contain **at most one** repeat marker in total. Patterns such as `(a ... b ...)` or `(a **1 b **1)` are rejected because the matcher is designed for a single variable-length segment per list.

#### 1.2 Pattern vs. Argument List

The matcher always works on **two** segment vectors:

- **Parameter template** (`rest-segments`) — the left-hand side, may contain `...` / `**1`.
- **Argument list** (`ready-segments`) — the right-hand side, is usually plain (`tail` = `'()`).

Both sides *may* contain repeats, which makes the algorithm closer to regular-expression intersection than to simple destructuring.

---

### 2. Public API

#### `candy:segmentable? target`

Returns `#t` if `target` (a list) can be successfully parsed into segments, `#f` otherwise.

Parsing fails when:
- `...` or `**1` appears at the very beginning of the list.
- A segment already has a non-empty `tail` and another `...` / `**1` follows it.

```scheme
(candy:segmentable? '(a b ... c))   ;; => #t
(candy:segmentable? '(... a))       ;; => #f
(candy:segmentable? '(a **1 **1)) ;; => #f
```

---

#### `candy:matchable? parameter-template argument-list`

Returns `#t` if the two lists match structurally (ignoring the actual values of the atoms), `#f` otherwise.

```scheme
(candy:matchable? '(a b ... c) '(1 2 3 4 5))
;; => #t   (a=1, b={2 3 4}, c=5)

(candy:matchable? '(a b **1 c) '(1))
;; => #f   (b needs at least one element)
```

Internally this builds a **match matrix** (see §4) and checks whether the bottom-right cell contains `'skipped` (the success sentinel).

---

#### `candy:match parameter-template argument-list`  
#### `candy:match matrix rest-segments ready-segments row-id column-id`

**Two-arity entry point** (public API): parses both lists, builds the matrix, walks it, and returns the **grouped bindings** directly:

```scheme
(candy:match '(a b c) '(1 2 3))
;; => ((a . 1) (b . 2) (c . 3))

(candy:match '(a b ... c) '(1 2 3 4 5))
;; => ((a . 1) (b . (2 3 4)) (c . 5))
```

For plain segments the right-hand side is a single value; for repeated segments (`...` / `**1`) it is a **list** of all consumed argument values.

**Five-arity internal point**: walks the matrix recursively from `(row-id, column-id)` towards the bottom-right, collecting every `(rest-segment . ready-segment)` pair that was `matched` or `skipped`. The two-arity entry point calls this internally and then passes the raw pairs through `private:group-match-pairs` to produce the grouped symbol-level result above.

---

#### `candy:match-left parameter-template argument-list`
#### `candy:match-left match-segment-pairs`

`candy:match-left` is now a thin compatibility wrapper over `candy:match` (two-arity). Its one-arity form accepts raw segment pairs and applies `private:group-match-pairs` to produce the same grouped symbol-level result.

For plain segments the result is a simple pair:

```scheme
'(a . number?)   ;; parameter a matched argument number?
```

For repeated segments (`...` or `**1`) the right-hand side is aggregated into a **list**:

```scheme
'(b . (number? boolean? string?))   ;; b matched three arguments
```

This is the shape consumed by `private-with` in `interpreter.sls` when substituting lambda parameters into the body.

---

#### `candy:match-right match-segment-pairs`

The dual of `candy:match-left`. Maps each matched pair to a simple `(rest-type . ready-type)` pair:

```scheme
'((a . 1) (b . 2) (b . 3) (c . 4) ...)
```

Unlike `candy:match-left`, it does **not** group repeated segments; the same `rest-type` may appear multiple times. It is used less frequently, mainly for symmetry and debugging.

> **Warning**: `match-right` is **unsafe** when consumed by `private-with` (the substitution engine in `interpreter.sls`). Because `private-with` iterates with `fold-left` + `private-substitute`, the first `(rest-type . _)` pair shadows all subsequent ones, losing information. If the matched argument itself contains `**1` or `...` (e.g. a higher-order function signature like `map`), `match-right` can incorrectly fragment a binding and cause type collapse. See `doc/analysis/type.md` §6.7 Trap 3.

---

### 3. Record Type — `segment`

```scheme
(define-record-type segment
  (fields
    (immutable type)
    (mutable tail)))
```

| Accessor | Setter |
|----------|--------|
| `segment-type` | — |
| `segment-tail` | `segment-tail-set!` |

---

### 4. Algorithm — Match Matrix

The matcher is essentially a **dynamic-programming** implementation of regular-expression-like list matching.

#### 4.1 Matrix Shape

Given `m` rest-segments and `n` ready-segments, the matrix has `(m+1) × (n+1)` cells. Row 0 and column 0 are padding borders.

#### 4.2 Cell States

| State | Meaning |
|-------|---------|
| `matched` | The cell at `(row, col)` represents a valid pairing between `rest-segments[row-1]` and `ready-segments[col-1]`. |
| `skipped` | The cell is reachable but does **not** represent a pairing; it is a transition state (used for borders and repeat propagation). |
| `unused` | The cell is unreachable for the current path. |

#### 4.3 Allowed Moves

- **Right →** — consume one argument segment without consuming a parameter segment (only valid when the parameter has a repeat tail).
- **Down ↓** — consume one parameter segment without consuming an argument segment (only valid when the argument has a repeat tail).
- **No diagonal ↘** — a cell is never reached by consuming both sides at once. The diagonal relationship is encoded by adjacent `matched` cells.

#### 4.4 Building the Matrix (`private-segments->match-matrix`)

The builder is a recursive backtracker:

1. Start at `(0, 0)` and mark it `skipped`.
2. At each cell try to move **right** and/or **down**.
3. If a move leads to a successful path (i.e. eventually reaches `(m, n)` with valid end-zone conditions), mark the current cell with the requested status.
4. If neither move succeeds, leave the cell `unused`.

#### 4.5 End-Zone Rules (cell `(m, n)`)

The bottom-right cell is the acceptance state. It is marked `matched` when **any** of the following holds:

- It is immediately preceded by a `skipped` cell (row or column).
- The last parameter segment or last argument segment has tail `...` / `**1`.
- Either side ends with `...`.

If the matrix builder leaves `(m, n)` as `unused`, the two lists do not match.

#### 4.6 Backtracking (`candy:match` five-arity)

After the matrix is built, `candy:match` walks from `(0, 0)` to `(m, n)` recursively:

- `matched` cell → emit the corresponding segment pair, then recurse **right** and **down**.
- `skipped` cell → recurse **right** and **down** without emitting.
- `unused` cell → stop.

Because repeats allow both right and down moves from the same cell, the recursion naturally fans out to cover all possible consumptions.

---

### 5. Examples

#### 5.1 Simple destructuring

```scheme
(candy:match-left
  (candy:match '(a b c) '(1 2 3)))
;; => ((a . 1) (b . 2) (c . 3))
```

#### 5.2 Kleene-star (`...`)

```scheme
(candy:match-left
  (candy:match '(a b ... c) '(1 2 3 4 5)))
;; => ((a . 1) (b . (2 3 4)) (c . 5))
```

#### 5.3 Kleene-plus (`**1`)

```scheme
(candy:match-left
  (candy:match '(a b **1 c) '(1 2 3 4 5)))
;; => ((a . 1) (b . (2 3 4)) (c . 5))

(candy:matchable? '(a b **1 c) '(1 2))
;; => #f   (b needs ≥1 element)
```

#### 5.4 Both sides have repeats

`syntax-candy` does **not** support multiple repeat segments in a single template. Patterns such as `(a ... b ...)` are rejected by `private-segment`. The matcher is designed for at most one variable-length segment per list.

---

### 6. Relationship to the Type System

`syntax-candy` is **not** a general-purpose pattern matcher. It is specialised for the shapes that appear in type expressions:

- Lambda parameter lists: `(inner:list? number? boolean? ...)`.
- Macro templates: `(with ((a b c **1)) …)`.

The interpreter calls `candy:matchable?` before attempting substitution. If the shapes do not align, the lambda/macro application returns `'()` (no result). If they do align, `candy:match-left` produces the binding environment that `private-with` uses to rewrite the body.

Because the matcher operates on **segment vectors** rather than raw lists, the cost is `O(m × n)` in the number of segments, independent of how many concrete values a `...` segment ends up consuming.

---

### 7. Common Traps

1. **`...` / `**1` must follow a non-repeat segment, and a template may contain at most one repeat marker in total.**  
   `(a **1 **1)` is rejected because `a` would already carry `**1` and cannot receive another. `(a **1 b **1)` and `(a ... b ...)` are also rejected because the matcher only supports a single variable-length segment per list. This is caught by `private-segment` raising `"wrong rule"`.

2. **Empty repeats.**  
   `(a b **1)` cannot match `(a)` — `b` needs at least one item. Use `...` when zero items are acceptable.

3. **The matrix is rebuilt on every call.**  
   There is no memoisation. In tight loops (e.g. deep macro expansion inside the interpreter) the same parameter template may be re-segmented many times. This has not been a bottleneck in practice because type-level lists are short (< 20 elements).

---

### 8. Architecture Context — Why `syntax-candy` Exists

`syntax-candy` is not an optional utility; it is a **load-bearing wall** between Phase I (substitution generation) and Phase II (interpretation). To understand its bugs and optimisation potential, it helps to see exactly where the data flows.

#### 8.1 The Data Flow

```
Source Code
    │
    ▼
┌─────────────────────────────────────────┐
│  Phase I: Substitution Generation       │
│  (substitutions/generator.sls)          │
│                                         │
│  • `lambda-process` builds a type like  │
│    `(number? <- (inner:list? a b ...))` │
│  • `trivial-process` builds macro       │
│    substitutions such as `((with …) +)` │
└─────────────────────────────────────────┘
    │
    ▼
Raw type expressions (S-expressions with symbols,
index-nodes, identifier-references, and DSL keywords)
    │
    ▼
┌─────────────────────────────────────────┐
│  Phase II: DSL Interpreter              │
│  (interpreter.sls)                      │
│                                         │
│  1. `type:interpret` sees               │
│     `(inner:executable? fn args)`       │
│  2. If `fn` is `inner:lambda?`          │
│     → β-reduction                       │
│  3. If `fn` is `inner:macro?`           │
│     → macro expansion (`execute-macro`) │
└─────────────────────────────────────────┘
    │
    ▼
`syntax-candy` is invoked here ───────┐
                                      │
  • `candy:matchable?` checks shape   │
  • `candy:match-left` produces       │
    bindings for `private-with`       │
                                      │
    e.g.  template `(a b **1)`        │
          argument `(d e f)`          │
          ───────►  `((a . d)         │
                     (b . (e f)))`    │
                                      │
`private-with` rewrites the macro     │
body, then `type:interpret` continues │
```

#### 8.2 Why Not Ordinary `match` or `destructure`?

Scheme already has excellent pattern matchers (`match`, `ufo-match`). Why invent another one?

The answer lies in the **heterogeneous, variable-length** nature of type-level lists:

```scheme
;; A function that accepts any number of number? arguments
(number? <- (inner:list? number? ...))

;; A macro that wants the *first* element of a list of unknown length
(car (with ((a b c **1)) (with-equal? inner:list? a b)))
```

 Ordinary `match` can destructure fixed-length lists, or homogeneous repeated lists (`(a ...)`), but it struggles when **both sides** may contain repeats, or when a repeat must be aligned against a non-repeat on the other side. The type system needs to ask questions such as:

- "Does `(inner:list? number? ...)` match `(inner:list? fixnum? boolean? string?)`?"
- "If so, what is the binding of `...`?"

These are **regular-expression-intersection** problems, not simple destructuring problems. `syntax-candy` solves them with a small DP matrix that is O(m × n) in the number of segments, regardless of how many concrete items a `...` consumes.

#### 8.3 The Critical Invariant

Every successful macro or lambda application in the interpreter depends on the following invariant:

> **`candy:match` (and therefore `candy:match-left`) returns *exactly one* binding entry per distinct parameter segment, and the right-hand side of a repeated segment contains *each matched argument exactly once*, in order.**

`private:group-match-pairs` enforces this invariant explicitly: even if the raw matrix walk were to emit duplicate pairs in future, the fold-left grouper collapses them by `segment-type` before any substitution reaches `private-with`. The interpreter silently continues with a bloated or corrupted substitution list, and the Cartesian-product pruner downstream has to deal with the mess.

---

### 9. Known Bugs & Their Systemic Impact

#### 9.1 Note — Why `walk` Does *Not* Produce Duplicates in Practice

##### Where

`candy:match` five-arity, lines 84–97.

##### Observation

`walk` recurses in **both** directions from every `matched` or `skipped` cell and `append`s the results:

```scheme
[(equal? current-value 'matched)
  (cons (cons (vector-ref rest-segments (- r 1))
              (vector-ref ready-segments (- c 1)))
        (append (walk (+ r 1) c) (walk r (+ c 1))))]
[(equal? current-value 'skipped)
  (append (walk (+ r 1) c) (walk r (+ c 1)))]
```

At first glance this looks like it would emit duplicate pairs when a repeated segment has multiple valid consumptions (e.g. template `(a ...)` vs. argument `(1 2)`). However, the **matrix builder** (`private-next-step-ok?`) effectively enforces a single canonical path:

1. At every cell, `private-next-step-ok?` tries **right first**, and if that succeeds it **never tries down**.
2. In the start zone, `(0, n)` tries **down first**, and if that succeeds it **never tries right**.

Consequently the matrix contains at most one outgoing edge per cell, turning the walk into a simple linear traversal rather than a DAG enumeration. Running `candy:match '(a ...)' '(1 2)'` returns exactly two distinct pairs `((a . 1) (a . 2))`, with no duplication.

##### Why This Is Still Fragile

The absence of duplicates is an **emergent property** of the current `right-first` / `down-first` heuristic, not a guaranteed invariant of the algorithm. If the exploration order in `private-next-step-ok?` ever changes (or if a future optimisation adds a second valid edge to a cell), `walk` will silently start emitting duplicates. A safer design would be:

- **Option A**: Make `walk` a depth-first search that returns as soon as it reaches `(m, n)`, guaranteeing exactly one path.
- **Option B**: Keep the all-paths collector, but deduplicate the result list by `equal?` on `(rest-segment, ready-segment)` pairs before returning.

Either choice makes the no-duplication guarantee explicit rather than accidental.

---

#### 9.2 Bug B — `candy:match-left` Compared Whole `segment` Records with `equal?` ✅ Fixed

##### Where

`private:group-match-pairs` (called by `candy:match-left`), previously line 64:

```scheme
(if (equal? (car last-pair) (car match-segment-pair))
```

##### Root Cause

Both operands were `segment` record objects. Chez Scheme's `equal?` on records compares every field recursively. While `segment` had only two fields (`type` and `tail`), this happened to work. But it created a **fragile implicit dependency** on the record layout:

- If a third field is added, `equal?` could suddenly return `#f` for segments that represent the same pattern variable.
- Because `tail` is mutable, two segments that were `equal?` at one moment could cease to be `equal?` later, causing non-deterministic grouping failures.

##### Systemic Impact

When `equal?` unexpectedly returned `#f`, the grouper treated two occurrences of the same repeat segment as **different** variables. Instead of producing:

```scheme
'(b . (number? boolean? string?))
```

it produced:

```scheme
'(b . (number?)) (b . (boolean?)) (b . (string?))
```

`private-with` then saw three separate bindings for `b`, and only the last one won. The macro body ended up with a truncated or incorrect substitution.

##### Fix Applied

`private:group-match-pairs` now compares the **semantic identity** of the segment via `segment-type`:

```scheme
(if (equal? (segment-type (car last-pair))
            (segment-type (car match-segment-pair)))
```

`segment-type` is immutable and represents the actual pattern variable symbol. It is the correct key for grouping, and the fix removes the implicit dependency on the full record layout.

---

#### 9.3 Bug C — `private-segment` Raises a String, Not a Condition

##### Where

`private-segment`, lines 300 and 306:

```scheme
(raise "wrong rule")
```

##### Root Cause

Chez Scheme allows `raise` with any value. `candy:segmentable?` already wraps `private-segment` in `ufo-try` and safely swallows the exception:

```scheme
(try
  (private-segment target)
  #t
  (except c 
    (else #f)))
```

However, `candy:matchable?` and `candy:match` call `private-segment` **directly**, without any `try`/`except`. If a caller (e.g. a user-defined rule in `self-defined-rules`) passes a malformed template such as `'(... a)` directly to `candy:matchable?`, the string exception propagates until it hits whatever handler happens to be on the stack. That handler may or may not catch a string; if it does not, the entire `type:interpret` call chain aborts.

##### Systemic Impact

Because type inference runs on **every** document in the workspace, one malformed library can crash hover information for the entire project. The crash is not guaranteed — it depends on whether the calling code uses `ufo-try` — but it is possible.

##### Fix Direction

Raise a symbol or a proper condition:

```scheme
(raise 'invalid-macro-template)
;; or
(raise (make-condition 'invalid-macro-template))
```

Alternatively, add a `try`/`except` guard inside `candy:matchable?` and `candy:match` so that illegal templates are treated as non-matching (`#f` or `'()`) rather than fatal errors.

---

#### 9.4 Bug D — `private-next-step-ok?` Performs Incomplete Backtracking

##### Where

`private-next-step-ok?`, lines 259–282.

##### Root Cause

The function writes into the matrix, recurses, and then — if the recursive exploration fails — resets **only** the current cell to `unused`:

```scheme
(matrix-set! matrix cols row-id column-id status)
(private-segments->match-matrix matrix rest-segments ready-segments row-id (+ column-id 1))
(if (equal? 'unused (matrix-take matrix cols row-id (+ column-id 1)))
  (matrix-set! matrix cols row-id column-id 'unused))
```

The recursive call may have written into cells far beyond the immediate neighbour. Those writes are **never undone**.

##### Systemic Impact

Because `candy:matchable?` only checks cell `(0,0)`, this pollution is usually benign. However, in matrices where a failed rightward exploration writes `skipped`/`matched` marks deep into the grid, a subsequent downward exploration from the same parent may encounter those stale marks and treat them as valid path continuations. This can cause `candy:matchable?` to return `#t` for shapes that should not match, or `candy:match` to collect spurious segment pairs.

In the type system this manifests as **occasional false-positive macro matches**: a macro template matches an argument list that it should reject, and `private-with` substitutes nonsense values into the body. The resulting type is wrong but syntactically valid, so it propagates silently.

##### Fix Direction

Two options:

1. **Functional matrix**: Instead of mutating a single vector, `private-next-step-ok?` returns a new vector on success and `#f` on failure. No backtracking needed because each path has its own copy.
2. **Snapshot & restore**: Before recursing, save the current matrix row (or the whole matrix, if small) and restore it on failure.

Given that type-level segment counts are tiny (< 20), option 1 (functional) is clean and eliminates an entire class of state-bugs at negligible cost.

---

### 10. Optimisation Opportunities

#### 10.1 Cache `private-segment` Results

**Observation**: The same parameter template is re-segmented on every macro invocation. For example, the `car` macro template `(a b c **1)` is parsed hundreds of times when `init-references` runs over a medium-sized project.

**Opportunity**: Add an `eq?`-based memoisation table:

```scheme
(define private-segment-cache (make-eq-hashtable))

(define (private-segment rule-list)
  (or (hashtable-ref private-segment-cache rule-list #f)
      (let ([result (list->vector (reverse (fold-left ... '() rule-list)))])
        (hashtable-set! private-segment-cache rule-list result)
        result)))
```

`rule-list` is almost always a quoted literal in the source code, so `eq?` hits are frequent. Benchmarking is needed, but for 1100+ `rnrs-meta-rules` signatures this could shave milliseconds off every workspace init.

#### 10.2 Avoid Double Parsing in `candy:matchable?` → `candy:match` Chains

**Observation**: The interpreter often calls `candy:matchable?` first, and if it returns `#t`, immediately calls `candy:match`. Each call re-parses both lists and rebuilds the matrix.

**Opportunity**: Provide a combined entry point:

```scheme
(define (candy:match-if-possible parameter-template argument-list)
  (let ([rest (private-segment parameter-template)]
        [ready (private-segment argument-list)])
    (and (candy:matchable?/presegmented rest ready)
         (candy:match/presegmented rest ready))))
```

This halves the segment-parsing work in the common case.

#### 10.3 Replace `walk`’s `append` with a Tail-Recursive Accumulator

**Observation**: `walk` uses `(append left right)` at every branch. For a template with `k` repeated segments, the total number of emitted pairs is `O(n^k)` in the worst case, and each `append` is `O(length)`.

**Opportunity**: Rewrite `walk` as a tail-recursive function that takes an accumulator list and `cons`es results onto it. This does not affect correctness (the current builder guarantees a single path), but it removes the quadratic `append` penalty.

#### 10.4 Use a Fixed-size Functional Matrix Instead of a Mutable Vector

**Observation**: The current matrix is a flat mutable vector allocated with:

```scheme
(make-vector (* (+ 1 m) (+ 1 n)) 'unused)
```

For typical `m, n < 10`, this vector has < 121 cells. Allocation and mutation overhead dominate.

**Opportunity**: Represent the matrix as a nested list or a small fixed-size record (e.g. 5×5, 10×10, 20×20 tiers) and build it functionally. Chez Scheme’s generational GC is very fast for short-lived small objects; avoiding `matrix-set!` / `matrix-take` indirection may actually be faster.

---

### 11. Synthesis — How Bugs in `syntax-candy` Ripple Outward

`syntax-candy` sits at a narrow but critical chokepoint. Every lambda application and every macro expansion in the type interpreter passes through it. Because the rest of the pipeline (Cartesian product, recursive interpretation, deduplication) is designed to be **robust to noise**, the symptoms of `syntax-candy` bugs are rarely crashes. Instead they are:

| Symptom in the type system | Likely `syntax-candy` root cause |
|---|---|
| A parameter’s type is unexpectedly `something?` after macro expansion | Bug B or D — binding was lost or mis-grouped |
| `init-workspace` aborts with unhandled exception | Bug C — malformed template raises string |

This makes `syntax-candy` a high-leverage target for hardening: a small, well-tested module with ~300 lines of code that, if fixed, improves both correctness and performance across the entire type system.

## 13. Record 类型推断


> 本文分析 `define-record-type` 在 type inference 子系统中的设计意图、实现冲突及改进方向。

---

### 1. 涉及的核心文件与职责

| 文件 | 职责 |
|------|------|
| `analysis/identifier/rules/define-record-type.sls` | Identifier 规则阶段：解析 `(define-record-type point ...)`，创建 `point`(syntax)、`make-point`(constructor)、`point?`(predicator)、`point-x`(getter)、`point-x-set!`(setter) 等 `identifier-reference`，并设置 `export-to-other-node` |
| `analysis/type/substitutions/rules/record.sls` | Type 规则阶段：遍历 `define-record-type` 的 index-node，收集其下的 identifier-reference，为 getter/setter/constructor 设置 `type-expressions` |
| `analysis/type/substitutions/rules/trivial.sls` | 兜底规则：对 usage site 的 identifier 生成 substitution。对 predicator **硬编码** `(boolean? <- (inner:list? something?))` |
| `analysis/type/domain-specific-language/interpreter.sls` | 类型解释器：解释 substitution / type-expression，生成最终类型字符串 |
| `tests/analysis/type/substitutions/rules/test-record-comprehensive.sps` | 综合测试，覆盖 record type inference 的各条路径 |

---

### 2. 设计意图

scheme-langserver 的 type inference 采用 **substitution + 解释器** 架构：

1. **Identifier 阶段**：`define-record-type.sls` 把 record 定义解析成一组 `identifier-reference`，每个 reference 带有 `type` 字段（`constructor` / `getter` / `setter` / `predicator` / `syntax`）。
2. **Type 阶段**：`record.sls` 在 AST 上找到 `(define-record-type ...)` 节点，收集其下的 identifier-reference，然后为它们设置 `type-expressions`。
3. **Usage 阶段**：`trivial.sls` 在代码使用点（如 `(make-point 1 2)` 或 `(point-x p)`）找到对应的 identifier-reference，根据其 `type` 决定如何生成 substitution。
4. **解释阶段**：`interpreter.sls` 把 substitution 解释成最终类型。

**理想的数据流**：

```
(define-record-type point (fields (mutable x)))
  ↓ identifier 阶段
创建 make-point(constructor), point?(predicator), point-x(getter), point-x-set!(setter)
  ↓ type 阶段
设置 type-expressions:
  make-point: (point? <- (inner:list? something? ...))
  point?:     (boolean? <- (inner:list? something?))
  point-x:    (something? <- (inner:list? point?))
  point-x-set!:(inner:void? <- (inner:list? point? something?))
  ↓ usage 阶段
(point-x p) 的 usage node 的 substitution = point-x 的 identifier-reference
  ↓ interpreter 阶段
解释 identifier-reference → 读取其 type-expressions →
输出 "(something? <- (inner:list? point?))"
```

---

### 3. 设计 vs 实现：六大冲突

#### 冲突一：Predicator 的「双源」问题——谁该决定它的类型？

**设计意图**：`record.sls` 统一负责所有 record identifier 的 type-expressions。

**现实**：

`record.sls:41-46` 中 predicator 的 `type-expressions` 被**显式注释掉**：

```scheme
;; Predicator type-expressions are handled by trivial.sls (hard-coded).
;; Keeping it here risks inconsistency because trivial.sls and record.sls
;; may run in different orders depending on Scheme library semantics.
;; (identifier-reference-type-expressions-set! 
;;   predicator
;;   `((,(construct-type-expression-with-meta 'boolean?) <- (inner:list? something?))))
```

原因：开发者担心 `record.sls` 和 `trivial.sls` 的执行顺序不确定。如果 `trivial.sls` 先执行，它会根据 predicator 的 `type-expressions` 生成 substitution；如果 `record.sls` 后执行并修改 `type-expressions`，前面的 substitution 就过期了。

但 `trivial.sls:98-99` 对 predicator 的处理是**硬编码**：

```scheme
[(and (equal? 'predicator type) (not (null? target-index-node)))
  (extend-index-node-substitution-list index-node `(,private-boolean? <- (inner:list? something?)))]
```

这造成了**类型来源的分裂**：
- constructor / getter / setter 的类型来自 `record.sls` 设置的 `type-expressions`
- predicator 的类型来自 `trivial.sls` 的硬编码，与 `identifier-reference-type-expressions` 完全无关

测试 `test-record-comprehensive.sps` 的 Group B/C 明确验证了这一分裂：
- `point?` 的 usage node substitution 是一个**函数类型列表**（`(boolean? <- ...)`），而非 `identifier-reference`
- 而 `make-point` 的 usage node substitution 是 `identifier-reference` 对象本身

**后果**：
1. 如果以后想给 predicator 增加更精确的类型（如 `(boolean? <- (inner:list? point?))` 而非 `(boolean? <- (inner:list? something?))`），修改点分散在 `trivial.sls` 中，无法利用 `record.sls` 已经知道的 predicator 信息。
2. `identifier-reference-type-expressions` 对 predicator 永远是空的，破坏了「每个 identifier-reference 都携带自己的类型信息」这一设计假设。

---

#### 冲突二：Constructor 返回类型中嵌入了 Identifier-Reference 对象——类型 AST 被运行时对象污染

**设计意图**：type-expressions 应该是纯粹的类型 AST，使用自定义 DSL 表示。

**现实**：

`record.sls:47-49` 设置 constructor 的 type-expression：

```scheme
(identifier-reference-type-expressions-set! 
  constructor 
  `((,predicator <- (inner:list? something? ...))))
```

这里 `predicator` 是一个 **`identifier-reference` 对象**（由 `find` 从 collection 中取出），被直接嵌入到 type-expression 的 quasiquote 中。

`interpreter.sls:287-293` 处理 `identifier-reference?` 的逻辑：

```scheme
[(identifier-reference? expression)
  (cond 
    [(and (not (null? (identifier-reference-type-expressions expression))) 
          (contain? '(constructor getter setter) (identifier-reference-type expression)))
      (type:environment-result-list-set! env (identifier-reference-type-expressions expression))]
    [else (type:environment-result-list-set! env `(,expression))])]
```

**关键问题**：当 interpreter 在解释 constructor 的 type-expression `((,predicator <- ...))` 时，它会递归解释内部的 `predicator`（一个 identifier-reference）。由于 `predicator` 的 `type` 是 `'predicator`，不在 `(constructor getter setter)` 列表中，所以走 `else` 分支——**直接返回 `identifier-reference` 对象本身**。

这导致 hover 输出中 constructor 的返回类型显示为：

```
([identifier-reference point?] <- (inner:list? something? ...))
```

而不是期望的：

```
(point? <- (inner:list? something? ...))
```

测试 `test-record-comprehensive.sps` Group D（L200-207）明确验证了这一现象：

```scheme
(test-assert "constructor return type is identifier-reference (predicator)"
  (identifier-reference? (car type-expr)))
(test-equal "constructor return type identifier is point?"
  'point? (identifier-reference-identifier (car type-expr)))
```

**后果**：
1. 类型 AST 不再是纯粹的 DSL，而是混合了 `identifier-reference` 和 `index-node` 等运行时对象。
2. `inner-type-checker.sls` 和 `interpreter.sls` 被迫处理这些运行时对象，增加了复杂度。
3. hover 输出中出现 `[identifier-reference point?]` 这种对终端用户不友好的字符串。

---

#### 冲突三：Record 继承（Parent）在 Type 侧完全缺失

**设计意图**：R6RS 的 `define-record-type` 支持 `(parent parent-name)`，允许 record 继承父 record 的字段。

**现实**：

`define-record-type.sls:43-67` 有完整的 parent 处理逻辑：
- 解析 `(parent parent-name)`
- 查找父 record 的 `define-record-type` 节点
- 遍历父 record 的 fields，为当前 record 生成继承的 getter/setter

但 `record.sls` 完全没有处理 parent：

```scheme
(define (define-record-type-process document index-node)
  (match expression
    [(_ dummy0 dummy1 ...) 
      (let ([collection (private-collect-identifiers index-node)])
        ...)]))
```

`private-collect-identifiers` 只是递归收集 `index-node-references-export-to-other-node`，如果 parent 的 fields 已经被正确地 export 到当前节点，理论上可以收集到。但 `record.sls` 假设 `predicator` 和 `constructor` 都存在于 `collection` 中，而 parent 场景下这些 identifier 的分布更复杂。

更重要的是，即使收集到了 parent 的 getter/setter，它们的 type-expression 中嵌入的 `predicator` 应该是**父 record 的 predicator**，而非当前 record 的 predicator。`record.sls` 对此没有任何区分。

**后果**：带 `(parent ...)` 的 record 定义，其继承字段的 type inference 结果不正确或缺失。

---

#### 冲突四：执行顺序依赖导致「时而工作、时而失败」

**设计意图**：type inference 应该是确定性的，不受 Scheme 库加载顺序影响。

**现实**：

`record.sls` 依赖 identifier 阶段已经创建并 export 了所有 identifier-reference。它通过 `private-collect-identifiers` 遍历 `index-node-references-export-to-other-node` 来收集这些 reference。

但 `export-to-other-node` 的设置是在 `define-record-type.sls` 中完成的，而 `record.sls` 的调用是在 `generator.sls` 的 `step` 中。`step` 的调用时机取决于：
1. `init-references` 的完成时机
2. `construct-substitutions-for` 的调用时机
3. 各 `.sls` 文件的加载顺序（Scheme 库的 `import` 解析顺序）

`record.sls` 的注释明确承认了这一风险：

> "trivial.sls and record.sls may run in different orders depending on Scheme library semantics"

`test-record-comprehensive.sps` 的 Group F（L235-252）甚至发现了一个相关异常：

```scheme
(let ([idx-node (identifier-reference-index-node make-point-ref)])
  (display (if (index-node? idx-node) "index-node"
             (if (annotation? idx-node) "annotation"
               (if (null? idx-node) "null" "other")))))
```

`identifier-reference-index-node` 有时返回 `annotation` 对象而非 `index-node`。这说明 identifier 创建和 type 规则之间的数据一致性存在问题。

**后果**：
1. 同样的代码在不同的环境（Chez 版本、加载顺序）下可能产生不同的 type inference 结果。
2. 测试可能在某些机器上通过，在另一些机器上失败。

---

#### 冲突五：Getter/Setter 的类型签名过于粗糙

**设计意图**：getter 返回字段类型，setter 接收 record 实例和新值。

**现实**：

`record.sls` 为所有 getter 设置的 type-expression 是：

```scheme
(something? <- (inner:list? ,predicator))
```

为所有 setter 设置的是：

```scheme
(inner:void? <- (inner:list? ,predicator something?))
```

这里有两个问题：

1. **getter 返回类型永远是 `something?`**：如果 record 定义了 `(fields (mutable x))`，`point-x` 的返回类型理论上应该是 `x` 的初始化类型。但当前实现完全丢失了这个信息，一律用 `something?`（万能类型）。

2. **setter 的新值类型也是 `something?`**：没有对 mutable field 的类型做任何约束。

对比 lambda 的类型推断（参数类型可以从调用点推导），record 的字段类型在定义时就已确定（从 `make-point` 的调用参数），但系统没有建立这种联系。

**后果**：
1. Record 的 getter/setter 类型推断对用户几乎没有价值——`something?` 等于「我不知道」。
   - **注**：`interpreter.sls` 的 `private-substitute-index-node&macro` 曾把 `(something? <- (inner:list? point?))` 错误折叠为裸的 `something?`，已修复（见下方「已修复的 Interpreter Bug」）。现在 hover 至少会显示完整的函数签名。
2. 无法检测 `(point-x-set! p "hello")` 是否类型正确（如果 `x` 原本是 `number?`）。

---

#### 已修复：Interpreter 二次解释时 Lambda 类型表达式被错误折叠

**问题**：

`type:interpret->strings` 在格式化类型结果前，会先调用 `private-substitute-index-node&macro` 做预处理。原实现中 `[(list? target) ...]` 分支包含一个短路逻辑：

```scheme
(let ([tmp (map private-substitute-index-node&macro target)])
  (if (equal? 'something? (car tmp))
    'something?
    tmp))
```

当目标是一个 lambda 类型表达式 `(something? <- (inner:list? predicator))` 时，递归 map 后 `car tmp` 恰好是 `something?`，导致**整个 lambda 被折叠成裸的 `'something?`**。随后 `type:interpret-result-list` 再次解释时只看到一个 symbol，最终 hover 输出就是光秃秃的 `something?`，用户完全不知道自己 hover 的是一个函数。

**修复**（`interpreter.sls:58`）：

在 `private-substitute-index-node&macro` 中增加 `[(inner:lambda? target) (map private-substitute-index-node&macro target)]` 分支，匹配优先级高于 `list?`。lambda 类型表达式从此跳过 `(equal? 'something? (car tmp))` 的短路检查，保留完整的 `(return-type <- param-list)` 结构。

**效果**：
- hover record getter 时，从显示 `something?` 变为显示 `(something? <- (inner:list? [identifier-reference point?]))`
- 该修复对所有 return type 为 `something?` 的 lambda 类型表达式均有效，不限于 record

---

#### 冲突六：Type-Expressions 的更新不是原子的——Mutable 状态导致数据竞争

**设计意图**：`identifier-reference` 的 `type-expressions` 字段一旦设置就不变。

**现实**：

`record.sls` 使用 `identifier-reference-type-expressions-set!` 直接修改 mutable 字段：

```scheme
(identifier-reference-type-expressions-set! getter `((something? <- (inner:list? ,predicator))))
```

在多线程模式下（`threaded?=#t`），`init-references` 和 `construct-substitutions-for` 可能并行执行。虽然 `workspace.sls` 有 `mutex`，但 `identifier-reference` 对象本身没有锁。

如果多个文档同时定义了同名的 record type（比如两个库都定义了 `point`），它们的 type-expressions 设置可能互相覆盖。

更现实的问题是：**增量更新**。当用户修改文件中的 `define-record-type`（比如增加一个字段），`refresh-workspace-for` 会重新运行 `construct-substitutions-for`。但旧的 `identifier-reference` 对象可能仍然存在于其他文档的 cache 中，其 `type-expressions` 已经是过时的。

**后果**：
1. 多线程或增量更新场景下，type inference 结果可能不稳定。
2. 无法安全地缓存 `identifier-reference` 的 type-expressions。

---

### 4. 改进方案

#### 方案 A：统一 Predicator 来源，消除双源（短期）

**改动**：
1. 取消 `record.sls` 中对 predicator 的注释，恢复设置 `type-expressions`。
2. 修改 `trivial.sls`，移除对 predicator 的硬编码逻辑，统一走 `identifier-reference-type-expressions`。
3. 在 `workspace.sls` 中确保 `construct-substitutions-for` 在 `init-references` **完全结束后**再调用。

**好处**：消除类型来源分裂，predicator 也能返回精确的 `(boolean? <- (inner:list? point?))`。

**风险**：需要验证执行顺序在所有 Scheme 实现中都稳定。

---

#### 方案 B：Type-Expressions 中不再嵌入 Identifier-Reference 对象（中期）

**改动**：
1. Constructor 的返回类型从 `((,predicator <- ...))` 改为 `((point? <- ...))`——用**纯符号**代替 `identifier-reference`。
2. `interpreter.sls` 在解释类型表达式中的符号时，在环境中查找其对应类型（类似当前查找 `rnrs-meta-rules` 的方式）。
3. 引入 `type:environment` 的符号表，记录 `point?` → `boolean?` 的映射。

**好处**：
- 类型 AST 保持纯粹，不再混入运行时对象。
- hover 输出干净，用户看到 `point?` 而非 `[identifier-reference point?]`。
- 更容易序列化和缓存 type-expressions。

---

#### 方案 C：引入结构化 Record Type DSL（中期）

**改动**：
1. 新增 `inner:record?` 类型表达式：
   ```scheme
   (inner:record? point? (fields (x number?) (y string?)))
   ```
2. `record.sls` 不再分别设置 constructor/getter/setter 的 type-expressions，而是统一设置 record 本身的类型定义。
3. `interpreter.sls` 根据 `inner:record?` 自动推导：
   - constructor: `(point? <- (inner:list? number? string?))`
   - getter `point-x`: `(number? <- (inner:list? point?))`
   - setter `point-x-set!`: `(inner:void? <- (inner:list? point? number?))`

**好处**：
- 字段类型不再丢失，从 `something?` 提升到具体类型。
- 天然支持 parent 继承：`(inner:record? child? (parent point?) (fields (z boolean?)))`
- 类型错误检测能力大幅提升（如 `point-x-set!` 收到 `string?` 可以报警）。

---

#### 方案 D：把 Record Type 处理合并到 Identifier 阶段（长期）

**改动**：
1. 在 `define-record-type.sls` 创建 `identifier-reference` 时，**直接设置** `type-expressions`。
2. 删除 `record.sls` 的 `define-record-type-process`，或将其简化为空壳。
3. `generator.sls` 的 `step` 对 `define-record-type` 节点只做递归遍历 children，不再做特殊的 type rule 处理。

**好处**：
- 彻底消除 identifier 阶段和 type 阶段的执行顺序依赖。
- `type-expressions` 在对象创建时就确定，之后不再 mutable，消除数据竞争。
- 代码路径更简单，易于维护。

**风险**：`define-record-type.sls` 当前不知道字段的具体类型（如 `x` 是 `number?`），只能设置 `something?`。如果要支持精确字段类型，需要等 `construct-substitutions-for` 完成后才能知道。因此这个方案与「精确字段类型」目标有冲突。

---

#### 方案 E：延迟精确化（Lazy Refinement）

**改动**：
1. Identifier 阶段创建 reference 时，设置**粗略**的 type-expressions（字段类型用 `something?`）。
2. `construct-substitutions-for` 完成后，运行一个**二次遍历**（`refine-record-types`），根据 `make-point` 在代码中的实际调用参数，推断字段的具体类型，然后更新 type-expressions。
3. 使用 immutable 的 type-expression 结构，更新时创建新对象而非 `set!`。

**好处**：
- 结合方案 A 和 D 的优点：创建时就有类型，后续还能精确化。
- 不引入 mutable 状态，支持多线程和缓存。
- 可以从 `(make-point 1 2)` 的调用点推断 `x` 和 `y` 是 `number?`。

---

### 5. 测试覆盖现状

当前已有测试：

| 测试文件 | 覆盖内容 |
|---------|---------|
| `tests/analysis/type/substitutions/rules/test-record.sps` | 真实项目中的 `define-record-type`（`position->alist` 的 `position-line`、document 的 setter） |
| `tests/analysis/type/substitutions/rules/test-record-comprehensive.sps` | `record-type` fixture 的全面测试：constructor/getter/setter/predicator 的 type-expressions、interpreter 结果、predicator 双源问题、identifier-reference-index-node 异常 |
| `tests/robustness-editor-fuzz.sps` | 把 `record-type/point.scm.txt` 作为 fuzzing fixture，验证变异后不会崩溃 |

**测试缺口**：
- 没有覆盖 `(parent ...)` 继承场景的类型推断
- 没有覆盖增量更新（修改 `define-record-type` 后 refresh）的类型一致性
- 没有覆盖多线程并发创建同名 record type 的场景

---

*文档版本：2024-05-09*

## 14. 工具：output-type-analysis


`bin/output-type-analysis.ss` 是一个命令行工具，用于批量导出 Scheme 项目中 library `(export ...)` 子句里各标识符的类型推断结果。

---

### 前置条件

运行前必须激活 Akku 环境，否则 Chez Scheme 找不到依赖库：

```bash
source .akku/bin/activate
```

---

### 两种运行模式

#### Mode A：单库模式（3 参数）

只分析指定 library，输出该库所有 export 标识符的类型。

```bash
scheme --script bin/output-type-analysis.ss \
  <target-dir> \
  "<library-name>" \
  <output-file>
```

- `<target-dir>` — 项目根目录（会被 `init-workspace` 扫描）
- `<library-name>` — library 名称，用 S-expression 字符串形式，如 `"(fixtures simple-lib math)"`
- `<output-file>` — 输出文件路径

**示例**：

```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  "(fixtures simple-lib math)" \
  /tmp/math-types.txt
```

**示例输出**：

```
library:	( fixtures simple-lib math)
path:		tests/resources/workspace-fixtures/simple-lib/math.scm.txt
identifier:	add
type:		([identifier-reference number?] <- (inner:list? [identifier-reference number?] [identifier-reference number?] ) )
```

#### Mode B：批量模式（2 参数）

遍历目录下**所有** library，输出每个 library 的 export 标识符类型。

```bash
scheme --script bin/output-type-analysis.ss \
  <target-dir> \
  <output-file>
```

**示例**：

```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  /tmp/all-types.txt
```

---

### 输出格式

每个 file-node 产生一个输出块：

```
library:	(<library-name>)
path:		<absolute-path>
identifier:	<symbol>
type:		<type-expression>
...
```

- 同一 identifier 的多条 type 变体会被去重合并。
- 无 `(library ...)` 头的脚本文件不产生输出（不会输出 `library: ()`）。

---

### 自动文件过滤器

脚本会根据目标目录特征自动选择扫描范围：

| 条件 | 过滤器 | 扫描文件 |
|------|--------|---------|
| 目录下存在 `.akku` | `'akku` | `.sps` `.sls` `.scm` `.ss` |
| 目录下无 `.akku` | `'txt` | `.scm.txt` |

因此 fixture 目录（无 `.akku`）和项目自身源码（有 `.akku`）都可以直接运行，无需手动指定过滤器。

---

### 注意事项

1. **每次修改 `analysis/` 下的 `.sls` 源码后，若用此工具验证，建议先 `rm -rf .akku/libobj/scheme-langserver`，避免加载旧的 `.so` 缓存。**
2. 类型推断依赖 `init-workspace` 的完整分析流程（VFS → library-node → file-linkage → abstract interpreter），首次运行需要编译缓存，耗时较长（数十秒到数分钟）。
3. 批量模式在项目自身（~200 个 `.sls`）上运行可能需要数十分钟，属正常现象。
4. 脚本已内置 `(type-inference-advanced? #t)`（2026-11-05 起），即默认启用完整推断（等价 `run.ss -t advance`：fixpoint + rest-elements 两个 pass）；此前门控落地后脚本曾静默退化为 basic 推断。
5. 输出**不截断**：展示上限为 `greatest-fixnum`，所有候选类型全部列出（hover 仍保留 10 条截断，两者管线不同段）。
6. 输出文件已存在时会**先删除再写**（2026-11-05 修复：Chez `call-with-output-file` 遇已存在文件直接报错，此前重跑到同一路径会以静默崩溃告终）。

### 4. 历史与实现细节

#### 4.1 输出过滤策略的演变

`output-type-analysis.ss` 对原始推断结果的处理经历过三次变化：

**第一阶段（2026-06-18 之前）**：简单过滤 `"something? "`。

```scheme
(filter 
  (lambda (i) (not (equal? i "something? ")))
  ...)
```

这导致 `assq-ref` 被错误显示为返回 `boolean?`：因为 `assq-ref` 的真实返回 union 是 `{something?, boolean?}`，过滤掉 `something?` 后只剩下 `boolean?`。

**第二阶段（2026-06-18，commit `1c9ebfe9`）**：引入 `private:merge-something-union`。

只要结果中出现任意 `something?`，就把所有函数签名的返回值统一替换为 `something?`。这修复了 `assq-ref` 的 false positive，但也把 `contain?` 等真正返回 `boolean?` 的递归函数的精确签名压掉了。

**第三阶段（2026-06-20，commit `7691c08`）**：移除合并与过滤。

`something?` 被视为"不够精确"而非"错误签名"，不再做 union 级合并。

**第四阶段（2026-11-02/05，P2 压缩 + advance 门控）**：输出接入 hover 同款的语义压缩管线（`sem:compact-display`：语法去重 → 箭头支配消除 → 反链吸收，语义等价不丢信息，`natural-order-compare` 73 签名 → 6 条互不包含签名）；展示上限放开为不截断；脚本内置开启 `type-inference-advanced?`（完整推断）。例如 `contain?` 会同时显示：

```text
type:		([identifier-reference boolean?] <- (inner:list? something? something? something? ) ) 
type:		(something? <- (inner:list? something? something? something? ) ) 
```

#### 4.2 为什么使用 `type:interpret-result-list`

`output-type-analysis.ss` 原本可以调用 `type:recursive-interpret-result-list`（表达式级广度优先展开），但实验表明：

- 对非递归函数，输出没有明显改善，有时甚至退化（如 `type:solved?` 的 `boolean?` 重载被替换为 `something?`）。
- 对递归函数（`util/contain`、`util/binary-search`），会因未解决表达式集合爆炸而超时。

因此当前仍使用 `type:interpret-result-list`（无额外结果数量预算，依赖解释器自身的 max-depth/max-recursion 限制），在合理时间内完成推断。

#### 4.3 常见审查标准

对输出结果通常按以下维度判断：

| 维度 | 合理（✅） | 部分合理（⚠️） | 不合理（❌） | 无输出（➖） |
|------|------------|----------------|-------------|-------------|
| 是否有类型输出 | 有具体类型 | 有但含 `something?` | 有但明显错误 | 完全无输出 |
| 过程参数数量 | 与源码一致 | 部分一致 | 明显不一致 | — |
| 返回类型 | 与源码一致 | 过于保守 | 明显错误 | — |
| record 相关 | predicate/constructor/accessor 类型清晰 | 只有 `something?` | signature 明显错误 | — |

**明显错误的例子**：
- 已知返回 `boolean?` 的 predicate 被推断为 `number?`。
- 接受 2 个参数的函数被推断为 `(something? <- (inner:list? something?))`（1 个参数）。

#### 4.4 用于评估的 6 个 library

为系统评估类型推断质量，常选取以下 6 个复杂度递增的 library：

| Library | 选择理由 |
|---------|----------|
| `(scheme-langserver util contain)` | 纯函数、递归 predicate |
| `(scheme-langserver util json)` | 字符串/列表操作、少量分支 |
| `(scheme-langserver virtual-file-system file-node)` | `define-record-type`、getter/setter |
| `(scheme-langserver analysis identifier reference)` | records、多字段、导出过程 |
| `(scheme-langserver analysis type domain-specific-language interpreter)` | 类型系统自身，自举测试 |
| `(scheme-langserver util binary-search)` | 已知表现差的递归/索引算术案例 |

完整评估方法见本文 §10.3（基准方法论与优化尝试记录）。

## 15. 未决项（Open Items）

> 本节由两份文档合并而来：`substitutions/rules/audit.md`（2026-10-29 规则审计）与
> `type-inference-improvement-plan.md`（2026-11-02 改进方案调研）。已完成的部分
> （P0–P3、审计 Phase-1 项）不再保留，只留未落地项与可复现的质量基线。

### 15.1 替换规则审计（核对基线：15 个规则文件 / 8 个专项测试）

**O1. `cond.sls` / `if.sls` 的条件约束为零信息**。两者都把条件类型硬编码为
`'something?`（全顶类型），不携带任何信息。修法：约束为 `boolean?`。风险低、
语义更优（Scheme 容忍非布尔条件，但 *意图* 是布尔，更紧的约束可能暴露真实错误）。

**O2. `do.sls` 忽略 `(test result ...)` 尾部**。规则处理了绑定
（`var init update`），但没有把父 `do` 节点链接到 `result ...` 表达式，
`do` 形式的类型永远与实际返回值不统一。修法草图：处理完绑定后，走
`(caddr children)` 的子节点，跳过 test 谓词，在 `do` 节点与每个 result
表达式之间注册双向替换。

**O3. `case.sls` 不对 key 做 datum 收窄**。key 被转发给各子句的 datum 列表，
但没有任何约束把 key 的类型与 datum 类型联系起来。完整收窄需要 datum 求值；
最低可接受的步骤是加一个回归测试钉住转发行为。

**O4. `application.sls` 不注册应用约束**。节点的子节点直接成为其可能类型，
但不产生任何 实参↔形参 约束——真正的应用推断需要经 identifier-reference
系统解析被调用者类型。**建议**：没有更大范围的设计评审不要动；按比例
对应的步骤是加一段说明该限制的代码注释。

**O5. 测试覆盖缺口**。15 个规则文件只有 8 个专项测试。缺：
`application`、`begin`、`case`、`cond`、`do`、`if`、`unless`（外加 s7 的
`define*`/`lambda*`，目前搭 `define`/`lambda` 的测试便车）。优先最小
fixture 单元测试，一个规则一个测试文件。

**O6. `trivial.sls` 是大杂烩（223 行）**。字面量、pair/vector、引用形式、
参数隐式转换、跨文档引用、空列表全在一个模块。可拆为
`trivial-literals` / `trivial-collections` / `trivial-quoting`，
中等工作量、低紧迫度的可维护性重构。

已从审计中移除（已完成）：`let*.sls`/`letrec.sls` 重复——`letrec.sls` 已删除，
named-let 与 `letrec`/`letrec*` 统一由 `let*.sls` 分发；`unless` 已路由到
独立的 `unless-process`。

### 15.2 改进路线图未落地项（2026-11-02 调研）

问题基线（`output-type-analysis` 全量导出本仓库 543 个 export 标识符）：
precise 38% / imprecise 49% / no-info 13%，无错误签名。四个问题类中
**递归返回值（不动点迭代）与数值塔压缩、rest 槽（P0–P3）已完成**；
完成部分随 `-t advance` 门控（见 §9.1 与 §10 的 2026-11-05 注记）。
剩余方向：

**P4. 谓词命题槽位 + semtype narrowing（未开始，根治 something? 泛滥）**。
arrow 挂 `(正, 负)` 命题对（Typed Racket ICFP'10 / PEP 742 TypeIs 同款）；
`if`/`cond` 分支 then→`arg∩T`、else→`arg−T`。只处理一阶位置（谓词直接
应用裸标识符），不碰 higher-order 命题擦除不 sound 的坑
（arXiv:2609.16299）；union 求差复用已落地的 semtype 引擎。

**P5. record 字段类型 / CDuce record 模型 / SCC 互递归（长期，未开始）**。
短期：constructor 调用点累积 union（Dialyzer 局部版）；中期：对齐 CDuce
record 模型（arXiv 2404.00338）与 semtype 引擎衔接。**排除 row
polymorphism**（OCaml/Elm 路线需要 unification，与 substitution DSL
不兼容；R6RS record 封闭语义 → 选 Hack shape 式封闭记录）。

**数值塔 1C（可选档位）**：IR 级 tower collapse，模仿 Pyright int→float
promotion / TS literal widening，有损；hover 显示用、诊断关断。

方向间收敛性：POPL'22 已把 occurrence typing 归约到集合论框架——P4 的
restrict/remove 与 semtype 的 meet/diff 是同一套原语；已落地的压缩
（P2）也在给 semtype 引擎喂数据。无架构冲突。

### 15.3 全量导出质量基线复现（2026-11-02 实测）

- 命令：`scheme --script bin/output-type-analysis.ss . /tmp/sls-types-all.txt`
  （需 `source .akku/bin/activate`；全量含依赖 22.5 分钟）
- 6 评估库耗时：contain 37s / json 14s / file-node 13s / reference 13s /
  interpreter 17s / binary-search 15s（历史上 interpreter 与 binary-search
  在 600s 超时阈值下失败，当时已快 35 倍以上）
- 复核确认：271 条"空 `(inner:list?)` 返回"全部为 `'()` 基例分支的 sound
  签名（如 `search-end-with` 的 else 分支），非错误签名

## 16. 外部先例深度调研（类型推断 DSL 的理论背景）


> 调研日期：2026-10-31。动机：为 scheme-langserver 的类型推断 DSL（谓词符号树 +
> 规则表 + 候选并集，见 `domain-specific-language/`）寻找外部的理论先例、算法蓝本与
> 可借鉴机制。四个方向各由独立调研通道完成，文献阅读完整性在附录 A 逐份声明。
>
> 快速结论：**没有完全一样的系统**——"内置过程的类型写成可声明的规则表 + 模板宏 +
> 解释器"的组合未见第二家；但拆开来，每个特性都有 30 年内的成熟对应，且四个方向
> 在文献中互相引用、最终汇聚到 Castagna 的集合论类型框架。

#### 总览：四条线其实是一条线

```
内部表示：  谓词符号树 + 候选并集  ──升级──▶  集合论内部表示（方向④，基础设施）
产品语义：  有类型就显示             ──补纪律──▶  success typing 判据（方向①，零误报诊断）
精度来源：  规则表手写门            ──自动化──▶  occurrence typing 分支细化（方向②，提升最大）
规则理论：  ad-hoc with-gate       ──找背书──▶  conditional types（方向③，理论正当性）
```

①②③④ 可独立落地，但组合起来构成自洽体系：④给判定、①给语义纪律、②给精度、③给
规则层形式化。长期看 ②的 restrict/remove 与 ④的 meet/diff 会是同一套原语——现在做
②时把 restrict/remove 实现为"候选集操作 + 留交集接口"，就是在为④铺路。

#### 阅读指南

- §1 Success Typing（Dialyzer）——语义内核：过近似 + 空交判据 + widening
- §2 Occurrence Typing（Typed Racket）——分支细化，用户可见收益最大
- §3 Conditional Types + 宏类型规则——with-gate 的理论谱系与规则注册先例
- §4 Semantic Subtyping——`type:→?` 重写的算法蓝本与硬性差距
- §5 综合建议（性价比排序）
- 附录 A：阅读完整性声明；附录 B：全部来源 URL

---

### §1 Success Typing（Dialyzer）——给"候选并集"立语义

**核心一手资料**：
[Lindahl & Sagonas PPDP'06 全文](https://user.it.uu.se/~kostis/Papers/succ_types.pdf)（DOI 10.1145/1140335.1140356）、
[TypEr 全文](https://user.it.uu.se/~kostis/Papers/typer.pdf)、
[contracts 论文 Erlang'07](https://user.it.uu.se/~kostis/Papers/contracts.pdf)、
[Bugs'05 经验](https://user.it.uu.se/~kostis/Papers/bugs05.pdf)、
[Falsification 视角 arXiv:1502.01278](https://arxiv.org/abs/1502.01278)、
[etylizer 对比评估](https://arxiv.org/html/2302.12783v2)、
[OTP 源码 lib/dialyzer/src](https://github.com/erlang/otp/tree/master/lib/dialyzer/src)。

#### 1.1 形式定义

**Definition 1（PPDP'06 §4.1）**：函数 f 的 success typing 是签名 `(ᾱ)→β`，满足：
*每当 `f(p̄)` 归约到值 v，就有 `v∈β` 且 `p̄∈ᾱ`*。关键在补集方向：`p̄∉ᾱ ⟹
该应用必定失败*（crash 或不终止）。推断目标是**过近似**"能成功"的集合，与
Hindley-Milner 收缩"被允许"集合的方向相反。

试金石（论文用布尔 `and/2` 比较三个系统）：

| 系统 | 推断类型 | 问题 |
|---|---|---|
| HM | `(bool,bool)->bool` | 拒绝 `and(false,42)`——但运行时不报错 |
| Marlow-Wadler 子类型 ICFP'97 | `(any,false)->bool` | 反直觉；域欠近似 |
| **Success typing** | `(any,any)->bool` | 域**过近似**：包含所有可能成功的调用 |

本质差异：静态类型域 ⊆ 动态类型域（欠近似，pessimistic）；success typing 域 ⊇
动态类型域（过近似，optimistic）。HM 用**合一**解约束；success typing 用**子类型**，
且域是无限制并集类型（`atom|pid` 直接写并，无需构造器包装），单子类型免费获得。

Jakob & Thiemann 的再表述（模型检测术语）：**标准类型系统做 verification（证明无错），
success typing 做 falsification（证伪，只报告 definite errors）**。

与 soft typing（Wright/Cartwright 一脉、MrSpidey）的区别：soft typing 报告**完备**
警告并插装运行时检查；success typing 报告**可靠**警告、零程序拒绝、不插装。

**类型错误判据**（PEPM'13 形式化，全部"用户可见价值"就这一句）：

```
e₁ 产生类型错误 ⟺ type(e₁) = τ₁ ∧ 上下文期望 τ₂ ∧ τ₁ ∩ τ₂ = none()
```

#### 1.2 推断算法：两阶段

**阶段一：约束生成**（推导规则要点）：

- `ABS`：函数体约束绑定到函数类型上（constrained type），但**不外泄**；
- `CASE`：每个子句生成一个析取支，不转 DNF（多 case 嵌套时防爆炸）——这是
  path-insensitivity 的精确来源（TypEr §4.4.3 原话："making the analysis
  path-insensitive, thereby gaining time at the cost of precision"）；
- `APP`（非常规）：应用结果的类型 β 是函数值域的**上界**（β⊆α），而非传统系统的
  下界——success typing 只关心避免误报，乐观假设成立。

**阶段二：约束求解**：

- `Sol`：类型变量 → 具体类型；初始全部 `any()`，**递归变量初始 `none()`**
  （第一轮递归调用无解=⊥，被 join 吸收，leaf 子句先给闭式解）；
- 子类型约束求解 = meet：`T = Sol(α) ⊓ Sol(β)`；T=none() ⟹ 该子句不可满足 ⟹
  该子句**不贡献类型**；
- 析取的解 = 各支逐点最小上界（join）；`⊥ ⊔ Sol = Sol`；
- 整体：调用图 → SCC 凝缩 DAG → 拓扑逆序自底向上，递归组内迭代至不动点。

**并集与终止保障**：

- 类型格：单子类型 + 基类型 + 结构类型（元组按 arity；list 是**唯一递归类型**，
  `list(T) = cons(T,list(T)) ∪ nil`，参数必为具体类型）+ 函数类型 + 任意并。
- **union limit**：并集成员超限加宽到超类型（limit=3 时 `1∪2∪3∪4 → integer()`）；
  现代 Dialyzer 有 range 与 byte/char 阶梯式加宽。
- **depth-k 抽象**：复合项深度超阈值 k 的子项抽象为 `any()`，与 union limit 配合
  保证格有限 ⟹ 不动点必达。
- 下界：树自动机包含问题 EXPTIME-complete；实践中毫秒级（见 §4 性能数据）。

**精度来源——第二遍 refinement（PPDP'06 §6，全文最精巧的部分）**：

- *Definition 3*：refined success typing `(ᾱ')→β'` 满足 `ᾱ'⊆ᾱ`、`β'⊆β`，对所有
  `p̄∈ᾱ'` 能成功者 `f(p̄)∈β'`；额外约束有两个来源：调用点与程序员 spec；
- **闭世界 refinement**：模块系统不可绕过。escape analysis 区分 escaping（导出
  函数 + 闭包逃逸的局部函数）与 internal 函数；**internal 函数的所有调用点已知**，
  定义域 refine 为各调用点实参类型之并（经典例子：`length3(List,N)` 累加器从
  `(list,any)->any` 沿调用链 refine 成 `(list,integer)->integer`）；
- 前向数据流：从 escaping 函数入口启动（参数类型 jump-start 为 success typing 的
  域——安全，因为域包含一切可成功输入）；worklist 风格；函数体内 path 独立
  （case 各支 join），函数间有限 path 敏感（每个函数按**具体调用类型**分别分析，
  调用类型种数达上限时 widen 为并集）；
- **注意**：逃逸函数（= 库导出函数 = LSP hover 最常展示的）refine 不了。

#### 1.3 零误报的本质与"报不出"的边界

三道保险：

1. **乐观初始化**：所有未知函数/不可达代码默认 `(any,...,any)->any()`
   （PPDP'06 §4.2 "a final note"——这同时是模块化手段：缺代码不影响可靠性）；
2. **path 不敏感 + join**：合并路径丢精度时只可能把类型变大（过近似方向），
   不会制造假冲突；
3. **主动压制**：Bugs'05 原话 "Dialyzer currently **suppresses all discrepancies
   that might be due to losing information when joining**"——宁可漏报，不误报。

诚实注脚：零误报是设计目标而非绝对定理。etylizer 在其 321 例基准上发现 Dialyzer
2 例假阳性（1 例确认 bug，OTP issue #6333，已修）。

**"报不出"的统一机制**：过近似太大 ⟹ 交集判据失效。具体病灶：

1. 恒等/透传函数退化：`id(X)->X` 推断 `(any)->any`——任何调用都与域相交，永远不报
   （LYSE 的 `convert/1` 例子：两处**交换后必崩**的调用，Dialyzer 全静默；补重载
   spec 后才报出）；
2. 控制流信息丢失：success typing 不维护"输入 integer 则输出 integer"的依赖；
3. 深度截断（§2.3 的 `{succ,...}` 例子：第 4 层被 depth-k 吃掉，必崩调用漏报）；
4. 尾部递归累加器；
5. 量化：[etylizer 基准](https://arxiv.org/html/2302.12783v2)（156 个含已知类型
   错误的用例）Dialyzer 只检出 89 个——**约 44% 的 definite type error 漏报**
   （基准偏小，量级感真实，不可外推大库）。

PEPM'13 展示了"报不出"并非全坏：调用点能**反向约束**变量类型（"even uses of
variables can restrict their values"）——success typing 的调用点既能前向也能
后向收窄。

#### 1.4 多态的缺陷与补救

- 系统只有子类型多态，没有参数多态（TypEr §4.8 明确）。`id → (any)->any`；
  `lists:map/2` 是 `(((any)->any), [any]) -> [any]`；"success typing 准确捕获了
  所有不崩溃的调用，包括非预期调用"（`map(3.14, [])` happily 返回 []）。
- LPAR'18（仅摘要）批评："Dialyzer exhibits two main weaknesses: on the practical
  side, its ability to deal with functions that are typically polymorphic is rather
  poor; and on the theoretical side, a fully developed theory ... does not seem to
  exist"——多态 success type 的推导系统与正确性证明。
- **补救一：refinement 兜底**——但只对非逃逸函数有效。
- **补救二：user specs / 重载 contract**（contracts 论文，OTP `-spec` 的原始设计）：
  - spec 与 success typing 取 **infimum（协变 meet）**，四种情况中唯一报警的是
    **交集 = none**（contract violation）；spec 声明点不能被证伪则被信任，之后
    调用点违例算调用方责任（"soundness for contract violations"）；
  - 多态 type variable 实例化：单变量实例化为调用点实参类型；多变量实例化为各
    实参类型的**最小上界**（作者自认 ad-hoc）；
  - 重载 spec：域重叠时 **collapse 为支的并集**或**直接忽略**（现代 Dialyzer
    报 "Overloaded contract has overlapping domains; such contracts are currently
    unsupported and are simply ignored"）。

#### 1.5 IDE 集成与规模化经验

- **TypEr**：输出源码级 `-spec` 注释文件；`--byte` 模式直接读 BEAM debug_info
  （对 LSP"只开 buffer 未存盘"场景有直接借鉴意义）；持久表（PLT 雏形）。
- **性能实测**：

| 数据点 | 数值 | 来源 |
|---|---|---|
| 旧版吞吐 | ~800 LOC/s（2GHz P4） | bugs05 |
| OTP 全库（~70 万 LOC）success typing | ~30 min | PPDP'06 §6.4 |
| PLT 构建（现代） | 示例 6m39s；R15B02 起并行 | LYSE |
| 增量 | `--incremental`：变更模块→依赖模块最小集重跑 | OTP User's Guide |
| LSP 场景（ElixirLS） | 首启 PLT 10+ 分钟 CPU 打满，之后自动增量 | elixir-lsp/elixir-ls |

- **ElixirLS**（IDE 先例）：LSP server 内嵌增量 Dialyzer + "automatic inline
  suggestion of @specs based on inferred success typings"——证明零误报纪律是 IDE
  场景被接受的必要条件（误报在编辑器里比 CI 更致命）。
- **错误解释**（PEPM'13）：约束求解器携带源位置集合，对每个报警计算**最小程序
  切片**——LSP 诊断需要定位到子表达式而非整函数。
- **PLT ↔ workspace cache**：PLT 存逐模块的 success typings + contracts + 调用图；
  未变模块不重分析。对应我们的 workspace FASL cache（mtime 一致性校验已有），
  把"逐 library 的推断签名"作为缓存内容即可。

#### 1.6 与我们 DSL 的对应与落地路径

| Dialyzer | scheme-langserver DSL |
|---|---|
| `any()` | `something?` |
| union 过近似 | substitution 候选列表 |
| 空交判据 | **没有**——只有 `type:→?` 树匹配 |
| depth-k / union limit | `PRIVATE-MAX-DEPTH 10`、`chez-ir-rebuild-budget`（同哲学） |
| 闭世界 refinement | library-node 树（天然对应） |
| PLT | workspace FASL cache |

**结论（调研原文）**：success typing 的语义内核（过近似 + 空交判据 + 乐观 any
初始化 + 闭世界 refinement + union/depth widening）与 substitution 式规则 DSL
**完全兼容，其中判据与 widening 可立即落地**；约束求解器本身（meet/join 不动点）
不必搬——用谓词格写一个 **~200 行的交集检查 post-pass** 即可等价获得"零误报
诊断"能力。具体：generator 产出 substitution 后加 post-pass——对每个应用节点，
实参候选与规则定义域做谓词符号求交，空则报 "definite type error"。

**固有代价（接受）**：参数多态缺失（高阶标准库函数 hover 长期 `something?`，
与 lambda 参数槽退化同款病，可接受——目标是"不错"而非"证明"）；零误报纪律主动
压制诊断（"Dialyzer 没报 ≠ 没问题"要写进预期管理）；副作用盲区（`none()` 与
失败的混淆，诊断文案写 "will never return a value" 而非 "type error"）。

---

### §2 Occurrence Typing（Typed Racket）——最大的精度提升机会

**核心一手资料**：
[POPL'08 全文](https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf)、
[ICFP'10《Logical Types for Untyped Languages》全文](https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf)
（现行实现对应的最终形式化，DOI 10.1145/1863543.1863561）、
[Castagna POPL'22](https://www.irif.fr/~gc/papers/popl22.pdf)、
[Utah PLT ifT-benchmark](https://github.com/utahplt/ifT-benchmark)、
Typed Racket 参考手册（ts-reference 8.17）。

#### 2.1 两代模型

**POPL'08（λ_TS，可见谓词版）**。类型语法：⊤、基类型、有限并集 `(S τ ...)`
（`(S)` = ⊥）、带潜在谓词的函数类型 `τ φ→ σ`。判断形式 `Γ ⊢ e : τ ; ψ`。

- 潜在谓词 φ（标注在函数类型上，"该函数是类型 τ 的判别器"）：
  `number? : ⊤ Number→ Boolean`；对单变量参数的应用把潜在谓词实例化为可见谓词；
- 可见谓词 ψ：`τx`（x 当前具有类型 τ）、`x`（x 不是 #f）、`true`/`false`、`•`；
- **环境更新规则（Fig. 4，细化规则的全部核心）**：

```
Γ + τx  =  Γ[x := restrict(Γ(x), τ)]     ; then 分支：收窄
Γ + x   =  Γ[x := remove(Γ(x), false)]   ; then 分支：去掉 #f
Γ + •   =  Γ
Γ − τx  =  Γ[x := remove(Γ(x), τ)]       ; else 分支：挖掉 τ
Γ − x   =  Γ[x := false]                 ; else 分支：必为 #f

restrict(σ,τ) = σ        若 ⊢ σ <: τ
restrict(σ,(S τ…)) = (S restrict(σ,τ) …)        ; 并集逐分量
restrict(σ,τ) = τ        其他情况（退化为 τ）
remove(σ,τ)   = ⊥        若 ⊢ σ <: τ              ; 分支不可达
remove(σ,(S τ…)) = (S remove(σ,τ) …)
remove(σ,τ)   = σ        其他情况
```

- **T-IF**：测试 e₁ 产生 ψ；then 分支在 `Γ+ψ` 下，else 在 `Γ−ψ` 下。例：
  `(λ (x : (S Number Boolean)) (if (number? x) (= x 1) (not x)))`——then 分支
  x:Number，else 分支 `remove((S Number Boolean), Number) = Boolean`；
- and/or 的组合（combpred 元函数）是**特设的**——ICFP'10 明确列为旧系统三大缺陷
  之一；
- 声音性麻烦：β 归约后的中间项可能不可类型化（如 `(if (number? #f) ...)`），
  论文加了仅用于证明的辅助规则。模型经 Isabelle/HOL + PLT Redex 双重机械化
  验证（github.com/samth/popl08-model）。

**ICFP'10（λ_TR，命题逻辑版，现行基础）**。判断升级为：

```
Γ ⊢ e : τ ; ψ⁺ | ψ⁻ ; o
```

e 具有类型 τ；若 e 为真则命题 ψ⁺ 成立，为假则 ψ⁻ 成立；o 是**对象**（e 访问的
运行时环境片段：∅、变量、或访问路径 π(x) 如 `car(cdr(x))`）。

- 命题语法：`τx`、`¬τx`、`ψ∧ψ`、`ψ∨ψ`、`ψ⊃ψ`、`tt`、`ff`；
- 函数类型携带**潜在命题对**（箭头上方）与**潜在对象**（箭头下方），参数命名
  使命题良作用域：

```
number? :  y:⊤  N(y) | ¬N(y)  ──→(∅)  B
car     :  x:⟨⊤,⊤⟩  ⊤car(x) | ⊤car(x) ──→(car) ⊤
```

- **关键洞察：两个潜在命题不一定互补**。`(and (number? x) (> x 100))` 的真命题
  是 `N(x)`，假命题却是 `¬N(x) ∨ ¬(>x100)`（x 可能是 97）——**这就是 and 的
  else 分支不能简单取反的原因**（ICFP'10 §3.1）；
- **T-VAR**：变量类型**从命题环境推导**——若 Γ 能证明 `τx` 则 x:τ；类型环境降级
  为命题环境的特例；
- **T-LET**：环境加 `τx` 与两条蕴含（`#f(x) ⊃ ψ₀⁺` 等），处理 or 展开中
  "tmp 为真 ⟹ (number? x) 为真"的推理；
- **and/or/not 不设特殊规则**：由 if 展开 + T-IF 传播自然导出。or 的真命题 =
  `ψa⁺ ∨ ψb⁺`，假命题 = `ψa⁻ ∧ ψb⁻`；and 反之；
- 证明系统（Fig. 4，自然演绎）关键原子规则：
  - L-SUB：`τx ⊢ σx` 若 τ<:σ；
  - L-SUBNOT：`¬τx ⊢ ¬σx` 若 σ<:τ（否定单调性反向）；
  - L-BOT：`⊥x` 推出任何命题；
  - L-UPDATE：`update(τ,σ) = τ∩σ`、`update(τ,¬σ) = τ−σ`；带路径时先沿路径进入
    类型再细化（环境证 `⟨⊤,⊤⟩x` 与 `Ncar(x)` 可推 `⟨N,⊤⟩x`）；
- cond 多子句推理（§5.3 例 14 全文推导）：第二子句环境已有第一子句测试失败的
  命题，叠加本子句测试命题，由矛盾消去得细化结果——"程序员式推理"的完整机器化。

#### 2.2 算法化：语法导向单遍检查，不是约束求解

- ICFP'10 §7.4：类型系统非算法化；实现需 (a) 消去 subsumption（把子类型义务分配
  进各规则）；(b) 实现 T-VAR（从命题环境推导变量类型的过程）；
- Typed Racket 实现形态（typed-racket-lib/typecheck/）：命题实现为受限 ADT——
  `Top`/`Bot`/`(: obj type)`/`(! obj type)`/`(and …)`/`(or …)`/`(when p q)`/
  `(unless p q)`/`(if p q r)`/`(linear-comp …)`；对象 = 标识符 + 路径元素列表。
  环境操作 = lookup/update（即 restrict/remove），推理**只在需要给变量定类型时**
  对原子命题做 restrict/remove，复合命题靠构造时的规范化（CNF/DNF 化）——
  **"证明系统"退化为命题集合的增量规范化 + 原子查询，无需一般定理证明**；
- 语法导向：类型检查在完全宏展开后的核心形式上单遍进行（Typed Racket 把检查器
  **嵌入宏展开器**）；
- 子类型关系本身平凡（顶层/自反/并集两条/函数逆变协变 + 潜在谓词可丢弃）；
  **细化全部发生在环境操作层，与子类型判定正交**——并集子类型是"候选并集"细化
  能工作的前提：then 分支 = 过滤，else 分支 = 删除；
- 可变性处理（ICFP'10 §7.1）：λ_TR 假设不可变；Racket 中可变字段的 selector
  被赋予平凡潜在命题/对象；被 `set!` 赋值的变量**禁用 L-UPDATE**（模块级静态
  分析找所有赋值点）——对 LSP 好消息：静态分析 set! 目标并禁用细化即可，
  模块化且便宜；
- case-lambda/多值（§7.2）：函数值域变为"每个返回值各带类型 + 潜在命题对 +
  潜在对象"的序列。

#### 2.3 工具链中的同类实现

| 系统 | 关系 |
|---|---|
| Typed Clojure / core.typed | 直接改造 TR 的 occurrence typing（ESOP'16）；后有 symbolic closure types（POPL'22） |
| Flow | 官方术语 "type refinements"；用**函数类型交集**表达谓词分支精确类型 |
| TypeScript | "type guard recognition"/narrowing；occurrence typing 的**直系后代但刻意简化**（无否定/交集的系统内账户，谓词细化硬编码于 CFG 走查；不完备也不保真）。Castagna《Revisiting Occurrence Typing》(SCP 2022)：narrowing ≡ occurrence typing，且二者从诞生起就与集合论语义类型纠缠 |
| Whiley | Pearce 独立再发明 flow typing |
| Rosette | 符号执行语言中的 occurrence typing（POPL'17） |
| mypy/Pyright 等 | 现代工业形态；ifT-benchmark 用 ICFP'10 例子做跨语言基准 |

**Occurrence Typing Modulo Theories**（Kent, Kempe, Tobin-Hochstadt, PLDI'16，
arXiv:1511.07033）：命题可携带线性整数约束交外部求解器；56k 行 TR 代码案例：
vector 访问 50% 零注解可证安全，加少量注解达 ~80%。对轻量 DSL 超预算，但其
"命题可扩展"架构（`(linear-comp obj obj)` 作为 prop 语法一格）值得借鉴。

**理论下限（对轻量实现最有用）**：Castagna POPL'22《On Type-Cases, Union
Elimination, and Occurrence Typing》——不需要效应系统、不需要流分析、不需要发明
新类型，只要 type-case + MacQueen et al. 1986 的三条 union elimination 规则，
即覆盖 occurrence typing 的全部用例。**restrict/remove 正是这三条规则的算法形态**。

#### 2.4 限制、代价与元理论警告

1. **宏交互**（POPL'08 自承局限）：必须先宏展开再检查；"深刻改变绑定结构"的复杂
   宏无法处理。Takikawa 博士论文记录了 class 系统的宏插桩工程；
2. **可变性**：路径细化对可变数据不可靠（别名）；samth 确认"occurrence typing
   不细化 mutable 变量"；
3. **元理论（重要）**：Fu, Angiuli & Tobin-Hochstadt 2026《Revisiting Soundness
   for Occurrence Typing, Semantically》(arXiv:2609.16299) 证明 ICFP'10 的替换式
   元理论**存在多处错误、声音性定理表述甚至不是良构的**，缺陷被 Kent'16、
   Fu'21、Bonnaire-Sergeant'16 继承，并**在 Typed Racket 实现中表现为真实的
   声音性 bug**；修复用 step-indexed logical relations（Lean 形式化）。**含义：
   即便原作者团队，这套机制的保真元理论也花了 16 年才理顺——轻量实现应定位为
   不完备的启发式细化（像 TypeScript 那样），不要背负保真承诺**；
4. **性能**：occurrence typing 本身开销平凡（语法导向 + 小命题规范化）。Typed
   Racket 的著名性能灾难（渐进类型化某些配置慢 90–121 倍，Takikawa POPL'16）
   来自**运行时契约包装**，与 occurrence typing 无关——LSP 无运行时边界，无此
   负担；
5. 作者整体反思：SNAPL'17《Migratory Typing: Ten Years Later》。

#### 2.5 落地设计（裁剪决策表）

| λ_TR 概念 | DSL 对应物 | 一期决策 |
|---|---|---|
| 类型 τ | 谓词符号树节点 + 候选并集 | 已有 |
| then 分支 | `restrict(σ,τ)` = 候选集**过滤**（保留 <:τ 的分量，全失败退化为 τ） | **实现核心，几十行** |
| else 分支 | `remove(σ,τ)` = 候选集**删除**（σ<:τ 得 ⊥ ⟹ 分支标不可达） | **实现核心** |
| and/or/not | 小命题代数：and 假命题 = 析取、or 真命题 = 析取；从 if 展开自然推导 | 只处理**单层谓词组合**，不实现蕴含/路径 |
| 潜在命题（用户谓词函数） | 谓词类型声明表（alist） | **一期放弃**；二期支持"谓词类型声明表" |
| 对象/路径 π(x)（car/cdr 细化） | `inner:pair?` 有分量类型，可加 car/cdr 两路径 | **一期放弃**（ICFP'10 §7.1 可变性问题随之规避） |
| 命题环境 Γ,ψ⁺/ψ⁻ | 分支环境 = 原替换环境副本 + 每变量收窄后的类型；两分支各持一份，join 点取并集 | 与现有 substitution 机制天然兼容 |
| 可变性 | 对 `set!` 目标（DSL 已能识别）禁用细化 | 照搬 TR 方案 |
| cond 多子句 | 顺序累积 else 命题 | **二期**；一期只做单 if 即可覆盖多数 LSP 场景 |

接入点：类型 pass 已是 expansion-driven（chez-ir->annotated-sexp + 虚拟文档），
细化作用于**展开后的 if 形式**，与 macro-expansion.sls 同层。语义定位明示：
**细化是"提示增强"而非"错误保证"**——细化失败（类型没有变窄）永远静默，不会报
假错。

---

### §3 Conditional Types + 宏类型规则——with-gate 的理论谱系

**核心一手资料**：
ALW94（[DOI 10.1145/174675.177847](https://doi.org/10.1145/174675.177847)；
**原文未能获取**——Stanford 主页/ACM DL/archive.org 均不可达；以下基于
[Jensen POPL'98 §6 逐字技术描述](http://web.cs.ucla.edu/~palsberg/tba/papers/jensen-popl98.pdf)
+ [Trifonov & Smith POPL'96 全文](https://flint.cs.yale.edu/trifonov/papers/subcon.pdf)）；
[Typed Clojure 博士论文全文](https://thesis.ambrosebs.com/)
（Part III Ch16–19 精读）；
[Advanced Macrology SFP'07 全文](https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf)。

#### 3.1 Conditional types：`(T→S | C)` 的确切语义

ALW94 的类型语言：函数类型、构造子类型、**并/交/递归类型** + 条件类型构造子 `?`。
推断结果为 `∀ᾱ. τ where C`（C = 限制类型变量实例化的子类型约束集）。

条件类型 `τ?φ` 的语义（Jensen §6 逐字描述）：**只有当 guard φ 非空（可满足）时，
结果才具有类型 τ；若 φ 归约为空类型，整个条件类型消去**。Jensen 称之为
"conjunction across types"（跨类型合取）。

完整 worked example（Jensen 引述）：`λy. case y of true→zero, false→succ(zero)`
推断为：

```
∀α. α → (zero?(α∩true)) ∪ (succ(zero)?(α∩false))  where {α ⊆ true∪false}
```

α 实例化为 `true` 时 `α∩false` 归约为空 ⟹ 该分支从类型中删除 ⟹ 整体归约为
`true→zero`。**guard 不可满足 ⟹ 分支消去**——这就是 DSL 中 `with-equal?` 门失败
时静默回退、不产生替换的理论原型（30 年先例）。

约束与求解：推断归结为**类型包含约束的可解性**（Aiken & Wimmers FPCA'93 的扩展，
为处理 union types）。Trifonov & Smith《Subtyping Constrained Types》系统化
`∀ᾱ.τ\C` 的子类型化，三层次：操作语义 ≤∀obs / 语义 ≤∀sem / 可判定近似 ≤∀dec；
要点：(SUB) 规则是唯一允许把约束集纳入考虑的规则；约束集可满足性是分支存亡的
判定核心。2024 年《Ill-Typed Programs Don't Evaluate》明确师承 ALW94 的单侧系统，
但指出 ALW 约束"far more expressive"——**高表达力以求解复杂度为代价**。

与 intersection types 的关系：交/并是原生成分；条件类型不是交类型的特例，而是其
**跨类型推广**（guard 与被守卫类型可属不同类型）。

#### 3.2 谱系：谁继承了条件类型

软类型文献是**两条独立谱系**（[Soft Contract Verification §5](https://arxiv.org/abs/1307.6239)）：

1. **HM 推广路线**：Cartwright & Fagan PLDI'91 → Wright & Cartwright TOPLAS'97 →
   **Aiken et al. POPL'94**（条件类型）；
2. **set-based 抽象解释路线**：Flanagan PLDI'96 → Flanagan & Felleisen
   （MrSpidey）——**类型语言中没有条件类型构造子**。

条件类型思想的真正"复兴"在 occurrence typing：ICFP'10 把"谓词测试为真/假时
参数获得什么类型"形式化为函数类型上的**潜在命题**——`(T→S|C)` 的现代可组合
版本，被 Typed Clojure 原样继承。TR 现役还有 `#:pre` 前置条件特性
（错误信息如 "unable to prove precondition: `(<=2 (vector-length a))`"）。

**对 DSL 的直接映射**：`(with ((a b c)) …)` ≈ `∀ᾱ. τ where C` 的约束头；
`(with-equal? inner:list? a b)` ≈ `τ?φ` 的 guard（类型变量间的相等/可满足性
约束）。guard 作为类型规则一等成分有 30 年先例。Trifonov & Smith 的教训：
**guard 语言保持小且可判定**（DSL 现在只有类型相等 + 类型谓词，恰好是对的）。

#### 3.3 宏的自定义类型规则：两条工业路线

**路线 a（Typed Racket 2007，SFP'07 全文）**：类型检查发生在**全展开后**的 core
form 上（原话："generally impossible to derive type rules for arbitrary
macros"）；用户**不能**注册规则。跨展开通信靠 **syntax property 协议**：typed
绑定宏展开时把类型塞进标识符的 `'type-label` property，检查器 `get-id-type`
读回；`require/typed` 的 contract 包装打 `'TRUST` property。宏与检查器通信三
机制：static bindings（syntax-local-value）、副作用、syntax properties。

**路线 b（Typed Clojure 2019，博士论文 Part III 全文）**：**交错展开**——自研
analyzer 提供 `unanalyzed` 与 `analyze-outer`（只展开一层）；检查器主循环对
未分析节点按**解析后的 head symbol 分发**——命中已注册规则（如
`clojure.core/cond → check-special-cond`）就对**未展开的 surface form** 直接
检查；否则再展开一层。另有 `do-special-form` 语法协议承载宏→检查器的常量信息
（选 `do` 位置因为元数据在展开中会丢、常量信息可被编译消去）、求值期副作用
通信（AOT 下展开期副作用会丢）、wrapper 宏（对展开结果太复杂的宏复制展开逻辑
+ 插入注解）。Ch 19 如实讨论展开顺序/副作用/重复展开的正确性前提。

**与 scheme-langserver 的对应**：generator/router 按 head 分发 + type 侧
expansion-driven 推断（beta 归约 + 虚拟文档）**已经是路线 b 的同构实现**；
Typed Clojure Ch19 对展开顺序/副作用/重复展开的讨论对本项目"负缓存 + 展开预算"
的工程决策是**直接先例**。缺的只是"规则可注册"的接口设计与 soundness 讨论
框架。

学术进阶设计（若哪天想摆脱每族宏手写模板）：
[Turnstile](https://docs.racket-lang.org/turnstile/)（规则 = 展开 + 检查合一，
一致性自动）、SoundX（先检查后去糖）、Ziggurat（宏的静态/动态语义分离定义，
"类型规则塔"）、[Type Tailoring ECOOP'24](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2024.44)
（宿主宏细化类型）、SweetT（规则自动推断）。

#### 3.4 结论

DSL 的 with-gate **处于 30 年谱系上**，不是野路子；形式化时借鉴 `τ?φ` 的
"guard 满足性驱动分支存亡"语义与 latent proposition 的对象-路径表示（λTC 的
π(x)）。guard 语言保持小且可判定是已被验证的设计决策。

---

### §4 Semantic Subtyping——重写 `type:→?` 的算法蓝本

**核心一手资料**：
[FCB LICS'02 全文](https://xml.coverpages.org/Frisch-lics02.pdf)（JACM'08 前身，
理论相同；JACM 正式版被反爬未获全文）、
[Castagna primer arXiv:1809.01427 §4.2](https://arxiv.org/html/1809.01427v8)、
[Frisch 博士论文 Ch.7/§11.3/§11.5](https://www.cduce.org/papers/frisch_phd.pdf)、
[Kent 教程全文](https://pnwamk.github.io/sst-tutorial/)（Redex 模型，**Scheme 方言
写的，是最合适的对照蓝本**）、
[jclark/semtype 源码](https://github.com/jclark/semtype)（≈2600 行 Ballerina，
core.bal/function.bal/bdd.bal 等已精读）、
[ICFP'16 tallying](https://www.irif.fr/~gc/papers/icfp16.pdf)。

#### 4.1 理论核心

类型 = 值集合，子类型 = 集合包含（Hosoya/Pierce 在 XDuce 首创正则树语言版本；
FCB 推广到高阶函数 + 任意布尔组合 + 递归类型）。论域 D 划分 D_basic / D_prod /
D_fun 三个**互不相交**分片；∨∧¬ 解释为并/交/补。

**bootstrap 循环**：函数是值，判断函数值是否属于 `t→s` 需要对函数体做类型检查，
而类型检查又依赖子类型——定义循环。解法：先取非计算性 bootstrap 模型定义子类型
与类型系统，再定义操作语义，最后证明**良类型值集合 V 本身构成等价模型**
（LICS'02 Theorem 4.1）。

**核心判别定理（LICS'02 Theorem 2.17，箭头判定的完备规则）**：

```
∩ᵢ(tᵢ→sᵢ) ⊆ ∪ⱼ(t'ⱼ→s'ⱼ)
⟺ ∃j. [ t'ⱼ ≤ ∨ᵢ tᵢ  ∧  ∀I′⊆I. ( t'ⱼ ≤ ∨_{i∈I′} tᵢ ) ∨ ( ∧_{i∈I∖I′} sᵢ ≤ s'ⱼ ) ]
```

即对正箭头集合的所有子集组合做分裂——**语义子类型相对树匹配多出来的主要复杂度
来源**。实用推论：`(t₁∨t₂)→s ≃ (t₁→s)∧(t₂→s)` 等分配律在语义模型中自动成立
——ad-hoc 树匹配必须手工枚举这类等式且容易漏。

**判定算法（primer §4.2 四步）**：

1. `S <: T` 归约为空集判定：`isEmpty(diff(S,T))`——**差与交不是可选增强，是
   算法入口**；
2. 布尔表达式化 DNF（每子句 = 正原子合取 ∧ 负原子合取）——DNF 不是中间产物
   而是类型的规范表示；
3. 按种类分片（乘积只跟乘积比，箭头只跟箭头比）；
4. 分片内按构造器分解：积逐分量协变 + 负原子子集枚举；箭头用 Thm 2.17；递归
   类型用模拟关系的余归纳不动点。

**递归类型**：类型代数定义为递归类型代数；**正则性**（类型的"基"传递闭包有限）
是终止性关键。equirecursive（CDuce：hash-consing DAG + 带回溯/免回溯的归纳谓词
计算 + cache）与 isorecursive 均可。

#### 4.2 开源实现

**jclark/semtype**（Ballerina 实现，源码精读，≈2600 行）：

- 值域划分 22 个 "uniform types"（可选 readonly/read-write 拆分）；SemType =
  32 位位集或 `ComplexSemType{all, some, subtypeDataList}`；
- 每个 uniform type 一种 SubtypeData：简单类型用布尔；int/string/boolean 用
  专门区间/集合；**list/mapping/function 用 BDD**（三叉决策图 `Node{atom,left,
  middle,right}`——二叉版本 union 会指数复制，三叉版本 union 线性；即 Frisch
  论文 §11.3 的"arbres de décision ternaires"）；
- 每构造器五个运算：`union/intersect/diff/complement/isEmpty`；顶层
  `isSubtype = isEmpty ∘ diff`；
- 函数空集判定 = Thm 2.17 直接实现（`functionBddIsEmpty` 沿 BDD 累积正箭头域
  并集，`functionTheta` 做子集分裂）；
- 递归类型：`Env` 持 defs 数组，先插 dummy 再回填（指针式 equirecursive）；memo
  表按 BDD 节点缓存；多参数函数去糖成单参数 tuple → 单返回值。

**CDuce**（OCaml，论文级）：0.2 版约 20000 行；三叉决策树 + hash-consing；算法 =
 归纳谓词 Φ 计算 + cache，免回溯变体是论文贡献之一。**Kent 教程**（Redex 模型 +
  伪代码 + QuickCheck 式测试策略）：类型 = `(Base, Prod-BDD, Fun-BDD)` 分片三元组；
  **lazy BDD**（union 不合并子树、节点挂 union 列表）；subtyping/isEmpty、积/箭头
  inhabitation、函数应用全套伪代码。**这是 Scheme 重写最合适的对照蓝本**。

**tallying 与 OCaml**：OCaml 本体**没有** tallying（grep ctype.ml 确认）——
polymorphic variants 用 kinding constraints 模拟子类型；tallying 是集合论文献的
约束求解算法（`tally(D)` 产生有限主解完备集），每个分支判定以 ground 语义子类型
为 oracle。**对无多态、无类型变量的轻量推断完全用不上**——只在引入 ∀-多态时才
需要。

#### 4.3 与我们系统的硬性差距（源码级对照后得出）

语义子类型判定要求**布尔代数 + 每构造器五运算**。对照现状：

| 必需 | 现状 | 结论 |
|---|---|---|
| 显式并 ∪ | 分散在替换候选层 | 需提升为类型节点内的 first-class 表示 |
| 空 ⊥ 与顶 ⊤ | `inner:void?`/`something?` 近似有 | `something?` 是"未确定"语义，与 ⊤（全部值的集合）不同，需精确化 |
| 补/差 ¬∖ | **完全没有** | **硬缺口**——无它无法归约到空集判定 |
| 交 ∩ | **完全没有** | 箭头/积分解都要求 |
| 每构造器 isEmpty | 无 | pair/list/vector/lambda 各写一个 |
| 值域不相交分片 | `inner:pair?` 与 `inner:list?` **语义重叠**（AGENTS.md 记载的老陷阱） | **重写中最大的语义设计工作**——集合语义不能容忍重叠，必须先重写为不相交原子（proper-list / improper-pair / empty-list 三分） |

**只取并 ∪ + ⊤ + 函数类型的子集可行吗？不可行**——判定 `S<:T` 的第一步就是
构造 `S∧¬T` 判空；没有否定/差，只能回到不完备的语法规则。

**折中**：交集可以只在内部表示里存在，不出现在用户可见语法——Kent 教程/semtype
正是此路线（表面只有并，内部自由产生交/补；与"替换候选列表 = DNF 子句"恰好
同构）。**放弃函数类型的完备判定**（箭头先用单条近似规则：dom 反变 + cod 协变 +
dom 非空，即 Flow/TS 的选择），换可靠但不完备的判定——对 LSP 提示/诊断场景是
最优性价比起点。

#### 4.4 重写路线建议

1. 谓词语义细化为**互不相交的值域分片**（Scheme 值域：null / pair / vector /
   char / string / number / symbol / procedure / record / void / eof …；
   `inner:pair?` vs `inner:list?` 重叠必须解决）；
2. 内部表示改为 `(basic-位集, pair-BDD, lambda-BDD, vector-BDD, …)` 或 lazy BDD；
   基本类型用位集（Scheme 基本类型天然有限枚举，比 int 区间简单）；
3. 每构造器五运算 + isEmpty；`sem:subtype? = isEmpty ∘ diff`，memo 化
   （注意 AGENTS.md 线程安全教训：memo 表 per-thread 或加锁）；
4. 表层继续输出现有谓词符号树（打印语法），**只重写内部相容性判定**——对 LSP
   协议输出零侵入；
5. 测试策略抄 Kent 教程：随机生成类型 + 集合公理 property-based 测试
   （交换/结合/分配/德摩根、S∖T=∅ ⟺ S≤T）——CDuce/semtype 都验证了这套方法
   能抓出单测抓不到的 bug。

#### 4.5 性能证据

- CDuce 实测：450 行程序（~3500 内部类型节点）编译 <0.2s，其中约一半在子类型
  算法；子类型判定被调用 19,956 次，共 70,999 次内部迭代（Frisch 论文 §11.5，
  "理论最坏指数、实践毫秒级"论点的原始出处）；
- 下界：树自动机包含 EXPTIME-complete；指数行为主要由箭头判定的子集分裂与
  否定引起；
- semtype 无正式基准；worst-case 内部循环 O(2^|N|)/O(2^|P|)，memo 表使重复
  子问题多项式化——LSP 规模的调用量（≤10⁵ 次）完全无压力；
- **真正的风险不是性能，是 pair/list 重叠的语义重设计**。

---

#### 4.6 内部表示升级实施方案

> 本节把 §4 的调研结论落成可执行计划。核心原则：**只换判定层，不动上层**。

##### 分层影响面

```
rnrs-meta-rules（数据）→ generator（生成 substitution）→ type:→?（相容性判定，ad-hoc 树匹配）
                                                              ↑ 只有这一层被替换

升级后：
rnrs-meta-rules（不动）→ generator（不动）→ type:→? 改为：谓词树 → SemType → isEmpty(diff)
```

substitution 生成、candy 匹配、LSP 输出全部不动。`type:→?`（interpreter.sls:70-113）
里现有的 `inner:?->pair` + `candy:match-left` 结构比较，换成"转换 + 集合判定"，
判完 SemType 即弃（或缓存）。`inner:?->pair`（含其 30 年模式变量 quirk）随之退役。

##### 第一步：pair/list 重叠的解法——换 primitive

集合论语义要求值域不相交分片，而 `inner:pair?` 与 `inner:list?` 语义重叠
（非空 proper list 的脊柱就是 pair）。解法不是强行拆分，而是换 primitive：

```
pair    = 原始积构造器 (T1 × T2)            ← 唯一的 pair 表示
null    = 基类型 ('())
list(T) =def μα. (pair T α) ∪ null         ← 派生递归类型，表面语法保留
```

`inner:list?` 只是 `(inner:pair? T ...)` 链 + `null` 的语法糖，规则库一行不改。
数值塔同样展开为叶谓词的并（fixnum/bignum/flonum/ratnum/cflonum 各占一个 bit；
`integer? = fixnum ∪ bignum`，`real? = integer ∪ flonum ∪ ratnum`，依此类推）。

##### 内部表示（Kent 教程路线）

```scheme
;; SemType = 分片记录（各分片互不相交；Scheme 基类型是有限枚举，
;; 比 Ballerina 的 int 区间简单，直接用位集）
(define-record-type sem:type
  (fields base-bits    ; fixnum 位集
          pair         ; BDD 节点或 #f（car/cdr 各为一个 SemType）
          arrow        ; BDD 节点或 #f（域/值域 SemType）
          vector))     ; BDD 节点或 #f（元素 SemType）

;; BDD = 三叉决策图 #(atom left middle right)，按 atom 严格降序保证共享
;; （二叉版本 union 指数爆炸，三叉线性——semtype 的 bdd.bal 与
;; Frisch 论文 §11.3 "arbres de décision ternaires" 同款）
```

每个构造器实现五运算：`union / intersect / diff / complement / isEmpty`。
判定入口一句话：`sem:subtype?(S,T) = isEmpty(diff(S,T))`（primer 四步算法的 Step 1）。

##### 分期落地（每期独立验证）

**Phase 0：差异测试先行（~1 天）**。抄 Kent 教程 §6 的测试策略：随机生成谓词树
类型 + 集合公理（`S∖T=∅ ⟺ S≤T`、交换/结合/分配/德摩根），先对**现有 `type:→?`**
跑一遍，记录所有违背公理的案例——这是重写的验收基线，也是语义设计错误的探测仪。

**Phase 1：纯基类型（~2 天，~150 行）**。SemType 退化为 fixnum 位集：

```scheme
(define (sem:subtype? s t)
  (fxzero? (fxand (sem:type-base-bits s) (fxnot (sem:type-base-bits t)))))
```

覆盖所有非容器比较（number?/string?/谓词 reify 后的 identifier-reference——
按标识符名查基类型表）。这一期就能替换 `type:→?` 的大部分调用点。

**Phase 2：pair + null + vector（~1 周）**。引入 BDD；`inner:list?` 糖展开；
`inner:pair?` 进积构造器分解（primer Step 4：逐分量协变 + 负原子子集枚举）。
pair 的 isEmpty 就是"两个分量都非空"。

**Phase 3：箭头（~1 周）**。lambda `(ret <- params)` 进 arrow 分片。**先用近似单
规则**（dom 反变 + cod 协变 + dom 非空——Flow/TS 的选择），不实现 Thm 2.17 的
指数子集分裂；留升级口。

##### 代码落点

新子目录 `analysis/type/semantic-subtyping/`（遵守分层规则，只被 analysis/type 引）：

```
semtype.sls    ; SemType record + 位集运算 + 五运算分发 + memo
bdd.sls        ; 三叉决策图
convert.sls    ; 谓词树 → SemType（带缓存；identifier-reference 按名查基类型表）
api.sls        ; sem:subtype? / sem:equivalent? / sem:restrict / sem:remove
```

interpreter.sls 的 `type:→?` 改调 `sem:subtype?`。**方向②的 restrict/remove 在此
直接变成 `sem:intersect` / `sem:diff`**——"②为④铺路"的具体含义：做②时接口就按
这个签名留。

##### 风险清单（按真实程度排）

1. **memo 表线程安全**——AGENTS.md 记载过全局弱 eq 哈希表多线程下崩过的教训，
   per-thread（reference.sls 的 make-thread-parameter 先例）或加锁；
2. **`something?` 语义**——是"未确定"而非 ⊤；转换层区分：未确定 ⟹ 不做判定直接
   通过（保持现有宽松行为）；
3. **`inner:void?`**——"无返回值"标记而非底类型，作特殊 base bit，不参与 complement；
4. **模板段修饰符 `...`/`**1`**——不是类型；到达 SemType 的只有解释完毕的类型，
   但要用断言钉死这个假设；
5. 性能无忧（CDuce 实测 ~2 万次判定 <0.1s，本项目量级更小）——**风险全在语义
   设计**，所以 Phase 0 差异测试不是可选项。

---

### §5 综合建议

| 方向 | 用户可见收益 | 工程量 | 风险 | 建议 |
|---|---|---|---|---|
| ② occurrence typing 简化版 | **大**（根治 something? 泛滥） | 小（restrict/remove 几十行 + if 规则分支环境） | 低（启发式定位，元理论无负担） | **第一个做** |
| ① success typing 判据 post-pass | 中（新增一类零误报诊断） | 小（~200 行交集检查） | 低 | 第二个做 |
| ③ 形式化 + 规则注册接口 | 小（理论背书 + 可扩展性） | 中 | 低 | 随文档/重构做 |
| ④ 内部表示升级 | 大（长期） | **大**（值域不相交化是语义重设计） | 中高 | 单独立项，按 §4.6 分期实施（Phase 0 差异测试先行） |

收敛信号：四个方向在文献中互相引用、最终都汇到 Castagna 的集合论框架
（POPL'22 把 occurrence typing 归约到 union elimination；success typing 的类型格
是 semantic subtyping 的特例）。长期看 ②的 restrict/remove 与 ④的 meet/diff
是同一套原语——做②时把 restrict/remove 实现为"候选集操作 + 留交集接口"，就是
在为④铺路。

---

### 附录 A：阅读完整性声明

**§1 success typing**：✅ 全文精读：PPDP'06 核心论文、TypEr、contracts、
PEPM'13 错误切片、Bugs'05 主体、LYSE、etylizer、OTP man page/User's Guide；
⚠️ 部分：Jakob & Thiemann arXiv 扩展版（引言/形式化/λC/深度截断示例/结论）；
❌ 仅摘要：LPAR'18 多态 success types、Erlang'21 十五周年报告、FLOPS'16。

**§2 occurrence typing**：✅ 全文：POPL'08、ICFP'10（图 1–13 为图片不可读、
§7.4 末尾一小段截断）；其余读摘要 + 关键章节转述。2026 元理论再检视论文
（arXiv:2609.16299）为转述。

**§3 conditional types + 宏规则**：✅ 全文：Advanced Macrology SFP'07、
Typed Clojure 博士论文（Part III Ch16–19、Ch25 精读）、Jensen POPL'98、
Trifonov & Smith POPL'96、TR ts-guide；❌ 未获全文：ALW94 POPL'94 原文
（Stanford 主页/ACM DL/archive.org 不可达，已用 Jensen §6 逐字描述 +
Trifonov & Smith 补全）、Flanagan/Felleisen SBA 原文（镜像 404，谱系结论依据
Soft Contract Verification §5）。

**§4 semantic subtyping**：✅ 全文/源码：FCB LICS'02 前身全文、Castagna primer
§4.2、Frisch 论文 Ch.7/§11.3/§11.5、Kent 教程全文、semtype README/NOTES/TODO/
core.bal（635 行）/function.bal/mapping.bal 前 200 行/list.bal 前 60 行/
bdd.bal 前 100 行、ICFP'16 主要章节；❌ 仅摘要/元数据：FCB08 JACM 正式版
（HAL 被反爬）、Castagna 2022 教科书（仅参考文献页）、CDuce OCaml 源码本身。

### 附录 B：全部来源 URL

#### §1 success typing
- 核心论文：https://user.it.uu.se/~kostis/Papers/succ_types.pdf
- TypEr：https://user.it.uu.se/~kostis/Papers/typer.pdf
- Contracts：https://user.it.uu.se/~kostis/Papers/contracts.pdf
- Bugs'05：https://user.it.uu.se/~kostis/Papers/bugs05.pdf
- Falsification：https://arxiv.org/abs/1502.01278
- 错误切片 PEPM'13：https://riunet.upv.es/bitstreams/bf4bc430-0b7a-4c17-bb85-0ba44cf23347/download
- 多态 success types LPAR'18：https://easychair.org/publications/paper/FzsM/open
- 官方文档：https://www.erlang.org/doc/apps/dialyzer/dialyzer.html
- etylizer：https://arxiv.org/html/2302.12783v2
- OTP 源码：https://github.com/erlang/otp/tree/master/lib/dialyzer/src
- LYSE：https://learnyousomeerlang.com/dialyzer
- ElixirLS：https://github.com/elixir-lsp/elixir-ls

#### §2 occurrence typing
- POPL'08：https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf
- ICFP'10：https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf
- Castagna POPL'22：https://www.irif.fr/~gc/papers/popl22.pdf
- ifT-benchmark：https://github.com/utahplt/ifT-benchmark
- TS narrowing：https://www.typescriptlang.org/docs/handbook/2-narrowing.html
- Flow refinements：https://arxiv.org/abs/1907.05590
- OTMT PLDI'16：https://dl.acm.org/citation.cfm?id=2908091 （arXiv:1511.07033）
- 元理论再检视 2026：arXiv:2609.16299
- TR 模型：https://github.com/samth/popl08-model
- TR 性能：https://janvitek.org/pubs/jfp18.pdf
- SNAPL'17：https://www2.ccs.neu.edu/racket/pubs/typedracket.pdf

#### §3 conditional types + 宏规则
- ALW94：https://doi.org/10.1145/174675.177847
- Jensen POPL'98：http://web.cs.ucla.edu/~palsberg/tba/papers/jensen-popl98.pdf
- Trifonov & Smith：https://flint.cs.yale.edu/trifonov/papers/subcon.pdf
- 软类型两谱系：https://arxiv.org/abs/1307.6239
- Typed Clojure 论文：https://thesis.ambrosebs.com/ 、
  https://ambrosebs.com/files/ambrosebs-phd-thesis.pdf
- Advanced Macrology：https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf
- TR ts-guide：https://docs.racket-lang.org/ts-guide/occurrence-typing.html
- TR ts-reference：https://download.racket-lang.org/releases/8.17/pdf-doc/ts-reference.pdf
- Turnstile：https://docs.racket-lang.org/turnstile/
- Type Tailoring：https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2024.44

#### §4 semantic subtyping
- FCB LICS'02 全文：https://xml.coverpages.org/Frisch-lics02.pdf
- FCB08 JACM 元数据：https://hal.science/hal-00336120 （DOI 10.1145/1391289.1391293）
- Castagna primer：https://arxiv.org/abs/1809.01427
- Frisch 博士论文：https://www.cduce.org/papers/frisch_phd.pdf
- Kent 教程：https://pnwamk.github.io/sst-tutorial/ （Redex 模型
  https://github.com/pnwamk/sst-tutorial/）
- semtype：https://github.com/jclark/semtype
- Gentle Intro：https://www.cduce.org/papers/gentle.pdf
- ICFP'16 tallying：https://www.irif.fr/~gc/papers/icfp16.pdf
- OCaml ctype.ml（确认无 tally）：https://github.com/ocaml/ocaml/blob/trunk/typing/ctype.ml
- Ballerina semtype 生产移植：https://github.com/ballerina-platform/ballerina-lang/issues/42288
