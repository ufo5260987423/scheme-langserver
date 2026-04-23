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

> For the list-pattern matcher used when applying lambda types and macro heads, see [`syntax-candy.md`](syntax-candy.md).

---

## 2. The Type Expression DSL

Type expressions are ordinary Scheme S-expressions. There is no separate parser. The DSL is validated and manipulated by predicates in `domain-specific-language/inner-type-checker.sls`.

### 2.1 Atoms

| Form | Meaning |
|------|---------|
| `something?` | The top type (universal). Every value inhabits it. |
| `void?` | The type of `(void)`. |
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
| `meta` + symbol `let` / `let*` / `letrec` / `letrec*` | `let-process` / `let*-process` / `letrec-process` |
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

### 4.4 `let.sls` / `letrec.sls` – Binding Forms

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

`letrec-process` is almost identical to `let-process`; the key difference is that the loop identifier is not present, so no extra lambda wrapper is built.

### 4.5 `if.sls` – Conditional

For `(if cond then else)`:

- `cond` gets `something?` (we do not attempt boolean-truthiness analysis).
- `then` and `else` are each attached to the `if` node, and the `if` node is attached back to both branches. This encodes a **union type**: the value of the `if` may be either branch.

For two-arm `if`, the union has two members; for one-arm `if`, only the `then` branch is attached.

### 4.6 `define.sls` – Top-Level & Internal Definitions

Two shapes:

1. `(define (f p₁ p₂ …) body)` – like `lambda`, but the resulting function type is attached to the *identifier* node of `f`, not to the `define` node.
2. `(define var init)` – bidirectional link between `var` and `init`.

### 4.7 `case-lambda.sls` – Overloaded Arity

Each clause `(((p₁ p₂ …) body))` generates an independent function type. All clauses are attached to the root `case-lambda` node, producing a list of alternative types. During interpretation the interpreter may resolve to any of them; in practice the call site arity usually selects the correct one.

### 4.8 `record.sls` – `define-record-type`

Collects the `constructor`, `predicator`, `getter`s and `setter`s exported by the record definition and assigns them canonical types:

- `predicator` → `(boolean? <- (inner:list? something?))`
- `constructor` → `(predicator <- (inner:list? something? …))`
- `getter` → `(something? <- (inner:list? predicator))`
- `setter` → `(void? <- (inner:list? predicator something?))`

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

### 4.11 Self-Defined Rules (`ufo-match`, `ufo-try`)

The router in `self-defined-rules/router.sls` allows external libraries to register custom rules. Currently supported:

- **`ufo-match`** – `match-process` recognizes `(? pred var)` patterns and attaches all available predicate identifiers (symbols ending in `?`) to the pattern variable.
- **`ufo-try`** – `try-process` attaches union types for exception clauses.

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
- It is an atom (symbol, identifier-reference, `'something?`, `'void?`).
- It is a list, and every element is solved, **and** the list itself is an `inner:trivial?` type expression (i.e. not a raw application waiting for reduction).

`type:partially-solved?` counts how many leaf nodes are solved; it is used by the Cartesian-product pruner.

---

## 6. The Macro Engine

Some R6RS primitives cannot be expressed as simple function types. `car`, for example, must return the *first element type* of a list, but the list's length is unknown at compile time. The system solves this with a small macro language embedded in the type DSL.

### 6.1 `with` – Pattern Abstraction

```scheme
(with ((template) ...) body) arg1 arg2 …
```

`template` is a pattern using ordinary symbols and the segment markers `...` / `**1`. `body` is the macro body. When the interpreter sees a macro application, it:

1. Interprets the arguments to obtain their type-level values.
2. Calls `candy:matchable?` to see if the templates match.
3. Uses `candy:match-left` to produce a list of `((symbol . value) …)` pairs.
4. Substitutes these pairs into `body` via `private-with`.
5. Interprets the resulting body.

### 6.2 Built-In Macro Helpers

| Macro | Semantics |
|-------|-----------|
| `with-append` | `(with-append list1 list2)` → appended list |
| `with-equal?` | `(with-equal? a b body)` → `body` if `a` equals `b`, else the original macro expression (match failure) |

### 6.3 Example: `car`

```scheme
(car
  (with ((a b c **1))
    (with-equal? inner:list? a b)))
```

When applied to `(inner:list? fixnum? number?)`:

- Pattern `((a b c **1))` matches the single argument.
- `a = inner:list?`, `b = fixnum?`, `c = (number?)`.
- `with-equal?` checks `a == inner:list?` → true.
- Result is `b` → `fixnum?`.

### 6.4 `candy:matchable?` – Dynamic-Programming Matcher

The matcher lives in `syntax-candy.sls`. It segments a list into alternating **fixed** and **repeatable** sections. A segment is delimited by `...` or `**1`.

Matching is performed by filling a 2-D matrix (rows = template segments, columns = argument segments) with three statuses:

- `'matched` – both segments align.
- `'skipped` – one side is skipped (used for zero-or-more).
- `'unused` – unreachable cell.

The matrix is explored depth-first, allowing paths that step right or down but never diagonally. This is essentially a restricted regular-expression matcher for heterogeneous lists. It supports overlapping repeatable segments, which is needed for types like `(inner:list? number? ... string? ...)`.

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

### 9.2 Incremental Updates

When a file is changed:

1. `update-file-node-with-tail` rebuilds the document's `index-node-list`.
2. `refresh-workspace-for` determines whether the library header changed. If not, only the affected file's substitutions are regenerated.
3. `shrink-paths` computes topological batches; `init-references` re-runs `step` on the affected batch.

### 9.3 From Types to LSP Diagnostics

Currently the type system is used primarily for **hover information** (not yet for full `publishDiagnostics`). The `textDocument/hover` handler can look up an identifier's `type-expressions`, map them through `type:interpret->strings`, and display them to the user. The old documents listed "TODO: Diagnostic based on Type System"; the infrastructure is in place, but the actual diagnostic rules (type-mismatch errors) are not yet implemented.

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
| `analysis/type/substitutions/rules/letrec.sls` | `letrec`, `letrec*`. |
| `analysis/type/substitutions/rules/if.sls` | Conditional union types. |
| `analysis/type/substitutions/rules/define.sls` | `define` for variables and procedures. |
| `analysis/type/substitutions/rules/case-lambda.sls` | Overloaded arities. |
| `analysis/type/substitutions/rules/record.sls` | `define-record-type` types. |
| `analysis/type/substitutions/rules/cond.sls` | Multi-branch conditional. |
| `analysis/type/substitutions/rules/begin.sls` | Sequencing. |
| `analysis/type/substitutions/self-defined-rules/router.sls` | Extensible rule dispatcher. |
| `analysis/type/substitutions/self-defined-rules/ufo-match/match.sls` | `ufo-match` pattern support. |
| `analysis/type/domain-specific-language/interpreter.sls` | `type:interpret`, `type:->?`, macro expander, Cartesian-product pruner. |
| `analysis/type/domain-specific-language/inner-type-checker.sls` | Predicates: `inner:trivial?`, `inner:lambda?`, `inner:macro?`, `inner:executable?`, `inner:?->pair`. |
| `analysis/type/domain-specific-language/syntax-candy.sls` | `candy:matchable?`, `candy:match-left`, `candy:match-right`, segment-based DP matcher. |
| `analysis/identifier/meta.sls` | `construct-type-expression-with-meta`, loader for `rnrs-meta-rules`. |
