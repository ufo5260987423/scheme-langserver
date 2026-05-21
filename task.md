# Task: Debug auto-expansion reference propagation for `(match x [(? string? path) path])`

## Goal
Investigate why the `path` identifier-reference from the `(? string? path)` pattern does **not** reach the original `path-node` after macro expansion analysis, when using auto-expansion for `ufo-match` macros.

## Background
- In `analysis/identifier/self-defined-rules/router.sls`, `ufo-match` macros use auto-expansion via `expansion-generator->rule`.
- Hand-written rules (commented out in router.sls) directly attach references to sub-nodes of the original macro call.
- Auto-expansion relies on `private:shallow-copy` in `expansion-wrap.sls` to back-propagate references from the expanded AST to the original macro-call nodes via `pairs` mapping.
- The expansion of `match` is a deep cascade (`match` -> `match-next` -> `match-one` -> `match-two` -> ... -> `match-check-identifier` -> `let`).

## Test File
- `tests/analysis/identifier/test-match-auto-resolve-no-ellipsis.sps`

## Experiment: Memory cap raised from 10 to 15

Modified `analysis/identifier/expanders/expansion-wrap.sls`:
```scheme
; Before: (< (length memory) 10)
; After:  (< (length memory) 15)
```

This allows the cascade to go deeper (observed depths up to 14).

---

## Complete Cascade Chain (with memory cap = 15)

### DEPTH 11: `match-check-identifier` — where `path` binding is born

**macro call:**
```scheme
(match-check-identifier path
  (let-syntax ((new-sym? (syntax-rules () ...)))
    (new-sym? random-sym-to-match
      (let ((path v)) ...)
      (if (equal? v path) ...)))
  (if (equal? v path) ...))
```

**expansion root:**
```scheme
(let-syntax ((sym? (syntax-rules () ((sym? path sk fk) sk) ((sym? y sk fk) fk))))
  (sym? abracadabra
    (let-syntax ((new-sym? (syntax-rules () ...)))
      (new-sym? random-sym-to-match
        (let ((path v)) ...)
        (if (equal? v path) ...)))
    (if (equal? v path) ...)))
```

**ALL exports in expansion tree:**
- `node: path -> exports: (path)`
  - This is the **real binding** from `(let ((path v)) ...)`.
- `node: (new-sym? path sk2 fk2) -> exports: (sk2 fk2 new-sym? path)`
  - Pattern variable in inner `syntax-rules`.
- `node: (sym? path sk fk) -> exports: (sk fk sym? path)`
  - Pattern variable in outer `syntax-rules`.

**mapping path exports:**
- `path -> mapped to callee: path [SUCCESS]`
  - The `let`-binding `path` successfully maps back to the `path` argument of the `match-check-identifier` call.
- `(new-sym? path sk2 fk2) -> mapped to callee: (new-sym? path sk2 fk2) [SUCCESS]`
- `(sym? path sk fk) -> mapped to callee: (match-check-identifier ...) [FALLBACK]`

---

### DEPTH 10: `match-two` — `path` survives one more layer

**macro call:**
```scheme
(match-two v path (x (set! x))
  (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure))
  (failure))
```

**expansion root:**
```scheme
(match-check-identifier path
  (let-syntax ((new-sym? ...)) ...)
  (if (equal? v path) ...))
```

**ALL exports in expansion tree:**
- `node: path -> exports: (path path)`
  - Carried over from depth 11.
- `node: (match-check-identifier ...) -> exports: (sk fk sym? path ...)`
  - Composite node accumulates deep references.

**mapping path exports:**
- `path -> mapped to callee: path [SUCCESS]`
  - Maps back to the `path` argument of the `match-two` call.
- `(match-check-identifier ...) -> mapped to callee: (match-two v path ...) [FALLBACK]`
  - The **composite expansion node** has no counterpart in `pairs`.

---

### DEPTH 9: `match-one` — the chain breaks

**macro call:**
```scheme
(match-one v path (x (set! x))
  (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure))
  (failure))
```

**expansion root:**
```scheme
(match-two v path (x (set! x))
  (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure))
  (failure))
```

**ALL exports in expansion tree:**
- `node: (match-two v path ...) -> exports: (sk fk sym? path ... x x x path path x x x)`
  - Composite node now carries a **huge blob** of accumulated references.
- `node: path -> exports: (path path)`

**mapping path exports:**
- `(match-two v path ...) -> mapped to callee: (match-one v path ...) [FALLBACK to init-node]`
- `path -> mapped to callee: (match-one v path ...) [FALLBACK to init-node]`

**Why FALLBACK?**

`pairs` for this layer:
```
expansion: v -> callee: v
expansion: match-two -> callee: match-two
```

There is **NO mapping** for:
- The composite node `(match-two v path ...)`
- The atom `path` inside the expansion tree

`shallow-copy` does `(assoc i pairs)` where `i` is the expansion node carrying `path` exports. Since `pairs` only contains `v` and `match-two`, `assoc` returns `#f`, and the reference falls back to the entire `match-one` call node.

---

### DEPTH 8: `match-two`

**macro call:**
```scheme
(match-two v (and path) (x (set! x))
  (match-drop-ids (begin path))
  (failure))
```

**expansion root:**
```scheme
(match-one v path (x (set! x))
  (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure))
  (failure))
```

**mapping path exports:**
- `(match-one v path ...) -> mapped to callee: (match-two v (and path) ...) [FALLBACK to init-node]`

Same reason: `pairs` has `path -> path` and `match-one -> match-one`, but the **composite node** `(match-one v path ...)` is not in `pairs`.

---

### DEPTH 7: `match-check-ellipsis`

**macro call:**
```scheme
(match-check-ellipsis path
  (match-extract-vars and (match-gen-ellipsis ...) () ())
  (match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure)))
```

**expansion root:**
```scheme
(let-syntax ((ellipsis? (syntax-rules () ...)))
  (ellipsis? (a b c)
    (match-extract-vars ...)
    (match-two v (and path) ...)))
```

**mapping path exports:**
- `(ellipsis? (foo path) sk fk) -> mapped to callee: (match-check-ellipsis ...) [FALLBACK]`
- `(match-two v (and path) ...) -> mapped to callee: (match-two v (and path) ...) [SUCCESS]`

Note: The composite node `(match-two v (and path) ...)` **does** have a mapping because the callee itself contains this exact sub-expression. But this just maps the reference to the **callee's sub-node**, not to the original `(? string? path)` pattern node.

---

### DEPTHS 6 -> 0

The same pattern repeats:
- Composite expansion nodes carrying `path` exports are **not in `pairs`** -> FALLBACK.
- Atom `path` occasionally maps successfully to a callee atom `path`, but that callee `path` is just another intermediate macro-call parameter, not the original pattern variable.

By the time the cascade reaches depth 0 (the original `(match x [(? string? path) path])` call), the `path` reference has been **trapped inside a chain of fallback nodes** and never reaches the specific `path-node` inside `(? string? path)`.

---

## Root Cause Analysis

### 1. `pairs` only maps atomic / positional nodes

`private:expansion+index-node->pairs` generates mappings like:
```
expansion: path -> callee: path
expansion: match-two -> callee: match-two
expansion: begin -> callee: begin
```

It **does not** create mappings for composite expression nodes like:
```
(match-two v path (x (set! x)) ...)
(match-one v path (x (set! x)) ...)
(match-check-identifier path (let-syntax ...) ...)
```

### 2. `path` exports attach to composite nodes

`step` creates `path` identifier-references on the composite node that represents the macro call or `let` binding. For example:
- `(match-check-identifier ...)` node exports `path`
- `(match-two v path ...)` node exports `path`
- `(match-one v path ...)` node exports `path`

These composite nodes are the ones that carry the accumulated references, but they are **absent from `pairs`**.

### 3. `shallow-copy` has no transitive propagation

Each layer's `shallow-copy` only copies references from the current expansion tree to the **direct callee**.

Even if a deeper layer successfully maps `path` to `callee: path [SUCCESS]`, that `path` node is just a parameter of the intermediate macro call. When the **next outer layer** runs `shallow-copy`, it looks at its own `pairs`, which again lacks the mapping for the composite node, causing another FALLBACK.

### 4. Memory cap is not the bottleneck

Raising the cap from 10 -> 15 allows the cascade to go 4 layers deeper, but the fundamental issue -- **missing composite-node mappings in `pairs`** -- remains unchanged. The `path` reference still cannot escape the intermediate layers.

---

## Root Cause Re-Analysis (2025-05-18)

After adding precise debug output to `syntax-rules.sls`, the **true root cause** was found.

### The `pairs-debug` reveals a broken `callee-compound`

**depth 10 `match-two`:**
```
macro call: (match-two v path (x (set! x)) (match-one v (and) ...) (failure) ())
callee-compound: (match-two v path (x (set! x)) (match-one v (and) ...) (failure) ())
pairs count: 26
```
`callee-compound` is a **proper list** with 7 elements. `private:expansion+index-node->pairs` aligns them 1-to-1, including `path -> path`.

**depth 9 `match-one`:**
```
macro call: (match-one v path (x (set! x)) (match-one v (and) ...) (failure) ())
callee-compound: (match-two . v)
pairs count: 2
```
`callee-compound` is a **pair with only 2 elements** `(match-two . v)`, while `expansion` is `(match-two v path (x (set! x)) (match-one v (and) ...) (failure) ())` with 7 elements.

`private:expansion+index-node->pairs` can only align the first two (`match-two` and `v`). `path`, `g+s`, `sk`, `fk`, `i` have no counterparts in `callee-compound`, so no pairs are generated for them.

### Why is `callee-compound` broken?

`match-one` has a catch-all clause:
```scheme
((match-one . x)
 (match-two . x))
```

Template is `(match-two . x)` -- a **pair-form**.

`expand->index-node-compound-list` expands this template using `bindings`. `bindings` for `x` is produced by `pattern+index-node->pair-list`.

Look at `pattern.sls` line 365-381:
```scheme
[(list-form vector-form pair-form)
  (let loop ([rest-patterns p-c] [rest-index-nodes i-c])
    (cond
      [(null? rest-patterns) '()]
      [(null? rest-index-nodes) ...]
      [else
        `(,@(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes))
          . ,(loop (cdr rest-patterns) (cdr rest-index-nodes)))]))]
```

For `(match-one . x)`:
- `p-c` = `[match-one, x]` (2 elements)
- `i-c` = `[match-one, v, path, g+s, sk, fk, i]` (7 elements)

`loop` walks them **1-to-1**:
1. `match-one` <-> `match-one-node`
2. `x` <-> `v-node`
3. `rest-patterns` is empty -> return `'()`

The remaining `[path, g+s, sk, fk, i]` are **silently discarded**.

So `x` binds to **only the first element** `v-node`, not the entire rest list `(v-node path-node g+s-node sk-node fk-node i-node)`.

When `expand->index-node-compound-list` expands template `(match-two . x)`:
- `match-two` -> `match-two`
- `x` -> `v-node` (only the first element!)
- Result: `(match-two . v-node)` -- only 2 elements.

### The dead-code `pair-form` branch below

There is a second `[pair-form ...]` branch below (line 375) with `fold-left` logic. It is **dead code** because `case` already matched `pair-form` in the first branch `[(list-form vector-form pair-form) ...]`.

Even if it were reachable, the `fold-left` produces:
```
(() (x . v-node) (x . path-node) (x . g+s-node) ...)
```
`generate-binding` uses `find` which returns only the **first** `(x . v-node)`, so the bug would persist.

---

## Fix Attempt (2025-05-19)

Implemented Option A with additional improvements in `analysis/identifier/expanders/pattern.sls`.

### Change 1: `pattern+index-node->pair-list` pair-form rest binding

In the `[(list-form vector-form pair-form) ...]` branch, added a clause before `else`:

```scheme
[(and (eq? 'pair-form (pattern-type pattern)) (null? (cdr rest-patterns)))
 `((,(car rest-patterns) . ,rest-index-nodes))]
```

**Effect**: For a `pair-form` pattern like `(match-one . x)`, when `loop` reaches the last pattern child (`x`), it binds `x` to the **entire remaining** `rest-index-nodes` list `[v-node, path-node, g+s-node, sk-node, fk-node, i-node]` instead of just the first element.

**Verification** (`test-verify-pair-form.sps`):
```
x binding in pairs: FOUND -> LIST of 5 elements: (path g+s sk fk i)
```

### Change 2: `expand->index-node-compound-list` pair-form proper list

Modified the `pair-form` lambda to detect when the last element of `a` is a list, and if so produce a proper list via `append`:

```scheme
[pair-form (lambda (a)
  (let ([last (car (reverse a))])
    (if (and (list? last) (not (null? last)))
      (append (reverse (cdr (reverse a))) last)
      (let loop ([rest a])
        (if (null? (cdr rest))
          (car rest)
          (cons (car rest) (loop (cdr rest))))))))]
```

**Effect**: Template `(match-two . x)` now produces a proper list `(match-two v path g+s sk fk i)` (7 elements) instead of an improper pair `(match-two . v)` (2 elements).

**Verification**:
```
expanded result: LIST[6]: (match-two path g+s sk fk i) -> PROPER LIST (FIXED!)
```

### Change 3: `generate-binding` literal-identifier shortcut

When `pattern+context->pairs->iterator` returns a symbol (for literal identifiers like `=>` or `failure` that are not in the pattern context), `generate-binding` now directly returns `(literal . symbol)` instead of going through the complex ellipsed loop:

```scheme
(let* ([first-var (iterator)]
       [is-literal? (symbol? first-var)])
  (if is-literal?
    `(,literal . ,first-var)
    ...))
```

**Effect**: Literal identifiers in templates are preserved as symbols in `callee-compound`, rather than being replaced by `'()`.

### Change 4: `generate-binding` list-value handling

Added a `(list? (vector-ref v i))` branch in the main loop:

```scheme
[(list? (vector-ref v i))
  (loop (+ 1 i) ancestors (append result (vector-ref v i)))]
```

**Effect**: When a rest variable binds to a list of index-nodes, `generate-binding` correctly splices the list elements into the result.

---

## Verification Results

### ✅ Isolated pair-form test passes
`test-verify-pair-form.sps` confirms:
- `x` now binds to the full rest list `[path, g+s, sk, fk, i]`
- `expand->index-node-compound-list` produces a proper list `(match-two path g+s sk fk i)`
- `pairs` alignment would have 6 mappings instead of 2

### ❌ Full auto-resolve test still fails
`test-match-auto-resolve-no-ellipsis.sps` still reports:
```
FAIL auto-resolve match attaches 'path reference
```

`path-node` exports remain empty after auto-resolve.

---

## Why the fix is insufficient

The `pattern.sls` changes fix the **pair-form truncation bug** in `match-one`'s catch-all clause, but `match-next` has a deeper structural problem that cannot be solved by `pattern.sls` alone.

### `match-next`'s template is the real blocker

`match-next` has a clause:
```scheme
((match-next v g+s (pat . body) . rest)
 (match-next v g+s (pat (=> failure) . body) . rest))
```

**Pattern children**: `[match-next, v, g+s, (pat . body), rest]` (5 elements)  
**Callee children**: `[x, ((? string? path) path)]` (2 elements)

Wait, this is not accurate. Let me re-analyze.

The callee of `match-next` is:
```scheme
(match-next v (x (set! x)) ((? string? path) path))
```

This has **3 operands**: `v`, `(x (set! x))`, `((? string? path) path)`.

But `make-pattern` on the pattern `(match-next v g+s (pat . body) . rest)` produces **5 children**:
`[match-next, v, g+s, (pat . body), rest]`.

`pattern+index-node->pair-list` walks them 1-to-1:
1. `match-next` <-> `v`
2. `v` <-> `(x (set! x))`
3. `g+s` <-> `((? string? path) path)`
4. `(pat . body)` <-> virtual empty node
5. `rest` <-> virtual empty node

Because the callee only has 3 top-level children, `pat` and `body` (inside `(pat . body)`) bind to **virtual nodes**, not to the actual pattern/body sub-expressions inside `((? string? path) path)`.

This causes `expand->index-node-compound-list` to produce a malformed `callee-compound`:
```
callee-compound type: pair
pairs count: 1
```

The resulting improper pair has only 1 mapping in `pairs`, so `shallow-copy` at the `match-next` layer FALLBACKs `path` exports to the entire macro-call node `(match x ((? string? path) path))`.

Subsequent outer layers (`match`) then propagate the reference to the macro-call node, never reaching the specific `path-node` inside `(? string? path)`.

### Summary of the remaining problem

| Layer | Issue | Can `pattern.sls` fix it? |
|-------|-------|---------------------------|
| `match-one` catch-all | `pair-form` rest var bound to 1st element only | ✅ Fixed |
| `match-next` clause | Pattern has 5 children, callee has 3; `pat`/`body` bind to virtual nodes | ❌ No -- requires structural redesign of `pairs` generation or hand-written rules |

---

## Deep Dive: `match-next` Structural Mismatch

This section explains exactly why `match-next` breaks the auto-expansion machinery.

### 1. The template and its pattern children

`match-next`'s matching clause:
```scheme
((match-next v g+s (pat . body) . rest)
 (match-next v g+s (pat (=> failure) . body) . rest))
```

Template: `(match-next v g+s (pat (=> failure) . body) . rest)`

`make-pattern` analyzes this template:
- It is a **pair-form** (ends with a pattern variable `rest`)
- `pattern-children` = `[match-next, v, g+s, (pat (=> failure) . body), rest]`
- That's **5 children**

Wait, let me re-trace `make-pattern` for `(match-next v g+s (pat . body) . rest)`.

`make-pattern` loop:
```
(loop '(match-next v g+s (pat . body) . rest))
  -> (cons (make-pattern 'match-next)
           (loop '(v g+s (pat . body) . rest)))
     -> (cons (make-pattern 'v)
              (loop '(g+s (pat . body) . rest)))
        -> (cons (make-pattern 'g+s)
                 (loop '((pat . body) . rest)))
           -> (cons (make-pattern '(pat . body))
                    (loop 'rest))
              -> (make-pattern 'rest) [since (pair? 'rest) = #f]
              -> returns (rest-pattern)
           -> returns ((pat . body)-pattern . (rest-pattern)) = ((pat . body)-pattern rest-pattern)
        -> returns (g+s-pattern (pat . body)-pattern rest-pattern)
     -> returns (v-pattern g+s-pattern (pat . body)-pattern rest-pattern)
  -> returns (match-next-pattern v-pattern g+s-pattern (pat . body)-pattern rest-pattern)
```

So `pattern-children` is a proper list of 5 elements:
`[match-next-pattern, v-pattern, g+s-pattern, (pat . body)-pattern, rest-pattern]`.

### 2. The callee and its children

When the user writes:
```scheme
(match x [(? string? path) path])
```

`match` expands to `(let ((v x)) (match-next v (x (set! x)) ((? string? path) path)))`.

The **callee** of `match-next` is:
```scheme
(match-next v (x (set! x)) ((? string? path) path))
```

Its **index-node children** (operands) are:
`[v, (x (set! x)), ((? string? path) path)]` -- **3 elements**.

### 3. `pattern+index-node->pair-list` 1-to-1 loop

The function receives:
- `pattern-children` = `[match-next, v, g+s, (pat . body), rest]` (5 elements)
- `index-node-children` = `[v, (x (set! x)), ((? string? path) path)]` (3 elements)

`loop` walks them 1-to-1:
1. `match-next` <-> `v`
2. `v` <-> `(x (set! x))`
3. `g+s` <-> `((? string? path) path)`
4. `(pat . body)` <-> **virtual node** (rest-index-nodes exhausted)
5. `rest` <-> **virtual node**

So **after the first 3 children**, `pat`/`body`/`rest` are bound to virtual nodes.

This is the structural mismatch: **pattern has 5 positional children but the callee only has 3 operands**.

### 4. The virtual node binding

When `rest-index-nodes` is empty but `rest-patterns` is not, `pattern+index-node->pair-list` executes:
```scheme
[(null? rest-index-nodes)
 (apply append
   (map (lambda (p) (pattern+index-node->pair-list p (make-index-node '() '() '() '() '() '() '() '()))) rest-patterns))]
```

It creates a **virtual empty index-node** for each remaining pattern child.

For `match-next`:
- `(pat . body)` gets a virtual node
- `rest` gets a virtual node

Then recursively:
- `pat` gets a virtual node (child of the virtual node for `(pat . body)`)
- `body` gets a virtual node (child of the virtual node for `(pat . body)`)

So `pat` and `body` are both bound to **virtual empty nodes**.

### 5. Impact on `callee-compound`

`expand->index-node-compound-list` expands the template `(match-next v g+s (pat (=> failure) . body) . rest)` using these bindings:

| Template element | Value from bindings |
|-----------------|---------------------|
| `match-next` | `v` (index-node) |
| `v` | `(x (set! x))` (index-node) |
| `g+s` | `((? string? path) path)` (index-node) |
| `(pat (=> failure) . body)` | `(virtual-node (=> failure) . virtual-node)` |
| `rest` | `(virtual-node)` or `virtual-node` |

The `pair-form` lambda in `expand->index-node-compound-list` processes this list. Because the 4th element is an improper pair, the resulting `callee-compound` is malformed.

In practice, the result is an **improper pair** (`pair car=match-next cdr-type=pair`), not a proper list.

### 6. Impact on `pairs`

`private:expansion+index-node->pairs` receives this malformed `callee-compound`.

For an improper pair, it executes:
```scheme
[(pair? compound-list)
 (private:expansion+index-node->pairs `(,(car compound-list) ,(cdr compound-list)) index-node)]
```

It converts the pair into a 2-element list `[car, cdr]`.

But `cdr` is itself a pair, so this recursive conversion produces a cascade of 2-element lists that don't align with the expansion tree's children.

The expansion root has 3 children. The converted `callee-compound` has 2 top-level elements. `private:expansion+index-node->pairs` takes the minimum (2) to align. Each alignment produces 1 pair (or a recursive call that may fail). In the end, only **1 pair** survives.

### 7. The final fallout

With only 1 pair, `shallow-copy` at the `match-next` layer cannot find mappings for most expansion nodes.

When it encounters the expansion node carrying the `path` export, it falls back to `initialization-index-node` -- the entire `match-next` macro call `(match x ((? string? path) path))`.

The `path` reference is attached to the macro-call node, not to `path-node`.

Outer layers (`match`) then see this reference on the macro-call node and propagate it further, but never to the specific `path-node` inside `(? string? path)`.

### 8. Why this is fundamentally hard to fix

The core issue is that `pattern+index-node->pair-list` uses **syntactic 1-to-1 positional matching** on `index-node-children`, while `match-next`'s pattern uses **semantic destructuring** (`(pat . body)` matches against the third callee child `((? string? path) path)` by splitting it into pattern and body).

`pattern+index-node->pair-list` would need to:
1. Recognize that `(pat . body)` is a pair-form pattern
2. Match it against the third callee child `((? string? path) path)`
3. Bind `pat` to `(? string? path)` and `body` to `(path)`

But step 2 requires knowing that `g+s` is the second child and `(pat . body)` is the third -- which is already what happens.

Wait, actually the problem is different. `pattern+index-node->pair-list` DOES match `g+s` against the third child `((? string? path) path)`. It's just that the pattern `(pat . body)` is the **fourth** child, and there is no fourth callee child.

Ah, I see the confusion. Let me re-examine:

Pattern: `(match-next v g+s (pat . body) . rest)`
Pattern children from `make-pattern`: `[match-next, v, g+s, (pat . body), rest]`

Callee: `(match-next v (x (set! x)) ((? string? path) path))`
Callee children: `[v, (x (set! x)), ((? string? path) path)]`

The callee's **operator** (`match-next`) is not counted as a child in `index-node-children`. The children are the **operands**.

So:
1. `match-next` (pattern child 1) <-> `v` (callee child 1)
2. `v` (pattern child 2) <-> `(x (set! x))` (callee child 2)
3. `g+s` (pattern child 3) <-> `((? string? path) path)` (callee child 3)
4. `(pat . body)` (pattern child 4) <-> **no callee child** -> virtual node
5. `rest` (pattern child 5) <-> **no callee child** -> virtual node

So the issue is that the pattern has **5 children** but the callee only has **3 operands**.

In the syntax-rules definition, `(match-next v g+s (pat . body) . rest)` describes the **macro call syntax**, where `match-next` is the operator and `v`, `g+s`, `(pat . body)`, and `rest` are operands.

But in the actual macro call `(match-next v (x (set! x)) ((? string? path) path))`, there are only **3 operands**: `v`, `(x (set! x))`, `((? string? path) path)`.

`pattern+index-node->pair-list` sees the pattern as having 5 children (including the operator) and tries to match them against the callee's children. But the callee's children don't include the operator!

Actually, `make-pattern` on `(match-next v g+s (pat . body) . rest)` treats `match-next` as the first child. And the callee index-node for `(match-next v (x (set! x)) ((? string? path) path))` has `match-next` as its datum and `[v, (x (set! x)), ((? string? path) path)]` as its children.

So `pattern+index-node->pair-list` matches:
1. `match-next` (pattern) <-> `v` (callee's first operand)
2. `v` (pattern) <-> `(x (set! x))` (callee's second operand)
3. `g+s` (pattern) <-> `((? string? path) path)` (callee's third operand)

This is already shifted by 1! The operator `match-next` in the pattern is being matched against the first operand `v` in the callee.

Wait, that can't be right. Let me check how `pattern+index-node->pair-list` is called.

In `syntax-rules.sls`:
```scheme
[pairs (pattern+index-node->pair-list pattern local-index-node)]
```

`pattern` is `(make-pattern pattern-expression)` where `pattern-expression` is the pattern from the clause.

For `match-next`, the clause is:
```scheme
((match-next v g+s (pat . body) . rest)
 ...)
```

`pattern-expression` = `(match-next v g+s (pat . body) . rest)`.

`local-index-node` is the **macro call** index-node: `(match-next v (x (set! x)) ((? string? path) path))`.

`pattern+index-node->pair-list` receives `pattern` and `local-index-node`.

It first creates the top-level pair: `(pattern . local-index-node)`.

Then it recursively processes children:
- `p-c` = `pattern-children` = `[match-next, v, g+s, (pat . body), rest]`
- `i-c` = `index-node-children` = `[v, (x (set! x)), ((? string? path) path)]`

Wait, `index-node-children` for the macro call `(match-next v (x (set! x)) ((? string? path) path))` -- what are its children?

In Scheme AST, a list `(a b c)` is represented as an index-node with datum `a` and children `[b, c]`.

So for `(match-next v (x (set! x)) ((? string? path) path))`:
- datum = `match-next`
- children = `[v, (x (set! x)), ((? string? path) path)]`

And `make-pattern` on `(match-next v g+s (pat . body) . rest)`:
- `pattern-children` = `[match-next, v, g+s, (pat . body), rest]`

So `pattern+index-node->pair-list` matches:
1. `match-next` (pattern) <-> `v` (callee's first operand)
2. `v` (pattern) <-> `(x (set! x))` (callee's second operand)
3. `g+s` (pattern) <-> `((? string? path) path)` (callee's third operand)
4. `(pat . body)` (pattern) <-> virtual node
5. `rest` (pattern) <-> virtual node

This is the structural mismatch.

### 9. Why this matters for `path-node`

The `path` identifier-reference originates from the `let`-binding `(let ((path v)) ...)` deep inside `match-check-identifier`.

It propagates upward through `match-two`, `match-one`, and reaches `match-next`.

At `match-next`, `shallow-copy` needs to map the `path` export from the expansion tree to the callee tree. But because `pairs` only has 1 valid mapping, `path` FALLBACKs to the macro-call node.

The `path` reference is now on `(match-next v (x (set! x)) ((? string? path) path))` instead of on `path-node` inside `(? string? path)`.

When `match` (the outermost layer) runs `shallow-copy`, it sees the `path` reference on the `match-next` macro-call node. But `match`'s `pairs` maps the macro-call node to the user's original `(match x [(? string? path) path])` call. So `path` ends up on the original `match` call, not on `path-node`.

### 10. Summary

The structural mismatch in `match-next`:

| | Pattern | Callee |
|--|---------|--------|
| Structure | `(match-next v g+s (pat . body) . rest)` | `(match-next v (x (set! x)) ((? string? path) path))` |
| Pattern children | 5 | 3 |
| `pat` binding | virtual node | should be `(? string? path)` |
| `body` binding | virtual node | should be `(path)` |
| `rest` binding | virtual node | should be `()` |

Because `pattern+index-node->pair-list` cannot decompose the 3rd callee child `((? string? path) path)` into `pat` and `body`, these pattern variables bind to garbage virtual nodes. This poisons `callee-compound`, which becomes an improper pair with only 1 valid `pairs` mapping. `shallow-copy` then FALLBACKs `path` exports to the macro-call node, and the reference never reaches `path-node`.

---

## Updated Fix Direction

### Option A-1: `pattern.sls` pair-form fix (DONE)

Implemented. Fixes `match-one` and any macro with `pair-form` rest variables. Verified in isolation.

### Option A-2: Improve `match-next` pairs via structural splicing

Teach `private:expansion+index-node->pairs` to "look inside" callee sub-nodes when the top-level `callee-compound` is malformed. For example, when `g+s` maps to `((? string? path) path)`, recursively extract `pat` and `body` from that sub-node.

**Risk**: Very invasive, requires knowing the semantics of `ufo-match` macros inside generic `syntax-rules` machinery.

### Option B: Redesign `callee-compound` generation

Make `expand->index-node-compound-list` aware of `pat`/`body`/`rest` semantics, or add a post-processing step that reconstructs a proper `callee-compound` from the virtual-node tree.

**Risk**: Hard to generalize; would be `ufo-match`-specific.

### Option C: Hand-written rules (Recommended for `ufo-match`)

Restore `match-process` in `router.sls` for `ufo-match` macros. Hand-written rules bypass the entire `expand->index-node-compound-list` / `pairs` machinery and directly attach references to the correct sub-nodes.

**Trade-off**: Loses the generality of auto-expansion, but `ufo-match` is complex enough that auto-expansion may never handle it correctly without a major redesign.

---

## Conclusion

1. **`match-one`'s `pair-form` truncation bug is fixed** by binding rest variables to the full remaining index-node list.
2. **`match-next`'s structural mismatch remains unresolved**. Its pattern has 5 positional children but the callee only has 3. `pat`/`body`/`rest` bind to virtual nodes, producing a malformed `callee-compound` with only 1 `pairs` mapping. `path` exports FALLBACK at this layer and never reach the original `path-node`.
3. **The auto-expansion `pairs` mechanism is fundamentally limited** for macros where pattern variables do not align 1-to-1 with callee children (e.g. `ufo-match`'s `match-next`).
4. **Practical path forward**: Option C -- use hand-written `match-process` rules for `ufo-match` macros, or accept that auto-expansion will not provide accurate go-to-definition for pattern variables in `match` expressions.

---

## Files Modified for This Investigation

- `analysis/identifier/expanders/pattern.sls`
  - Added `pair-form` rest-variable full-list binding (line ~381)
  - Added `list?` branch in `generate-binding` main loop
  - Added literal-identifier shortcut in `generate-binding`
  - Modified `expand->index-node-compound-list` `pair-form` lambda to produce proper lists

- `analysis/identifier/expanders/expansion-wrap.sls`
  - Memory cap 10 -> 15 (already present before this fix attempt)
  - Added PLAN.md reverse-map logic (`build-reverse-map`, `sync-to-parent-expansion`, `all-pairs` fallback)
  - Debug prints added and removed during investigation

- `analysis/identifier/expanders/syntax-rules.sls`
  - Debug prints added and removed during investigation

- `analysis/identifier/self-defined-rules/router.sls`
  - Pure auto-macro-resolve path (no `match-process` mixin)

- `tests/analysis/identifier/test-match-auto-resolve-no-ellipsis.sps`
  - Added inspection code for `syntax-expander` result

- `test-verify-pair-form.sps`
  - New: isolated verification test for `pair-form` binding behavior
