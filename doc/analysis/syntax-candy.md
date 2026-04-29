# Syntax Candy — List Pattern Matcher

> Module: `analysis/type/domain-specific-language/syntax-candy.sls`

This module implements a small **list-pattern matcher** for the type-system DSL. It is the engine that decides whether a parameter template (e.g. `(a b ... c)`) can match a concrete argument list (e.g. `(1 2 3 4)`) and, if so, produces the variable bindings.

It is used heavily by:
- `interpreter.sls` — when applying `inner:lambda?` types to argument lists.
- Macro expansion — when a macro head contains repeated segments (`...` / `**1`).

---

## 1. Core Concepts

### 1.1 Segment

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

### 1.2 Pattern vs. Argument List

The matcher always works on **two** segment vectors:

- **Parameter template** (`rest-segments`) — the left-hand side, may contain `...` / `**1`.
- **Argument list** (`ready-segments`) — the right-hand side, is usually plain (`tail` = `'()`).

Both sides *may* contain repeats, which makes the algorithm closer to regular-expression intersection than to simple destructuring.

---

## 2. Public API

### `candy:segmentable? target`

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

### `candy:matchable? parameter-template argument-list`

Returns `#t` if the two lists match structurally (ignoring the actual values of the atoms), `#f` otherwise.

```scheme
(candy:matchable? '(a b ... c) '(1 2 3 4 5))
;; => #t   (a=1, b={2 3 4}, c=5)

(candy:matchable? '(a b **1 c) '(1))
;; => #f   (b needs at least one element)
```

Internally this builds a **match matrix** (see §4) and checks whether the bottom-right cell contains `'skipped` (the success sentinel).

---

### `candy:match parameter-template argument-list`  
### `candy:match matrix rest-segments ready-segments row-id column-id`

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

### `candy:match-left parameter-template argument-list`
### `candy:match-left match-segment-pairs`

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

### `candy:match-right match-segment-pairs`

The dual of `candy:match-left`. Maps each matched pair to a simple `(rest-type . ready-type)` pair:

```scheme
'((a . 1) (b . 2) (b . 3) (c . 4) ...)
```

Unlike `candy:match-left`, it does **not** group repeated segments; the same `rest-type` may appear multiple times. It is used less frequently, mainly for symmetry and debugging.

---

## 3. Record Type — `segment`

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

## 4. Algorithm — Match Matrix

The matcher is essentially a **dynamic-programming** implementation of regular-expression-like list matching.

### 4.1 Matrix Shape

Given `m` rest-segments and `n` ready-segments, the matrix has `(m+1) × (n+1)` cells. Row 0 and column 0 are padding borders.

### 4.2 Cell States

| State | Meaning |
|-------|---------|
| `matched` | The cell at `(row, col)` represents a valid pairing between `rest-segments[row-1]` and `ready-segments[col-1]`. |
| `skipped` | The cell is reachable but does **not** represent a pairing; it is a transition state (used for borders and repeat propagation). |
| `unused` | The cell is unreachable for the current path. |

### 4.3 Allowed Moves

- **Right →** — consume one argument segment without consuming a parameter segment (only valid when the parameter has a repeat tail).
- **Down ↓** — consume one parameter segment without consuming an argument segment (only valid when the argument has a repeat tail).
- **No diagonal ↘** — a cell is never reached by consuming both sides at once. The diagonal relationship is encoded by adjacent `matched` cells.

### 4.4 Building the Matrix (`private-segments->match-matrix`)

The builder is a recursive backtracker:

1. Start at `(0, 0)` and mark it `skipped`.
2. At each cell try to move **right** and/or **down**.
3. If a move leads to a successful path (i.e. eventually reaches `(m, n)` with valid end-zone conditions), mark the current cell with the requested status.
4. If neither move succeeds, leave the cell `unused`.

### 4.5 End-Zone Rules (cell `(m, n)`)

The bottom-right cell is the acceptance state. It is marked `matched` when **any** of the following holds:

- It is immediately preceded by a `skipped` cell (row or column).
- The last parameter segment or last argument segment has tail `...` / `**1`.
- Either side ends with `...`.

If the matrix builder leaves `(m, n)` as `unused`, the two lists do not match.

### 4.6 Backtracking (`candy:match` five-arity)

After the matrix is built, `candy:match` walks from `(0, 0)` to `(m, n)` recursively:

- `matched` cell → emit the corresponding segment pair, then recurse **right** and **down**.
- `skipped` cell → recurse **right** and **down** without emitting.
- `unused` cell → stop.

Because repeats allow both right and down moves from the same cell, the recursion naturally fans out to cover all possible consumptions.

---

## 5. Examples

### 5.1 Simple destructuring

```scheme
(candy:match-left
  (candy:match '(a b c) '(1 2 3)))
;; => ((a . 1) (b . 2) (c . 3))
```

### 5.2 Kleene-star (`...`)

```scheme
(candy:match-left
  (candy:match '(a b ... c) '(1 2 3 4 5)))
;; => ((a . 1) (b . (2 3 4)) (c . 5))
```

### 5.3 Kleene-plus (`**1`)

```scheme
(candy:match-left
  (candy:match '(a b **1 c) '(1 2 3 4 5)))
;; => ((a . 1) (b . (2 3 4)) (c . 5))

(candy:matchable? '(a b **1 c) '(1 2))
;; => #f   (b needs ≥1 element)
```

### 5.4 Both sides have repeats

`syntax-candy` does **not** support multiple repeat segments in a single template. Patterns such as `(a ... b ...)` are rejected by `private-segment`. The matcher is designed for at most one variable-length segment per list.

---

## 6. Relationship to the Type System

`syntax-candy` is **not** a general-purpose pattern matcher. It is specialised for the shapes that appear in type expressions:

- Lambda parameter lists: `(inner:list? number? boolean? ...)`.
- Macro templates: `(with ((a b c **1)) …)`.

The interpreter calls `candy:matchable?` before attempting substitution. If the shapes do not align, the lambda/macro application returns `'()` (no result). If they do align, `candy:match-left` produces the binding environment that `private-with` uses to rewrite the body.

Because the matcher operates on **segment vectors** rather than raw lists, the cost is `O(m × n)` in the number of segments, independent of how many concrete values a `...` segment ends up consuming.

---

## 7. Common Traps

1. **`...` / `**1` must follow a non-repeat segment, and a template may contain at most one repeat marker in total.**  
   `(a **1 **1)` is rejected because `a` would already carry `**1` and cannot receive another. `(a **1 b **1)` and `(a ... b ...)` are also rejected because the matcher only supports a single variable-length segment per list. This is caught by `private-segment` raising `"wrong rule"`.

2. **Empty repeats.**  
   `(a b **1)` cannot match `(a)` — `b` needs at least one item. Use `...` when zero items are acceptable.

3. **The matrix is rebuilt on every call.**  
   There is no memoisation. In tight loops (e.g. deep macro expansion inside the interpreter) the same parameter template may be re-segmented many times. This has not been a bottleneck in practice because type-level lists are short (< 20 elements).

---

## 8. Architecture Context — Why `syntax-candy` Exists

`syntax-candy` is not an optional utility; it is a **load-bearing wall** between Phase I (substitution generation) and Phase II (interpretation). To understand its bugs and optimisation potential, it helps to see exactly where the data flows.

### 8.1 The Data Flow

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

### 8.2 Why Not Ordinary `match` or `destructure`?

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

### 8.3 The Critical Invariant

Every successful macro or lambda application in the interpreter depends on the following invariant:

> **`candy:match` (and therefore `candy:match-left`) returns *exactly one* binding entry per distinct parameter segment, and the right-hand side of a repeated segment contains *each matched argument exactly once*, in order.**

`private:group-match-pairs` enforces this invariant explicitly: even if the raw matrix walk were to emit duplicate pairs in future, the fold-left grouper collapses them by `segment-type` before any substitution reaches `private-with`. The interpreter silently continues with a bloated or corrupted substitution list, and the Cartesian-product pruner downstream has to deal with the mess.

---

## 9. Known Bugs & Their Systemic Impact

### 9.1 Note — Why `walk` Does *Not* Produce Duplicates in Practice

#### Where

`candy:match` five-arity, lines 84–97.

#### Observation

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

#### Why This Is Still Fragile

The absence of duplicates is an **emergent property** of the current `right-first` / `down-first` heuristic, not a guaranteed invariant of the algorithm. If the exploration order in `private-next-step-ok?` ever changes (or if a future optimisation adds a second valid edge to a cell), `walk` will silently start emitting duplicates. A safer design would be:

- **Option A**: Make `walk` a depth-first search that returns as soon as it reaches `(m, n)`, guaranteeing exactly one path.
- **Option B**: Keep the all-paths collector, but deduplicate the result list by `equal?` on `(rest-segment, ready-segment)` pairs before returning.

Either choice makes the no-duplication guarantee explicit rather than accidental.

---

### 9.2 Bug B — `candy:match-left` Compared Whole `segment` Records with `equal?` ✅ Fixed

#### Where

`private:group-match-pairs` (called by `candy:match-left`), previously line 64:

```scheme
(if (equal? (car last-pair) (car match-segment-pair))
```

#### Root Cause

Both operands were `segment` record objects. Chez Scheme's `equal?` on records compares every field recursively. While `segment` had only two fields (`type` and `tail`), this happened to work. But it created a **fragile implicit dependency** on the record layout:

- If a third field is added, `equal?` could suddenly return `#f` for segments that represent the same pattern variable.
- Because `tail` is mutable, two segments that were `equal?` at one moment could cease to be `equal?` later, causing non-deterministic grouping failures.

#### Systemic Impact

When `equal?` unexpectedly returned `#f`, the grouper treated two occurrences of the same repeat segment as **different** variables. Instead of producing:

```scheme
'(b . (number? boolean? string?))
```

it produced:

```scheme
'(b . (number?)) (b . (boolean?)) (b . (string?))
```

`private-with` then saw three separate bindings for `b`, and only the last one won. The macro body ended up with a truncated or incorrect substitution.

#### Fix Applied

`private:group-match-pairs` now compares the **semantic identity** of the segment via `segment-type`:

```scheme
(if (equal? (segment-type (car last-pair))
            (segment-type (car match-segment-pair)))
```

`segment-type` is immutable and represents the actual pattern variable symbol. It is the correct key for grouping, and the fix removes the implicit dependency on the full record layout.

---

### 9.3 Bug C — `private-segment` Raises a String, Not a Condition

#### Where

`private-segment`, lines 300 and 306:

```scheme
(raise "wrong rule")
```

#### Root Cause

Chez Scheme allows `raise` with any value. `candy:segmentable?` already wraps `private-segment` in `ufo-try` and safely swallows the exception:

```scheme
(try
  (private-segment target)
  #t
  (except c 
    (else #f)))
```

However, `candy:matchable?` and `candy:match` call `private-segment` **directly**, without any `try`/`except`. If a caller (e.g. a user-defined rule in `self-defined-rules`) passes a malformed template such as `'(... a)` directly to `candy:matchable?`, the string exception propagates until it hits whatever handler happens to be on the stack. That handler may or may not catch a string; if it does not, the entire `type:interpret` call chain aborts.

#### Systemic Impact

Because type inference runs on **every** document in the workspace, one malformed library can crash hover information for the entire project. The crash is not guaranteed — it depends on whether the calling code uses `ufo-try` — but it is possible.

#### Fix Direction

Raise a symbol or a proper condition:

```scheme
(raise 'invalid-macro-template)
;; or
(raise (make-condition 'invalid-macro-template))
```

Alternatively, add a `try`/`except` guard inside `candy:matchable?` and `candy:match` so that illegal templates are treated as non-matching (`#f` or `'()`) rather than fatal errors.

---

### 9.4 Bug D — `private-next-step-ok?` Performs Incomplete Backtracking

#### Where

`private-next-step-ok?`, lines 259–282.

#### Root Cause

The function writes into the matrix, recurses, and then — if the recursive exploration fails — resets **only** the current cell to `unused`:

```scheme
(matrix-set! matrix cols row-id column-id status)
(private-segments->match-matrix matrix rest-segments ready-segments row-id (+ column-id 1))
(if (equal? 'unused (matrix-take matrix cols row-id (+ column-id 1)))
  (matrix-set! matrix cols row-id column-id 'unused))
```

The recursive call may have written into cells far beyond the immediate neighbour. Those writes are **never undone**.

#### Systemic Impact

Because `candy:matchable?` only checks cell `(0,0)`, this pollution is usually benign. However, in matrices where a failed rightward exploration writes `skipped`/`matched` marks deep into the grid, a subsequent downward exploration from the same parent may encounter those stale marks and treat them as valid path continuations. This can cause `candy:matchable?` to return `#t` for shapes that should not match, or `candy:match` to collect spurious segment pairs.

In the type system this manifests as **occasional false-positive macro matches**: a macro template matches an argument list that it should reject, and `private-with` substitutes nonsense values into the body. The resulting type is wrong but syntactically valid, so it propagates silently.

#### Fix Direction

Two options:

1. **Functional matrix**: Instead of mutating a single vector, `private-next-step-ok?` returns a new vector on success and `#f` on failure. No backtracking needed because each path has its own copy.
2. **Snapshot & restore**: Before recursing, save the current matrix row (or the whole matrix, if small) and restore it on failure.

Given that type-level segment counts are tiny (< 20), option 1 (functional) is clean and eliminates an entire class of state-bugs at negligible cost.

---

## 10. Optimisation Opportunities

### 10.1 Cache `private-segment` Results

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

### 10.2 Avoid Double Parsing in `candy:matchable?` → `candy:match` Chains

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

### 10.3 Replace `walk`’s `append` with a Tail-Recursive Accumulator

**Observation**: `walk` uses `(append left right)` at every branch. For a template with `k` repeated segments, the total number of emitted pairs is `O(n^k)` in the worst case, and each `append` is `O(length)`.

**Opportunity**: Rewrite `walk` as a tail-recursive function that takes an accumulator list and `cons`es results onto it. This does not affect correctness (the current builder guarantees a single path), but it removes the quadratic `append` penalty.

### 10.4 Use a Fixed-size Functional Matrix Instead of a Mutable Vector

**Observation**: The current matrix is a flat mutable vector allocated with:

```scheme
(make-vector (* (+ 1 m) (+ 1 n)) 'unused)
```

For typical `m, n < 10`, this vector has < 121 cells. Allocation and mutation overhead dominate.

**Opportunity**: Represent the matrix as a nested list or a small fixed-size record (e.g. 5×5, 10×10, 20×20 tiers) and build it functionally. Chez Scheme’s generational GC is very fast for short-lived small objects; avoiding `matrix-set!` / `matrix-take` indirection may actually be faster.

---

## 11. Synthesis — How Bugs in `syntax-candy` Ripple Outward

`syntax-candy` sits at a narrow but critical chokepoint. Every lambda application and every macro expansion in the type interpreter passes through it. Because the rest of the pipeline (Cartesian product, recursive interpretation, deduplication) is designed to be **robust to noise**, the symptoms of `syntax-candy` bugs are rarely crashes. Instead they are:

| Symptom in the type system | Likely `syntax-candy` root cause |
|---|---|
| A parameter’s type is unexpectedly `something?` after macro expansion | Bug B or D — binding was lost or mis-grouped |
| `init-workspace` aborts with unhandled exception | Bug C — malformed template raises string |

This makes `syntax-candy` a high-leverage target for hardening: a small, well-tested module with ~300 lines of code that, if fixed, improves both correctness and performance across the entire type system.
