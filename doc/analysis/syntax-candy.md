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

Notice that `...` (and `**1`) does **not** create a new segment by itself; it mutates the `tail` of the *preceding* segment. This is why `tail` is mutable and why the parser rejects patterns such as `(...)` or `(a **1 b **1)`.

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
(candy:segmentable? '(a **1 b **1)) ;; => #f
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

Two-arity entry point: parses both lists, builds the matrix, then backtracks to collect the matched segment pairs.

Five-arity internal point: walks the matrix recursively from `(row-id, column-id)` towards the bottom-right, collecting every `(rest-segment . ready-segment)` pair that was `matched` or `skipped`.

Return value is an **alist**:

```scheme
'((#<segment a> . #<segment 1>)
  (#<segment b> . #<segment 2>)
  ...)
```

When a repeated segment consumes multiple argument elements, the same `rest-segment` appears multiple times with different `ready-segment` values. Post-processing (`candy:match-left` / `candy:match-right`) groups them.

---

### `candy:match-left match-segment-pairs`

Groups repeated matches so that the **left** side (the parameter template) is unique.

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

The dual of `candy:match-left`. Groups by the **right** side (the argument list). Returns pairs of the form:

```scheme
'((#<segment number?> . number?) ...)
```

Used less frequently; mainly for symmetry and debugging.

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

```scheme
(candy:matchable? '(a ... b ...) '(1 2 3))
;; => #t   (a consumes some prefix, b consumes the rest)
```

---

## 6. Relationship to the Type System

`syntax-candy` is **not** a general-purpose pattern matcher. It is specialised for the shapes that appear in type expressions:

- Lambda parameter lists: `(inner:list? number? boolean? ...)`.
- Macro templates: `(with ((a b c **1)) …)`.

The interpreter calls `candy:matchable?` before attempting substitution. If the shapes do not align, the lambda/macro application returns `'()` (no result). If they do align, `candy:match-left` produces the binding environment that `private-with` uses to rewrite the body.

Because the matcher operates on **segment vectors** rather than raw lists, the cost is `O(m × n)` in the number of segments, independent of how many concrete values a `...` segment ends up consuming.

---

## 7. Common Traps

1. **`...` / `**1` must follow a non-repeat segment.**  
   `(a **1 b **1)` is rejected because `b` would already carry `**1` from the first occurrence. This is caught by `private-segment` raising `"wrong rule"`.

2. **Empty repeats.**  
   `(a b **1)` cannot match `(a)` — `b` needs at least one item. Use `...` when zero items are acceptable.

3. **The matrix is rebuilt on every call.**  
   There is no memoisation. In tight loops (e.g. deep macro expansion inside the interpreter) the same parameter template may be re-segmented many times. This has not been a bottleneck in practice because type-level lists are short (< 20 elements).
