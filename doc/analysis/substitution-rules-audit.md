# Substitution Rules Audit & Improvement Plan

## Overview

`analysis/type/substitutions/rules/` contains the per-special-form processors that generate type equations (substitutions) during Phase I of type inference. Each `-process` function receives a `document` and an `index-node`, then calls `extend-index-node-substitution-list` to register type constraints.

This document records findings from a systematic audit and proposes a work plan.

---

## 1. Test Coverage Audit

There are **16 rule files** (including `util.sls`). Only **6 have dedicated tests**.

| Rule File | Has Test | Test File | Notes |
|-----------|----------|-----------|-------|
| `application.sls` | ❌ | — | 1-liner; no test |
| `begin.sls` | ❌ | — | 1-liner; no test |
| `case-lambda.sls` | ✅ | `test-case-lambda.sps` | Tests 2 real files |
| `case.sls` | ❌ | — | No test |
| `cond.sls` | ❌ | — | No test |
| `define.sls` | ✅ | `test-define.sps` | Tests 3 real files |
| `do.sls` | ❌ | — | No test |
| `if.sls` | ❌ | — | No test |
| `lambda.sls` | ✅ | `test-lambda.sps` | Also tests cross-clause |
| `let.sls` | ✅ | `test-let.sps` | Named & unnamed |
| `let*.sls` | ❌ | — | No test |
| `letrec.sls` | ❌ | — | No test |
| `record.sls` | ✅ | `test-record.sps` | `define-record-type` |
| `trivial.sls` | ✅ | `test-trivial.sps` | Literals, symbols |
| `util.sls` | — | — | Helpers only |

**Coverage rate: 37.5%.**

---

## 2. Code Quality Findings

### 2.1 Exact Duplication — `let*.sls` and `letrec.sls`

These two files are **byte-for-byte identical** except for library name and export:

```scheme
;; let*.sls
(library (scheme-langserver analysis type substitutions rules let*)
  (export let*-process) ...)

;; letrec.sls
(library (scheme-langserver analysis type substitutions rules letrec)
  (export letrec-process) ...)
```

Both bodies execute:
1. `(extend-index-node-substitution-list index-node return-index-node)`
2. `(extend-index-node-substitution-list return-index-node index-node)`
3. `(for-each let:private-process-key-value key-value-index-nodes)`

**Impact:** Maintenance burden. Any fix to `let*` must be manually mirrored to `letrec`.

**Fix options:**
- **Option A (preferred):** Extract a shared helper in `let.sls` and have both `let*.sls` and `letrec.sls` re-export it.
- **Option B:** Merge `let*.sls` and `letrec.sls` into a single file that exports both names. The generator already dispatches to them separately, so this is safe.

---

### 2.2 Useless Type Constraints — `cond.sls` and `if.sls`

Both hard-code the predicate/condition type as `'something?` (the universal top type):

```scheme
;; cond.sls
(extend-index-node-substitution-list first-child 'something?)

;; if.sls
(extend-index-node-substitution-list condition-index-node 'something?)
```

`'something?` conveys **zero useful information**. The interpreter already treats unknowns as `something?` by default.

**Fix:** Constrain conditions to `boolean?` instead:
```scheme
(extend-index-node-substitution-list condition-index-node private-boolean?)
```

This is a low-risk, semantically better default. It will not break correct code (any value can be used in a boolean context in Scheme, but the *intent* of a condition is boolean), and it may help catch obvious errors where a non-boolean is used as a condition.

---

### 2.3 Missing Return-Type Semantics — `do.sls`

The `do` rule processes variable bindings (`var init update`) but **completely ignores** the `(test result ...)` tail:

```scheme
[(_ ((var init update ...) **1) (test result ...) _ ... ) 
  (let* ([children (index-node-children index-node)]
      [var-index-node (cadr children)])
    (for-each private-process (index-node-children var-index-node)))]
```

A `do` form’s type should unify with the `result ...` expressions (the values returned when the test becomes true). Currently the parent `do` node has no substitution linking it to its results.

**Fix:** After processing bindings, add:
```scheme
(let ([test-result-children (index-node-children (caddr children))])
  (for-each 
    (lambda (result-node)
      (extend-index-node-substitution-list index-node result-node)
      (extend-index-node-substitution-list result-node index-node))
    (cdr test-result-children)))  ; skip the test predicate itself
```

---

### 2.4 Missing Key-Type Narrowing — `case.sls`

`case` forwards the key expression to each clause’s datum list:

```scheme
(for-each 
  (lambda (t) (extend-index-node-substitution-list expression-node t))
  previous-index-nodes)
```

But it never constrains the key against the datums. In principle, if the datums are all `'symbol` or `'number?`, the key’s type can be narrowed.

**Current assessment:** Full narrowing requires datum evaluation (e.g. `'(foo bar)` implies the key is a symbol). This is complex and may not be worth the effort for IDE diagnostics. At minimum, the existing forwarding behavior should be tested.

---

### 2.5 Suspiciously Trivial Rules

#### `application.sls` — does almost nothing
```scheme
(define (application-process document index-node)
  (extend-index-node-substitution-list index-node (index-node-children index-node)))
```

This registers the application node’s children as possible types for the node, but it does **not** create function-application constraints (argument types ↔ parameter types). This is a major missing feature, but adding it would require integrating with the identifier-reference system to resolve the callee’s type.

**Recommendation:** Add a comment documenting this limitation. Do not attempt full application inference without a larger design review.

#### `begin.sls` — only links to last child
```scheme
(define (begin-process document index-node)
  (extend-index-node-substitution-list
    index-node
    (car (reverse (index-node-children index-node)))))
```

This is semantically correct for R6RS `begin` (returns the last expression’s value), but it does not enforce that intermediate expressions are side-effect-only. This is acceptable for IDE use.

**Recommendation:** Add a test documenting this behavior.

---

### 2.6 `unless` Routed to `begin-process`

In `generator.sls:139`:
```scheme
[(equal? r '(unless)) (private-add-rule rules `((,begin-process) . ,identifier))]
```

`unless` is semantically `(if (not test) body ...)`. Routing it to `begin-process` means the condition expression is never processed by `if-process`, so no type constraints are generated for the test.

**Fix:** Route `unless` to `if-process` instead, or create a minimal `unless-process` that wraps `if-process` logic.

---

### 2.7 `trivial.sls` is a Kitchen Sink

At 177 lines, `trivial.sls` handles:
- Literal types (`fixnum?`, `string?`, `boolean?`, …)
- Pairs, vectors, quasiquote, unquote-splicing
- Parameter implicit conversion
- Cross-document identifier references
- Null/empty list

**Impact:** High cognitive load. Hard to unit-test in isolation.

**Fix:** Split into 2–3 focused modules:
- `trivial-literals.sls` — numbers, strings, booleans, chars, symbols
- `trivial-collections.sls` — pairs, vectors, null
- `trivial-quoting.sls` — quasiquote, unquote, unquote-splicing

This is a medium-effort refactoring with low immediate payoff but high long-term maintainability.

---

## 3. Recommended Work Plan

### Phase 1 — Quick Wins (1–2 hours)

| # | Task | Effort | Risk | File(s) |
|---|------|--------|------|---------|
| 1 | Deduplicate `let*.sls` / `letrec.sls` | 20 min | Low | `let*.sls`, `letrec.sls`, `let.sls` |
| 2 | Change `cond.sls` / `if.sls` condition constraint from `'something?` → `boolean?` | 10 min | Low | `cond.sls`, `if.sls` |
| 3 | Route `unless` to `if-process` in `generator.sls` | 5 min | Low | `generator.sls` |
| 4 | Fix `do.sls` return-type semantics | 15 min | Low | `do.sls` |

### Phase 2 — Add Missing Tests (2–3 hours)

| # | Task | Effort | File(s) |
|---|------|--------|---------|
| 5 | Add `test-if.sps` | 20 min | `if.sls` |
| 6 | Add `test-cond.sps` | 20 min | `cond.sls` |
| 7 | Add `test-begin.sps` | 15 min | `begin.sls` |
| 8 | Add `test-do.sps` | 20 min | `do.sls` |
| 9 | Add `test-case.sps` | 20 min | `case.sls` |
| 10 | Add `test-letstar-letrec.sps` | 20 min | `let*.sls`, `letrec.sls` |
| 11 | Add `test-application.sps` | 15 min | `application.sls` |

> **Testing style note:** Existing tests load real workspace files and do end-to-end inference. For these new tests, prefer **minimal fixture-based unit tests** using `tests/resources/workspace-fixtures/` or tiny inline `.scm.txt` files. Each test should only exercise one rule in isolation.

### Phase 3 — Refactoring (Optional, 2–3 hours)

| # | Task | Effort | File(s) |
|---|------|--------|---------|
| 12 | Split `trivial.sls` into focused modules | 2 h | `trivial.sls` |
| 13 | Document `application.sls` limitation | 10 min | `application.sls` |

---

## 4. Acceptance Criteria

After completing Phase 1 + Phase 2:
- All 16 rule files have at least one test assertion.
- `test.sh` passes (or the subset of type-substitution tests passes).
- No existing tests are broken.
- `cond` and `if` conditions are constrained to `boolean?` instead of `something?`.
- `let*.sls` and `letrec.sls` share a single implementation helper.
- `do.sls` links the parent node to its `result ...` expressions.
