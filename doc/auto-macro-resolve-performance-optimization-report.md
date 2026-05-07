# Auto Macro Resolve Performance Optimization Report

## Date
2026-05-04

## Summary
Implemented two optimization schemes (A + B) in `analysis/identifier/expanders/syntax-rules.sls` to reduce the per-call cost of `syntax-rules->generator:map+expansion`.

- **Scheme A**: Pre-compute `clause-index-nodes`, `clause-vector`, and `signatures-vector` once at initialization time (when the `syntax-rules` index-node is first processed), storing them as closure variables in the generator lambda. This avoids re-computing `(cddr (index-node-children input-index-node))` and the signature list on every generator invocation.

- **Scheme B**: Add lightweight signature-based clause filtering before `eval` + `syntax-case` in `private:confirm-clause`.  
  A signature is derived from each clause's pattern: `(min-param-count . param-shapes)`.  
  `param-shape` is one of `symbol`/`null`/`pair`/`vector`/`other`.  
  The filter requires the input expression's param count to be `>= min-param-count` and the leading params to match the fixed-prefix shapes (`symbol` matches anything, `pair` matches lists/null, etc.).  
  This is a conservative filter: it never rejects a clause that `syntax-case` might match, but it can skip many non-matching clauses before the expensive `eval` call.

## Test Results
All existing tests pass:
- `tests/analysis/identifier/expanders/test-pattern.sps` ✅
- `tests/analysis/identifier/test-auto-macro-resolve.sps` ✅
- `tests/analysis/identifier/test-match-expansion-compare.sps` ✅
- `tests/analysis/identifier/test-simple-macro-auto-resolve.sps` ✅
- `debug-trace/layered-auto-expand.ss` (12 layers) ✅

## Benchmark Methodology
Benchmark script: `debug-trace/benchmark-layers.sps`
- 50 iterations per layer
- Warmup iteration before timing
- Measures only the `expansion-generator` call time (excluding `init-workspace` overhead)
- Chez Scheme on Linux x86_64

## Raw Data

### Baseline (original code, no optimization)

| Layer | Macro | per-call (ms) |
|-------|-------|---------------|
| 1 | match | 2.82 |
| 2 | match-next | 1.34 |
| 3 | match-next | 1.05 |
| 4 | match-one | 0.89 |
| 5 | match-check-ellipsis | 1.28 |
| 6 | match-two | 5.20 |
| 7 | match-one | 0.94 |
| 8 | match-check-ellipsis | 1.19 |
| 9 | match-two | 2.00 |
| 10 | match-one | 0.98 |
| 11 | match-two | **13.57** |
| 12 | match-check-identifier | 1.37 |
| **Total** | | **~32.6** |

### Optimized (Scheme A + B)

| Layer | Macro | per-call (ms) | Speedup |
|-------|-------|---------------|---------|
| 1 | match | 1.82 | **1.55x** |
| 2 | match-next | 1.33 | 1.01x |
| 3 | match-next | 1.10 | 0.96x |
| 4 | match-one | 0.91 | 0.98x |
| 5 | match-check-ellipsis | 0.91 | **1.41x** |
| 6 | match-two | 4.96 | 1.05x |
| 7 | match-one | 1.30 | 0.72x |
| 8 | match-check-ellipsis | 0.94 | 1.27x |
| 9 | match-two | 1.89 | **1.06x** |
| 10 | match-one | 0.48 | **2.04x** |
| 11 | match-two | **1.92** | **7.07x** |
| 12 | match-check-identifier | 1.04 | 1.32x |
| **Total** | | **~18.6** | **1.75x** |

## Analysis

### Where the speedup comes from

1. **Layer 1 (`match`) — 1.55x speedup**: `match` has 5 clauses. The signature filter rejects 4 of them immediately (e.g. `(match)` has signature length 0, `(match atom)` has length 1, while the input `(match x ...)` has 2 params). Only the last clause `(match atom (pat . body) ...)` passes the filter. This reduces `eval` calls from 5 → 1.

2. **Layer 11 (`match-two`) — 7.07x speedup**: This is the biggest win. `match-two` has 28 clauses. The input for Layer 11 is `(match-two v path ...)`, where the second param is a plain symbol. Most `match-two` clauses expect special forms in the second position: `()` (null), `(quote p)` (pair), `(and)` (pair), etc. Only the final clause `(match-two v x g+s ...)` accepts a symbol in the second position. The signature filter rejects 27 clauses, leaving only 1 for `eval`.

3. **Layers with modest/no change**: When the input's leading params already have `pair` shape (e.g. `(match-two v (? string? path) ...)`), many clauses pass the signature filter because their patterns also use `pair` shape for the second param. In those cases `eval` still runs ~27 times, and the overhead of computing signatures is not amortized.

### Total cascade impact

A full 12-layer auto-resolve cascade went from **~32.6 ms** to **~18.6 ms**, a **1.75x** overall speedup. The dominant factor is the extreme speedup on Layer 11 (`match-two` with a symbol dispatch key).

## Files Modified

- `analysis/identifier/expanders/syntax-rules.sls` — Added signature extraction functions (`private:fixed-prefix`, `private:param-shape`, `private:extract-signature`, `private:shape-match?`, `private:params-shapes-match?`, `private:signature-match?`). Pre-computed `clause-vector` and `signatures-vector` in the outer scope. Modified `private:confirm-clause` to use vector iteration with signature pre-filtering.
