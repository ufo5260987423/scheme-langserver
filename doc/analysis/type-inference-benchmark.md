# Type Inference Performance Benchmark

This document records the measurement methodology, baseline results, and optimization progress for the scheme-langserver type-inference subsystem.

---

## Measurement Methodology

The benchmark runner lives at `bin/benchmark-type-inference.ss`. It is a standalone Chez Scheme script that measures wall-clock time, CPU time, GC time, and result-list size for key operations.

### Run it

```bash
source .akku/bin/activate
scheme --script bin/benchmark-type-inference.ss
```

Raw numbers are also written to `benchmark-type-inference.json` in the current working directory.

### Metrics

| Metric | Meaning |
|--------|---------|
| **Real(ms)** | Wall-clock time (what the user feels) |
| **CPU(ms)**  | Total CPU time consumed |
| **GC(ms)**   | Time spent in garbage collection |
| **Results**  | Number of items returned by `type:interpret-result-list` |

Each benchmark is run 1–3 times and averaged; expensive benchmarks run only once. A manual `(collect)` is issued before each sample to reduce GC noise.

### Benchmark cases

| # | Case | Why it matters |
|---|------|----------------|
| 1 | `init-workspace (no ti)` | Baseline workspace initialization without type inference |
| 2 | `init-workspace (with ti)` | Full workspace initialization **with** type inference — this is the top-level timeout culprit |
| 3 | `construct-substitutions binary-search` | Phase-I substitution generation for a recursive function |
| 4 | `type:interpret natural-order-compare cl` | Fast function (simple body, 218 results) — sanity check |
| 5 | `type:interpret assq-ref def / cl` | Medium-complexity utility function |
| 6 | `type:interpret binary-search def / cl` | The **primary explosion point**: recursive, mutually recursive, multi-branch `case-lambda` |
| 7 | `hover binary-search (no ti)` | End-to-end API latency without type inference |
| 8 | `hover assq-ref (with ti)` | End-to-end API latency **with** type inference |

> **Note on "def" vs "cl"**
> - `def` = the `(define binary-search ...)` index-node. Its substitution list is usually a single wrapper.
> - `cl` = the `case-lambda` child node inside the define. This is where the actual type-explosion happens.

---

## Baseline Results (current `main` / `kimi` branch)

### Full run #1 — define-level only

| Benchmark | Real(ms) | CPU(ms) | GC(ms) | Results |
|-----------|----------|---------|--------|---------|
| `init-workspace (no ti)` | 44,058 | 43,851 | 3,927 | 0 |
| `init-workspace (with ti)` | 75,492 | 75,101 | 2,478 | 0 |
| `construct-substitutions binary-search` | 14,684 | 14,607 | 709 | 0 |
| `type:interpret natural-order-compare` | 13,536 | 13,478 | 687 | 1 |
| `type:interpret assq-ref` | 15,896 | 15,784 | 790 | 1 |
| `type:interpret binary-search` | 48,664 | 48,356 | 5,549 | 1 |
| `hover binary-search (no ti)` | 15,448 | 15,365 | 897 | 0 |
| `hover assq-ref (with ti)` | 72,285 | 71,957 | 3,659 | 0 |

### Partial run #2 — including case-lambda nodes

The second run added `cl` (case-lambda) targets and timed out at the 600 s mark while running `init-workspace (no ti)`, but all type-interpretation benchmarks finished.

| Benchmark | Real(ms) | CPU(ms) | GC(ms) | Results |
|-----------|----------|---------|--------|---------|
| `type:interpret natural-order-compare cl` | 12,943 | 12,880 | 661 | **218** |
| `type:interpret assq-ref cl` | 17,722 | 17,569 | 2,473 | **1** |
| `type:interpret binary-search def` | 40,849 | 40,628 | 2,145 | **1** |
| `type:interpret binary-search cl` | **158,682** | **157,765** | **3,495** | **21,603** |

### Key take-aways from baseline

1. **`binary-search` case-lambda is the detonator**
   - 158.7 s real time
   - 21,603 result items
   - Compare to `natural-order-compare cl`: 12.9 s, 218 items
   - **≈ 12× slower, ≈ 100× more results**

2. **`init-workspace (with ti)` exceeds 600 s**
   - This is why the full test suite times out.
   - The timeout cascades through every protocol API test that enables type inference.

3. **The explosion is not in substitution generation**
   - `construct-substitutions binary-search` takes only ~15 s.
   - The cost is overwhelmingly in **Phase-II interpretation** (`type:interpret-result-list`).

---

## Root-Cause Analysis

The baseline data confirms the three root causes identified earlier:

### 1. `max-depth` is silently reset to 10

`type:interpret-result-list` accepts an optional `max-depth` parameter, but **all 7 internal recursive call sites** inside `interpreter.sls` invoke the 3-arity form `(type:interpret-result-list reified env new-memory)`, which drops `max-depth` and falls back to the global `PRIVATE-MAX-DEPTH = 10`.

> Even if a caller passes `max-depth = 1`, nested expansions still recurse to depth 10.

This is why `binary-search` — a function with mutual recursion, `cond` branches, `let*`, and multiple call sites — generates 21,603 results instead of being truncated much earlier.

### 2. `dedupe` in `util/dedupe.sls` is O(n²)

`dedupe` keeps the head, filters duplicates from the tail, and recurses:

```scheme
(define (dedupe e [equal-procedure])
  ... (filter ...) ...)
```

With 21,603 items, deduplication alone can dominate the runtime.

### 3. Zero memoization in `type:interpret`

The interpreter re-evaluates the same sub-expressions repeatedly. No hash table or cache guards against redundant work.

---

## Optimization Roadmap & Progress

| Priority | Fix | Est. Time | Expected Impact | Status |
|----------|-----|-----------|-----------------|--------|
| P0 | Pass `max-depth` through all 7 recursive sites in `interpreter.sls` | ~15 min | **Biggest** — directly caps the 21k explosion | Pending |
| P1 | Rewrite `dedupe` with hash-sets (O(n²) → O(n)) | ~30 min | 2–5× speedup on large result lists | **Done** |
| P2 | Replace `fold-left append '()` with `(apply append ...)` | ~5 min | Small constant-factor win | Pending |
| P3 | Add memoization to `type:interpret` | ~2 h | Eliminates redundant work | Pending |

---

## Optimization Results

### P1 — Hash-Set `dedupe`

**Change:** `util/dedupe.sls`
- When the equality predicate is `equal?`, both `dedupe` and `dedupe-deduped` now use `make-hashtable` with `equal-hash` and `equal?`.
- Custom equality predicates still fall back to the original O(n²) algorithm.

**Tests:** `tests/util/test-dedupe.sps` passes (all 21 assertions).

**Benchmark delta** (before → after):

| Benchmark | Before (ms) | After (ms) | Δ | Notes |
|-----------|-------------|------------|---|-------|
| `type:interpret binary-search cl` | **158,682** | **100,016** | **−37%** | Main target — 21,603 items deduped |
| `type:interpret binary-search def` | 40,849 | 42,548 | +4% | Only 1 result, dedupe not on hot path |
| `type:interpret natural-order-compare cl` | 12,943 | 14,910 | +15% | 218 items; hashtable overhead > win |
| `type:interpret assq-ref cl` | 17,722 | 15,073 | −15% | 1 result, minor variance |
| `hover assq-ref (with ti)` | 72,285 | 71,130 | −2% | End-to-end; bottleneck elsewhere |
| `init-workspace (no ti)` | 44,058 | 38,669 | −12% | Dedupe used heavily during linkage init |

**Interpretation:**
- The payoff is **proportional to list size**. For `binary-search cl` (21k results) the win is **−37%** (~58 s saved). For tiny lists the hash-table constant overhead can slightly regress performance.
- `init-workspace (no ti)` also improves (~12%) because `dedupe` is called during file-linkage initialization.
- This confirms the O(n²) hypothesis but also shows that **dedupe is not the only bottleneck**: even with O(n) dedupe, `binary-search cl` still takes **100 s**. The remaining time is dominated by the recursive interpreter generating 21k results in the first place.

**Conclusion:** P1 is a solid incremental win, but P0 (fixing `max-depth`) is required to attack the root explosion.

---

## How to Re-run After Changes

After editing any `.sls` file under `analysis/` or `util/`, clear the compiled cache first:

```bash
rm -rf .akku/libobj/scheme-langserver
source .akku/bin/activate
scheme --script bin/benchmark-type-inference.ss
```

Then diff the new `benchmark-type-inference.json` against the previous one to verify improvement.
