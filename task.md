# Task: Auto Macro Expansion Performance Optimization

## Background

`scheme-langserver` uses `expansion-generator->rule` in `analysis/identifier/expanders/expansion-wrap.sls` to auto-resolve macro expansion references. The core cascade (`match` -> `match-next` -> `match-one` -> `match-two` -> ...) is now functional after previous fixes to:

- `abstract-interpreter.sls`: expander-doc fallback for identifier resolution inside expansion trees
- `expansion-wrap.sls`: enhanced shallow-copy with reverse-map sync and all-pairs fallback
- `pattern.sls`: pair-form rest-variable binding and proper list splicing
- `syntax-rules.sls`: min-len alignment and symbol fallback for extra children

However, the full test suite (`bash test.sh`) is extremely slow because `.akku/libobj/scheme-langserver` compiled cache is missing and the auto-macro code itself has algorithmic hotspots.

## Performance Bottleneck Analysis

### 1. `private:find-expander-doc-for-node` — repeated parent-chain traversal (BIGGEST)

**Location**: `analysis/abstract-interpreter.sls:270-278`

`step` processes every AST node. Each call to `find-available-references-for` that fails in `current-document` falls back to `private:find-expander-doc-for-node`, which walks up the `index-node-parent` chain until it finds an `expanded+callee-list` entry. For deep ASTs this is O(depth) per node, invoked thousands of times.

**Fix**: cache expander-doc directly on index-nodes (or in a hashtable keyed by node) so lookup is O(1).

### 2. `build-reverse-map` + `assoc` — O(n) lookup in alist

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:74-77, 79-101`

`private:shallow-copy` builds `reverse-map` as an alist. `private:sync-to-parent-expansion` uses `(assoc target-node reverse-map)` which scans the entire list. When cascade expansion produces hundreds of pairs, this becomes expensive.

**Fix**: use `eq-hashtable` instead of alist; lookup drops from O(n) to O(1).

### 3. `apply append` recursion explosion

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:43-64`

`private:recursive-collect`, `private:recursive-filter`, and `private:find-nodes-by-symbol` all use `apply append` to merge child results. Each recursive call constructs intermediate lists, creating GC pressure on large expansion trees.

**Fix**: rewrite as tail-recursive accumulator + `reverse`.

### 4. `syntax-rules.sls` — repeated `length` + nested `append`

**Location**: `analysis/identifier/expanders/syntax-rules.sls:163-211`

`private:expansion+index-node->pairs` calls `(length compound-children)` and `(length children)` multiple times. `length` is O(n) on lists. It also nests `apply append` inside `map`.

**Fix**: cache lengths in `let*`; collect pairs with tail-recursive accumulator.

### 5. `extract-all-pairs` — full scan of `expanded+callee-list`

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:71-72`

Every `private:shallow-copy` call scans the entire `expanded+callee-list` to extract all pairs. The list grows linearly with cascade depth.

**Fix**: incrementally maintain `all-pairs` inside `expanded+callee-list` entries.

## Execution Plan

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

## Files Involved

- `analysis/abstract-interpreter.sls`
- `analysis/identifier/expanders/expansion-wrap.sls`
- `analysis/identifier/expanders/syntax-rules.sls`
- `analysis/identifier/expanders/pattern.sls`

---

# Memory Investigation: Auto Macro Expansion

## Background

While running the auto-macro test suite, multiple `scheme --script` processes each consume **800 MB+ RAM**, forcing manual kills. The issue is not limited to parallel test runs; even single-process `performance.sps` (full-project init) stays resident for 10+ minutes with high memory usage.

## Memory Bottleneck Analysis

### MEM-1: `append-references-into-ordered-references-for` — O(n² log n) reference maintenance

**Location**: `analysis/identifier/reference.sls:182`

Every call performs:
1. `(append old-list new-items)` — copies the **entire** existing list
2. `sort-identifier-references` — O(n log n) sort over the merged list
3. `ordered-dedupe` — O(n) recursive dedupe (no hashtable optimization despite the module comment)

Called **83 times** across the project. Inside `private:shallow-copy` (`expansion-wrap.sls:197`) it updates `document-ordered-reference-list` and `index-node-references-import-in-this-node`.

As the document accumulates thousands of references, each insertion re-allocates and sorts the whole list. This is the single largest source of GC pressure and intermediate list allocation.

**Fix**: defer sorting/deduping until read time (dirty-flag pattern), or replace `append` with `cons` and only sort on first query.

### MEM-2: `index-node-references-export-to-other-node` built with `append`

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

### MEM-3: `private:expander-doc-cache-ht` — global cache never cleared

**Location**: `analysis/abstract-interpreter.sls:270`

```scheme
(define private:expander-doc-cache-ht (make-eq-hashtable))
```

Only `hashtable-set!`, never cleared. During full-project analysis `step` visits thousands of nodes; every miss is cached. After a large workspace init the hashtable may hold tens of thousands of `(node . (expanded+callee-list . result))` entries.

**Fix**: clear the hashtable at the start of `init-references` / `private-init-references`.

### MEM-4: `make-identifier-reference` creates duplicate objects

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:179` and `:123`

`private:shallow-copy` and `private:sync-to-parent-expansion` create brand-new `identifier-reference` records (10 fields each) for every export/import pair. The same logical binding may be re-created dozens of times during cascade expansion.

**Fix**: intern / reuse identical references via a weak eq-hashtable keyed by `(identifier document index-node init-node library-id type top-env)`.

### MEM-5: `build-reverse-map` temporary hashtable per `shallow-copy`

**Location**: `analysis/identifier/expanders/expansion-wrap.sls:108`

Every `private:shallow-copy` call allocates a fresh `eq-hashtable`, fills it from `all-pairs`, then discards it. With hundreds of pairs and deep cascades this creates many short-lived hashtables.

**Fix**: since OPT-5 already incrementally maintains `all-pairs`, consider also incrementally maintaining `reverse-map` inside `expanded+callee-list` entries (same 6th-slot technique), turning per-call allocation into per-entry update.

### MEM-6: `clear-references-for` defined but never called

**Location**: `virtual-file-system/index-node.sls:198`

```scheme
(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (for-each clear-references-for (index-node-children index-node)))
```

`private-init-references` (`analysis/workspace.sls:144`) never clears old references before re-running `step`. On incremental refresh (`refresh-workspace-for`), new references are appended on top of stale ones, causing a memory leak.

**Fix**: call `(clear-references-for (car (document-index-node-list document)))` at the top of `private-init-references`.

## Execution Plan

- [ ] **MEM-1** Defer `sort`+`dedupe` in `append-references-into-ordered-references-for`
- [ ] **MEM-2** Replace `append` with `cons` (or eq-hashtable guard) for export lists
- [ ] **MEM-3** Clear `private:expander-doc-cache-ht` on each `init-references`
- [ ] **MEM-4** Intern / deduplicate `make-identifier-reference` in shallow-copy
- [ ] **MEM-5** Incrementally maintain `reverse-map` (optional, lower priority)
- [ ] **MEM-6** Call `clear-references-for` at start of `private-init-references`

---

# Appendix: Chez Scheme Memory Profiling Investigation Plan

## Available Tools

### 1. Runtime statistics (`statistics` / `sstats`)

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

### 2. Object-type census (`object-counts`)

```scheme
(object-counts)
;; => ((flvector (static 1 . 16))
;;     (pair (static 112336 . 1797376))
;;     (vector (static 23937 . 1465568))
;;     ...)
```

Returns per-generation counts and byte sizes for each primitive object type (`pair`, `vector`, `string`, `symbol`, `closure`, `hashtable`, etc.).

**Limitation**: cannot distinguish user record types (e.g. `identifier-reference` vs generic `vector`).

### 3. Guardian-based object tracking (`make-guardian`)

```scheme
(define g (make-guardian))
(g (make-identifier-reference ...))
(collect)
(g)  ;; => #f if reclaimed, or the object if still alive
```

Weak-reference guardian. Useful for observing whether short-lived objects are actually being reclaimed between GCs.

### 4. Process-level measurement (GNU `time -v`)

Already used in testing:
```bash
/usr/bin/time -v scheme --script test.sps
# => Maximum resident set size (kbytes): 446580
```

**Limitation**: only gives final peak RSS, not per-function attribution.

## Missing Tools (Chez Scheme does not provide)

| Tool | Status | Impact |
|------|--------|--------|
| Heap dump (`dump-memory`) | ❌ not bound | Cannot inspect individual object graphs |
| Per-record-type counts | ❌ `object-counts` only sees primitives | Cannot count `identifier-reference` instances directly |
| Line-level allocation profiler | ❌ not available | Cannot attribute bytes to specific source lines |
| Heap snapshot diff | ❌ not available | Must manually sample before/after |

## Investigation Plan

Given the tool constraints, the practical approach is **delta sampling** at key boundaries.

### Phase 1: Function-level allocation attribution

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

### Phase 2: Object-type census snapshots

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

### Phase 3: Guardian-based object survival check

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

### Phase 4: Process-level baseline with controlled memory cap

**Goal**: get a reproducible RSS peak for regression testing.

**Method**:
1. Use `ulimit -v` to set a virtual-memory ceiling (e.g. 2GB).
2. Run `test-match-cascade-auto-resolve.sps` with GNU `time -v`.
3. Record:
   - `Maximum resident set size (kbytes)`
   - `Minor (reclaiming a frame) page faults`
   - Elapsed wall-clock time
4. If the test completes within the cap, success. If it OOMs, the cap is the baseline threshold.

## Deliverable

A new file `bin/memory-investigation.ss` that:
1. Imports the above helpers.
2. Wraps `test-match-cascade-auto-resolve.sps` (or `performance.sps`) with Phase 1–3 instrumentation.
3. Prints a structured report (text or JSON) showing:
   - Per-function allocation deltas
   - Object-type census deltas
   - Guardian survival counts
   - Final RSS from GNU `time`

This script should be checked into the repo (not committed to `kimi` yet) so it can be reused for future MEM optimization verification.
