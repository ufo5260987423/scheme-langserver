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
- [ ] **OPT-5** Incrementally maintain `all-pairs` in `expanded+callee-list`

---

## Files Involved

- `analysis/abstract-interpreter.sls`
- `analysis/identifier/expanders/expansion-wrap.sls`
- `analysis/identifier/expanders/syntax-rules.sls`
- `analysis/identifier/expanders/pattern.sls`
