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
