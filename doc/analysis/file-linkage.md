# file-linkage: File Dependency Graph

This document describes the design and usage of `analysis/dependency/file-linkage.sls`. This module is responsible for maintaining the **file dependency graph (file-linkage)** within a scheme-langserver workspace, providing the infrastructure for batch analysis, reference lookup, topological sorting, and incremental refresh.

---

## 1. Core Concepts

A scheme-langserver workspace contains many Scheme source files. These files reference each other via `import`, `load`, `include`, etc. The `file-linkage` module abstracts this reference network as a **directed graph** and provides the following capabilities:

1. **Build a full dependency graph**: Scan all files based on the virtual file system (`root-file-node` + `root-library-node`), parse their `import` / `load`, and construct an adjacency matrix.
2. **Query dependency relationships**: Given a file, quickly get what it **depends on** (outgoing) and what **depends on it** (incoming), supporting both direct neighbors and transitive closure.
3. **Incremental refresh**: When a file is modified or added, only update the corresponding row/column without rebuilding the entire graph.
4. **Topological batching**: Split the dependency graph into batches in topological order, ensuring files in each batch only depend on files in previous batches, enabling parallel or sequential analysis.
5. **Cycle handling**: Identify and break cyclic dependencies via `shrink-ids`, grouping files in a cycle into the same batch.

---

## 2. Data Structures

### 2.1 `file-linkage` Record Type

```scheme
(define-record-type file-linkage
  (fields
    (mutable path->id-map)  ; string-hashtable: path -> integer id
    (mutable id->path-map)  ; eq-hashtable:   integer id -> path
    (mutable matrix)))      ; vector<integer>: adjacency matrix (flattened)
```

- **`path->id-map`**: Maps absolute file paths to consecutive integer IDs using `string-hash` and `equal?`.
- **`id->path-map`**: Reverse mapping using `eq-hashtable`, for converting matrix row/column indices back to paths.
- **`matrix`**: A one-dimensional `vector` representing a **square matrix**. If there are `N` files, the matrix length is `N*N`. A value of `1` at `(i, j)` means file `i` **directly depends on** file `j` (i.e., `i` imports / loads `j`).

### 2.2 Matrix Encoding

The matrix is stored in row-major order using `encode` / `decode` from `util/matrix.sls`:

```scheme
(encode N i j) => (+ (* i N) j)
(decode N index) => `(,i ,j)
```

Since `N` is known at initialization time, all subsequent matrix operations derive the dimension via `(sqrt (vector-length matrix))`.

---

## 3. Initialization Flow

### 3.1 `init-file-linkage`

Entry point, supports two calling conventions:

```scheme
(init-file-linkage root-file-node root-library-node)
(init-file-linkage root-file-node root-library-node top-environment)
```

Optional `top-environment` values are `'r6rs` (default), `'r7rs`, and `'s7`, which determine which import parser to use.

Initialization happens in two steps:

1. **`init-maps`**: Recursively traverses all `library-node`s and `file-node`s under `root-library-node`, assigning a unique integer ID to each file and populating `path->id-map` and `id->path-map`.
2. **`init-matrix`**: Traverses all files again. For each file, parses its AST (via `document-index-node-list`) and extracts:
   - **Imported library files** (`get-imported-libraries-from-index-node`)
   - **Loaded files** (`load-process`)
   
   Then writes `1` to the corresponding position in the matrix.

### 3.2 Extracting Imported Libraries

```scheme
(get-imported-libraries-from-index-node root-library-node index-node [top-environment])
```

This function selects a different processor based on `top-environment`:

| Environment | Processor |
|-------------|-----------|
| `r6rs` | `library-import-process` |
| `r7rs` | `library-import-process-r7rs` |
| `s7`   | `library-import-process-r7rs` |

The processor extracts the list of imported library identifiers from the `index-node`, then uses `walk-library` to find the corresponding `library-node` under `root-library-node`, and finally returns the paths of all `file-node`s under those library nodes.

---

## 4. Query API

### 4.1 Basic Matrix Operations

```scheme
(file-linkage-take linkage from-path to-path)  ; => 0 or 1
(file-linkage-set! linkage from-path to-path)  ; sets to 1 (default)
```

### 4.2 Direct Neighbor Queries

```scheme
(file-linkage-from linkage from-path)  ; => list of files directly depended on by from-path
(file-linkage-to   linkage to-path)    ; => list of files that directly depend on to-path
```

Internally implemented by scanning the corresponding row/column of the matrix.

### 4.3 Transitive Closure Queries

```scheme
(get-reference-path-to   linkage to-path)    ; => all (direct + indirect) files that depend on to-path
(get-reference-path-from linkage from-path)  ; => all files that from-path (direct + indirect) depends on
```

Internally uses `linkage-matrix-to-recursive` and `linkage-matrix-from-recursive` for BFS/recursive expansion. The result includes the starting file itself.

### 4.4 Finding Head Nodes

```scheme
(file-linkage-head linkage)
```

Returns a list of files with **in-degree 0** (i.e., files not depended on by any other file). In the scheme-langserver batch processing flow, these files are typically the starting points for analysis.

---

## 5. Incremental Refresh

### 5.1 `refresh-file-linkage&get-refresh-path`

When a user modifies or saves a file in the editor, there is no need to rebuild the entire graph. This function handles incremental updates:

```scheme
(refresh-file-linkage&get-refresh-path
  linkage root-library-node file-node
  new-index-node-list new-library-identifier-list
  [top-environment])
```

Steps:

1. Get the `id` for the file. If it is a newly added file and `new-library-identifier-list` is non-empty, dynamically expand `path->id-map` / `id->path-map`, and expand the matrix capacity via `matrix-expand`.
2. Compute the file's **original dependency set** (`old-imported-file-ids`) and its **new dependency set** after modification (`new-imported-file-ids`).
3. Clear old dependencies (set to `0`) and write new dependencies (set to `1`).
4. Return a list of **affected paths** that need re-analysis:
   - All files that (directly or indirectly) **depend on this file** (`reference-id-from`)
   - The file itself
   - All files that this file (directly or indirectly) **depends on** (`reference-id-to`)

> Note: In the current implementation, if a file is deleted and `new-library-identifier-list` is empty, an empty list is returned (TODO: matrix shrink).

---

## 6. Topological Batching and Cycle Handling

### 6.1 `get-init-reference-batches`

```scheme
(get-init-reference-batches linkage)
```

Splits the file dependency graph into topological batches (`list<list<path>>`). Example usage:

```scheme
(let ([batches (get-init-reference-batches linkage)])
  ; batches looks like '((file-a file-b) (file-c) (file-d file-e))
  ; Files within the same batch do not depend on each other (or are in the same cycle),
  ; so they can be analyzed in parallel.
  ; Different batches have a strict dependency direction and must be analyzed sequentially.
  )
```

### 6.2 `shrink-ids`: Maximizing Parallelism in Dependency Chains

In a typical Scheme project, the `file-linkage` graph contains many long `import` chains. However, a significant portion of these chains are **parallel rather than sequential** — they branch out independently and do not depend on each other. The purpose of `shrink-ids` is to exploit this parallelism by "peeling" the graph layer by layer, grouping as many independent files as possible into the same batch.

The implementation is based on **Kahn's algorithm** for topological sorting, adapted to maximize batch size:

```scheme
(define (shrink-ids matrix ids)
  ...)
```

**Pre-processing ($O(n^2)$ one-time cost):**

1. Build an `eq-hashtable` (`id-set`) for $O(1)$ membership testing of the `ids` set.
2. For each node in `ids`, scan the matrix row to find neighbors that are also in `ids`. Build:
   - `out-degrees`: internal out-degree of each node within the `ids` subgraph.
   - `in-adj`: reverse adjacency list (for each node, which nodes have an edge *to* it).

**Layer peeling:**

3. Initialise a queue with all nodes whose **internal out-degree is 0**. These are the "outermost leaves" — they no longer depend on any remaining node in the current set, so they can be safely analysed **in parallel** as one batch.
4. Repeatedly drain the queue into a batch. For each node removed, use `in-adj` to decrement the out-degree of its predecessors. When a predecessor's out-degree drops to 0, it becomes ready for the **next** batch and is pushed onto the next queue.
5. Continue until the queue is empty. If all nodes have been consumed, return the accumulated batches.

**SCC (cycle) handling:**

6. If the queue is empty but nodes remain, the leftover nodes form one or more **strongly connected components (cycles)**. Rather than serialising them one-by-one (which destroys parallelism), the entire remaining SCC is **packed as a single batch**. All members of the cycle are then processed concurrently by `threaded-map`. This is the correct trade-off because cyclic dependencies are invalid in Scheme's library system anyway; splitting them does not improve analysis accuracy, but keeping them together preserves parallelism.

**Complexity:**
- The old implementation used a naive $O(n^2)$ scan per recursive layer, yielding $O(n^3)$ worst-case time for long chains.
- The new implementation performs one $O(n^2)$ pre-processing pass and then processes each edge exactly once during peeling, giving **$O(n^2)$ total time** (effectively $O(V+E)$ for the sparse graphs typical in Scheme projects).

### 6.3 `shrink-paths`

`shrink-paths` is the path-wrapper around `shrink-ids`: it converts paths to IDs, calls `shrink-ids`, and converts the result back to paths.

Its primary use case is in `analysis/workspace.sls` inside `refresh-workspace-for`. When a file is modified, `refresh-file-linkage&get-refresh-path` returns an "impact set" of paths that need re-analysis. This impact set often contains long `import` chains. `shrink-paths` flattens these chains into the **minimum number of batches with the maximum number of files per batch**, which is then passed to `init-references` for efficient parallel reference rebuilding.

---

## 7. Matrix Utilities `util/matrix.sls`

`file-linkage` relies on `util/matrix.sls` for low-level matrix operations:

| Function | Description |
|----------|-------------|
| `matrix-take` | Reads the value at position `(i, j)` |
| `matrix-set!` | Sets the value at position `(i, j)` |
| `matrix-from` | Returns all column indices with value `1` in the given row (outgoing neighbors) |
| `matrix-to`   | Returns all row indices with value `1` in the given column (incoming neighbors) |
| `matrix-expand` | Expands an `N x N` matrix to `(N+1) x (N+1)`, used when adding a new file |
| `find-cycle`  | Detects cycles in the graph via DFS |
| `encode/decode` | Converts between 1D vector index and 2D coordinates |

---

## 8. Usage Scenarios and Call Chains

### 8.1 Workspace Initialization

In `analysis/workspace.sls`, `init-workspace` calls `init-file-linkage`:

```scheme
(let ([linkage (init-file-linkage root-file-node root-library-node top-environment)])
  ...)
```

### 8.2 Reference Analysis (references)

In `protocol/apis/references.sls` and `protocol/apis/document-highlight.sls`, `get-reference-path-to` / `get-reference-path-from` are used to determine the search scope, avoiding a full workspace scan.

### 8.3 Abstract Interpreter

In `analysis/abstract-interpreter.sls`, `get-init-reference-batches` is used to obtain topological batches. The abstract interpreter then performs type inference and identifier analysis on each batch in order.

---

## 9. Tests

Relevant tests are located at:

```
tests/analysis/dependency/test-file-linkage.sps
```

Coverage includes:

- `init-linkage-matrix`: Verifies that the dependency `workspace.sls -> io.sls` is correctly written into the matrix.
- `get-init-inference-path`: Verifies that `get-init-reference-batches` includes the specified file.
- `file-linkage-to`: Verifies reverse lookup (who depends on `error-code.sls`).
- `r7rs` / `s7` environment support: Verifies import parsing and dependency graph construction under different Scheme dialects.

---

## 10. Summary

`file-linkage` is the core component of the scheme-langserver dependency analysis layer. Its responsibilities can be summarized as:

1. **Graph construction**: Encode library references and `load` relationships from the file system into an adjacency matrix.
2. **Graph querying**: Provide direct neighbors, transitive closure, head nodes, and other queries.
3. **Graph maintenance**: Support incremental updates and dynamic expansion.
4. **Graph utilization**: Through topological layering and cycle compaction, flatten deep dependency chains into the fewest parallel batches possible, providing the correct analysis order and maximal concurrency for subsequent abstract interpretation, reference lookup, and type inference.
