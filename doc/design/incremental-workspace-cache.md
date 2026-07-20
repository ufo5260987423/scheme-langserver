# Incremental Workspace Cache Design

> Status: design document, not yet implemented.
>
> Last updated after measuring actual save/relink costs on the
> scheme-langserver project itself.

## 1. Background and Problem

The current workspace cache persists the entire analyzed workspace state into a single Chez FASL file (`workspace.fasl`). Measurements on the scheme-langserver project (128 `.sls` files) show:

| Operation | Time | Size |
|-----------|------|------|
| Cold `init-workspace` | ~38–43 s | — |
| Save FASL cache | ~42–60 s | ~40 MB |
| Load FASL cache | ~1.75 s | — |

The save time is **longer than the cold startup time**, which is unacceptable for normal LSP shutdown.

### 1.1 Save-time breakdown (measured)

| Step | Time |
|------|------|
| Clear `document-diagnoses` | ~0.08 s |
| `private:clear-non-serializable-fields!` (walk full object graph) | **~17 s** |
| `private:prepare-workspace-payload` (collect mtimes + convert hashtables) | **~17 s** |
| `fasl-write` the whole object graph | **~27 s** |

The two biggest avoidable costs are:

1. **Walking the entire object graph** to clear procedure-valued fields.
2. **Walking the entire file tree** to collect mtimes and convert `file-linkage` maps.

In typical LSP usage, only a small fraction of files change between server restarts. Re-serializing the entire workspace on every shutdown is wasteful.

## 2. Goal

Reduce the cost of saving the workspace cache by only persisting the parts that have actually changed since the last save, **without increasing cold-start time**.

Constraints:

- Chez `fasl-write` cannot serialize procedures/closures.
- Chez `fasl-write` cannot serialize `equal-hashtable` (only `eq-hashtable`).
- `fasl-read` produces fresh objects; existing loaded object graphs cannot be patched in place.
- A full-graph relink pass costs **~14 s** on this project and must be avoided during startup.

## 3. Original Idea: Per-File Fragments + Full Relink

The first design split the cache into:

```
<cache-path>/
  manifest.scm
  base.fasl
  files/<safe-path>.fasl
```

After loading all fragments, a **re-linking pass** would restore cross-document
`identifier-reference` pointers by rebuilding a global lookup table.

### Why this was rejected

A simulated full-graph relink on the scheme-langserver project took **~14 s**:

| Object type | Count |
|-------------|-------|
| `index-node` | ~455,000 |
| `identifier-reference` | ~50,000 |

That would more than double the cached startup time (~1.75 s → ~16 s).  Since the whole point of the cache is fast startup, full relink is not acceptable.

## 4. Revised Design: Per-File Fragments + Identifier-Reference Registry

### 4.1 Core idea

Avoid relink by keeping a **global identifier-reference registry** that is loaded once from `base.fasl`.  Cross-document references are stored as registry keys inside per-file fragments, not as object pointers.  After loading the registry, resolving a cross-document reference is a hashtable lookup, not a graph traversal.

### 4.2 On-disk layout

```
<cache-path>/
  manifest.scm              ; metadata + fragment list
  base.fasl                 ; skeleton + identifier-reference registry
  files/
    <path-hash-1>.fasl      ; per-file document/index-node fragment
    <path-hash-2>.fasl
    ...
```

### 4.3 What goes into `base.fasl`

- Workspace skeleton
  - `file-node` tree (without leaf `document` payloads)
  - `library-node` tree
  - `file-linkage` matrix + maps
- **Identifier-reference registry**: a hashtable from `(document-uri symbol library-identifier)` to `identifier-reference`
- Workspace configuration (`facet`, `top-environment`, `type-inference?`, `threaded?`)

The registry is the key new component.  It is populated during analysis and kept up to date as references are created.  Because it contains only `identifier-reference` records (not the whole `index-node` graph), it is much smaller than the full workspace object graph.

### 4.4 What goes into each file fragment

- The leaf `file-node` with its `document`
- The `document` text + `index-node-list`
- Local `identifier-reference` records whose `identifier-reference-document` is this file are **not duplicated** here; they live in the registry in `base.fasl`.
- Cross-document references stored inside `index-node` fields are serialized as **registry keys**, not as direct object pointers.

### 4.5 Save flow

```
1. Determine what changed since last save
   - compare disk mtime with manifest mtime for each cached file
   - detect deleted files (in manifest but not on disk)
   - detect new files (on disk but not in manifest)

2. If files were added/removed or library headers changed:
   - rebuild file-node tree / library-node tree / file-linkage
   - rewrite base.fasl (including the registry)

3. For each changed file:
   - build a per-file payload
   - clear non-serializable fields only inside this file's subgraph
   - serialize cross-document references as registry keys
   - fasl-write to a tmp file
   - atomic rename to files/<safe-path>.fasl
   - update manifest entry

4. For each deleted file:
   - delete its fragment file
   - remove manifest entry
   - (optionally) remove stale entries from registry when rewriting base.fasl

5. Atomically rewrite manifest
```

### 4.6 Load flow

```
1. Read manifest; validate versions/fingerprints.

2. Load base.fasl to get:
   - workspace skeleton
   - identifier-reference registry

3. Load each file fragment listed in the manifest:
   - attach document to the skeleton file-node
   - resolve cross-document registry keys using the registry
   - only the references actually present in the file need resolution

4. For files whose mtime differs (or new files on disk):
   - re-parse and re-analyze
   - write updated fragments

5. Return workspace.
```

Because the registry is already loaded in step 2, resolving cross-document references is **O(number of cross-document references)** via hashtable lookup, not **O(total index-node count)**.

### 4.7 Manifest format

```scheme
(cache-manifest
  (format-version 3)
  (langserver-version "2.1.6")
  (chez-version ...)
  (machine-type ...)
  (record-fingerprint ...)
  (facet akku)
  (top-environment r6rs)
  (type-inference? #f)
  (threaded? #f)
  (base-saved-at <timestamp>)
  (file-fragments
    ((path "/abs/path/to/lib.sls")
     (fragment "files/...hash...fasl")
     (mtime (<sec> . <nsec>))
     (saved-at <timestamp>))
    ...))
```

The manifest is a plain S-expression file (text, not FASL) so it is cheap to read and write.

## 5. Registry Maintenance

The registry must be kept consistent with the `identifier-reference` records used during analysis.

Options:

1. **Eager registration**: every constructor of `identifier-reference` also registers it.  Simple and robust.
2. **Lazy scan before save**: scan all documents and collect references.  Adds save-time cost but requires fewer code changes.

Recommendation: **eager registration**.  The `make-identifier-reference` constructor can register the new record automatically.  When a document is refreshed, stale entries for that document can be removed from the registry before new references are created.

## 6. Low-Cost Alternative Before Full Split Cache

Before implementing per-file fragments, two simpler changes can reduce save time from ~42 s to ~30 s with very low risk:

### 6.1 Avoid walking the whole graph to clear procedure fields

Current code (`analysis/workspace.sls:127`):

```scheme
(define (private:clear-non-serializable-fields! workspace)
  (let ([seen (make-eq-hashtable)])
    (define (visit obj) ...)
    (visit (workspace-file-node workspace))
    (visit (workspace-library-node workspace))
    (visit (workspace-file-linkage workspace))))
```

This walks every reachable object.  Instead, keep two global lists:

```scheme
(define index-nodes-with-expansion-generators '())
(define identifier-references-with-syntax-expanders '())
```

Whenever a field containing a procedure is set, push the object onto the appropriate list.  At save time:

```scheme
(for-each (lambda (n) (index-node-expansion-generator-set! n '()))
          index-nodes-with-expansion-generators)
(for-each (lambda (r) (identifier-reference-syntax-expander-set! r #f))
          identifier-references-with-syntax-expanders)
```

This reduces the clearing cost from ~17 s to near zero.

### 6.2 Avoid walking the whole file tree to collect mtimes

Current code (`analysis/workspace.sls:196`) calls `file-modification-time` for every file during save.  The workspace already maintains `path->mtime-cache` (`workspace-path->mtime-cache`).  Instead of re-reading disk mtimes during save, serialize the cache directly.

This reduces the preparation cost from ~17 s to near zero.

### 6.3 Combined effect

| Optimization | Save-time reduction |
|--------------|---------------------|
| Avoid full-graph clear | ~17 s |
| Avoid full-tree mtime collection | ~17 s |
| **Total** | **~34 s** |

Remaining cost would be dominated by `fasl-write` (~27 s), so the save time would drop to roughly **27–30 s**.  This is not as good as per-file fragments, but it is much easier to implement and carries no startup-time risk.

## 7. Recommended Implementation Roadmap

### Phase 1: Low-cost wins (save ~12–15 s)

1. Maintain `index-nodes-with-expansion-generators` and `identifier-references-with-syntax-expanders` lists.
2. Replace `private:clear-non-serializable-fields!` with list-based clearing.
3. Serialize `path->mtime-cache` directly instead of re-collecting mtimes.

### Phase 2: Registry + split cache (save ~40 s in typical case)

1. Add `identifier-reference-registry`.
2. Modify `make-identifier-reference` to register new records eagerly.
3. Create `analysis/workspace-cache-split.sls` with split load/save.
4. Update `base.fasl` to include the registry.
5. Update per-file fragments to store cross-document references as registry keys.
6. Modify `init-workspace` to try split cache first, fall back to old `workspace.fasl`, then cold start.
7. Keep old single-file cache loader for backward compatibility during migration.

### Phase 3: Cleanup

1. Remove old `workspace.fasl` path once split cache is stable.
2. Update `AGENTS.md` and user docs.

## 8. Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Full-graph relink harms startup | Avoided by using registry + hashtable lookup |
| Registry becomes inconsistent | Eager registration; clear per-document entries on refresh |
| Per-file fragments create many small files | Use subdirectories or hashing to avoid huge flat directories |
| Cross-document references still need resolution | Limited to actual cross-document refs; registry lookup is O(1) |
| FASL cannot serialize new field types | Same workaround as today: clear procedure fields |
| Manifest corruption | Validate version/fingerprint; fallback to cold start |
| Incremental cache bugs | Keep old single-file loader as fallback |

## 9. Expected Benefits

For scheme-langserver itself (128 files, 40 MB current cache):

| Scenario | Current | After Phase 1 | After Phase 2 |
|----------|---------|---------------|---------------|
| First full save | ~42 s | ~27–30 s | ~42 s |
| Save after single-file edit | ~42 s | ~27–30 s | **~1–3 s** |
| Load unchanged workspace | ~1.75 s | ~1.75 s | **~2–3 s** |

Phase 1 reduces save time with no startup impact.  Phase 2 brings the big win for normal LSP restart-after-edit scenarios while keeping startup time essentially unchanged.

## 10. Rejected Alternative: Single FASL + Delta Log

Instead of splitting into many files, we could keep one `workspace.fasl` as the last full snapshot plus a `delta.fasl` containing only changed files.  On load we load both and overlay the delta.

Drawbacks:
- `delta.fasl` must still be rewritten on every save.
- Cross-document references in the delta still need relink.
- Less flexible than per-file fragments.

**Recommendation**: implement per-file fragments with registry.
