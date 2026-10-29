# Substitution Rules Audit — Open Items

> Rewritten 2026-10-29: the original audit's Phase-1 items #1 (let*/letrec dedup) and #3 (unless routing) have since landed — `letrec.sls` was deleted outright (named-let rules unified into `let*.sls`, which now dispatches `letrec`/`letrec*` too) and `unless.sls` exists as its own rule. The coverage table and several line counts went stale the same way. This file now tracks only the items verified **still open** against the current tree (`kimi` @ 2026-10-29: 15 rule files, 8 dedicated test files).

## Still open

### O1. Useless condition constraints — `cond.sls` / `if.sls`

Both still hard-code the condition type as `'something?` (the universal top type), conveying zero information:

```scheme
;; cond.sls
(extend-index-node-substitution-list first-child 'something?)
;; if.sls
(extend-index-node-substitution-list condition-index-node 'something?)
```

**Fix:** constrain conditions to `boolean?` instead. Low risk, semantically better default; Scheme tolerates non-boolean conditions, but the *intent* is boolean and the tighter constraint may surface real mistakes.

### O2. `do.sls` ignores its `(test result ...)` tail

The rule processes bindings (`var init update`) but never links the parent `do` node to the `result ...` expressions, so a `do` form's type never unifies with what it actually returns.

**Fix sketch:** after processing bindings, walk `(caddr children)`'s children, skip the test predicate, and register two-way substitutions between the `do` node and each result expression.

### O3. `case.sls` never narrows the key against datums

The key is forwarded to each clause's datum list, but no constraint ties the key's type to the datums' types. Full narrowing needs datum evaluation; minimum acceptable step is a regression test pinning the forwarding behavior.

### O4. `application.sls` registers no application constraint

```scheme
(define (application-process document index-node)
  (extend-index-node-substitution-list index-node (index-node-children index-node)))
```

The node's children become its possible types, but no argument↔parameter constraint is created — real application inference needs callee type resolution through the identifier-reference system. **Recommendation:** do not attempt without a larger design review; a code comment documenting the limitation is the proportionate step.

### O5. Test coverage gaps

8 dedicated test files for 15 rule files. Missing: `application`, `begin`, `case`, `cond`, `do`, `if`, `unless` (plus the S7 pair `define*`/`lambda*`, which ride on `define`/`lambda` tests informally). Prefer minimal fixture-based unit tests over loading real workspace files; one rule per test.

### O6. `trivial.sls` is a kitchen sink (223 lines)

Handles literals, pairs/vectors, quoting forms, parameter implicit conversion, cross-document references, and the null list all in one module. Splitting (`trivial-literals` / `trivial-collections` / `trivial-quoting`) is a medium-effort, low-urgency maintainability refactor.

## Done (kept for the record)

- ~~`let*.sls` / `letrec.sls` byte-identical duplication~~ — `letrec.sls` deleted; `let*.sls` unified named-let and `letrec`/`letrec*` dispatch.
- ~~`unless` routed to `begin-process`~~ — own `unless-process` exists (generator dispatches `(unless)` to it).
