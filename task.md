# Diagnostic System Improvement Plan

> Generated from review of `doc/protocol/diagnostic.md` and current code.
> Status: pending approval.

---

## 1. Background

Current diagnostics are produced in several places:

| Diagnostic | Source | Severity | Code |
|------------|--------|----------|------|
| Syntax / bracket errors | `analysis/tokenizer.sls` | Error | `syntax-error` |
| File not found | `analysis/tokenizer.sls` | Error | `file-not-found` |
| Duplicate identifier | `analysis/identifier/reference.sls` | Error | `duplicate-identifier` |
| Identifier resolution failure | `analysis/abstract-interpreter.sls` | Warning | `identifier-resolution-failure` |
| Library not found | `analysis/identifier/rules/library-import.sls` | Warning | `library-not-found` |
| Load file not found | `analysis/identifier/rules/load.sls` | Warning | `load-file-not-found` |
| Unused import | `analysis/workspace.sls` | Warning | `unused-import` |
| Type inference / rule warnings | `analysis/workspace.sls`, `type/substitutions/generator.sls` | Warning | `type-inference-warning`, `type-rule-warning` |
| Analysis error | `analysis/workspace.sls` | Error | `analysis-error` |

This plan only includes items that have been confirmed after discussion.
Items that were rejected or need more analysis are listed at the end for reference.

---

## 2. Confirmed Work Items

### 2.1 Fix typo in identifier-resolution-failure message

**File:** `analysis/abstract-interpreter.sls:134,148`

Current message:
```scheme
"Scheme-langserver Warning: Fail to catch identifiers"
```

Problems:
- Code already uses `Warning`; only documentation had the typo.
- Message is vague and does not name the failing identifier.

Scope of this task:
- Fix the typo only.
- **Do not** implement precise per-symbol undefined-identifier diagnostics here; that requires binding-position tracking and is a larger project.

Acceptance criteria:
- `grep -R "Warnning" analysis/ doc/` returns no matches.
- Existing tests still pass.

Estimated effort: 5 minutes.

---

### 2.2 Improve diagnostic message text quality

**Files:** multiple (see list below)

Current messages that should be cleaned up:

| Location | Current | Proposed |
|----------|---------|----------|
| `library-import.sls`, `define-library-import.sls`, `load.sls` | `"Fail to find library:foo"` / `"Fail to find file:foo"` | Add a space after the colon: `"Fail to find library: foo"` |
| `abstract-interpreter.sls:134,148` | `"Scheme-langserver Warning: Fail to catch identifiers"` | `"Scheme-langserver Warning: fail to catch identifiers"` (typo fix + lowercase for consistency) |
| `workspace.sls:433,439` | `"Analysis error: ..."` | Keep, but ensure `display-condition` output is readable |
| `workspace.sls:481,484` | `"Type inference warning: ..."` | Keep; these are already readable |

Note:
- `library-identifier->string` already trims trailing newlines after a recent fix, so messages should no longer end with `\n`.
- This task is only about human-readable message strings, not about adding new codes or LSP fields.

Acceptance criteria:
- Messages use consistent spacing and capitalization.
- No regressions in tests that match message strings.

Estimated effort: 1–2 hours (including updating any tests that assert exact messages).

---

### 2.3 Add LSP `tags: [Unnecessary]` for unused import

**Files:** `protocol/apis/document-diagnostic.sls`, `virtual-file-system/document.sls` (if needed)

Current `Diagnostic` object emitted by `private:make-diagnostic`:
```scheme
(make-alist 'range ... 'severity ... 'message ... 'source ... 'code ...)
```

LSP also supports `tags` for diagnostics that are not errors but stylistic/noise:
- `1` = `Unnecessary`
- `2` = `Deprecated`

Scope:
- When `code` is `"unused-import"`, add `'tags (vector 1)` to the output.
- Do not add tags for other diagnostics yet.

Acceptance criteria:
- A published unused-import diagnostic includes `"tags": [1]`.
- `textDocument/publishDiagnostics` and `textDocument/diagnostic` both include the tag.
- Tests verify the tag.

Estimated effort: 2–3 hours.

---

### 2.4 Unused local variable diagnostic

**File:** `analysis/workspace.sls`

Detect local bindings introduced by `define`, `lambda`, `case-lambda`, `let`, `let*`, `letrec`, `let-values`, `do`, and `with-syntax` that are never referenced.

This is different from **unused import**:
- Unused import: an imported identifier is never used.
- Unused local variable: a binding introduced in this document is never used.

Example:
```scheme
(define (f x y)
  1)            ; x and y are local bindings, never used -> report both
```

But:
```scheme
(import (rnrs))
(define (f) 1) ; define and 1 use standard bindings; no local binding is unused
```

Implementation sketch:
- After `step`, add a new pass `private:check-unused-local-variables`.
- Scan `document-ordered-reference-list` for references whose:
  - `identifier-reference-document` equals the current document (i.e. the binding is local),
  - `identifier-reference-type` is one of `variable`, `parameter`, or `procedure`,
  - `identifier-reference-usage-count` is 0,
  - identifier is not in the library's export list.
- Emit a diagnostic at the binding index-node:
  ```scheme
  `(start-bias end-bias 2 "Unused local variable: x" "identifier" "unused-local-variable")`
  ```

Caveats:
- **Exported bindings**: a top-level `(define x ...)` that is exported is considered used by the export and should not be flagged.
- **Mutually recursive definitions**: `even?` and `odd?` reference each other, so their `usage-count` is greater than 0 and they will not be flagged.
- **Lambda parameters**: reporting every unused parameter can be noisy (e.g. callback signatures). For a first version, still report them but consider using severity `Hint` or a suppression convention such as a leading underscore if feedback shows too much noise.
- **Top-level unexported defines**: these are not exported and not used locally. They are technically dead code and can be flagged, but this may be surprising. Start by flagging them; tune if needed.

Acceptance criteria:
- Fixture with unused local variables produces the new diagnostic.
- Fixture with used local variables does not.
- Exported top-level bindings are not flagged.
- Tests added.

Estimated effort: 3–5 days.

---

### 2.5 Duplicate import diagnostic

**File:** `analysis/workspace.sls`

Detect when the same library is imported more than once in the same document.

Example:
```scheme
(import (rnrs))
(import (rnrs))
```

Implementation sketch:
- In `private:check-unused-imports`, while walking `import` clauses, collect the resolved library identifiers per document.
- Use an `equal-hashtable` keyed by library identifier.
- If the same identifier appears more than once, emit a warning on the second and subsequent occurrences:
  ```scheme
  `(start end 2 "Duplicate import: (rnrs)" "import" "duplicate-import")`
  ```

Caveats:
- Different import modifiers should be considered different if they bring in disjoint bindings? For a first version, treat any repeated library identifier as duplicate, regardless of `only`/`except`.
- Do not flag `(import (rnrs))` and `(import (rnrs base))` as duplicates even though one is a subset of the other; only exact identifier matches.

Acceptance criteria:
- Fixture with duplicate imports produces the new diagnostic.
- Fixture with distinct imports does not.
- Tests added.

Estimated effort: 1 day.

---

### 2.6 Import modifier errors

**File:** `analysis/identifier/rules/library-import.sls` and r7rs variant

Detect when `only`, `except`, `rename`, or `alias` references an identifier that the target library does not export.

Example:
```scheme
(import (only (rnrs) not-exported-symbol))
```

Implementation sketch:
- When processing an `only`/`except`/`rename`/`alias` clause, the code already computes `imported-references` (the set of refs that match the requested identifiers).
- If a requested identifier has zero matching refs, emit:
  ```scheme
  `(start end 2 "Identifier not exported: not-exported-symbol" "import" "identifier-not-exported")`
  ```
- For `rename`/`alias`, check the external name, not the internal name.

Caveats:
- Built-in / meta libraries may not have export lists in the same way; skip or be conservative for them.
- This is different from `library-not-found`: the library exists, but the requested binding does not.

Acceptance criteria:
- Fixtures for `only`/`except`/`rename` with non-existent bindings produce diagnostics.
- Valid imports do not produce false positives.
- Tests added.

Estimated effort: 2–3 days.

---

### 2.7 Reduce repeated AST scanning in unused-import detection

**File:** `analysis/workspace.sls`

Current implementation:
1. `private:mark-used-imports` walks the entire AST to build `used-ht`.
2. `private:check-unused-imports` walks the entire AST again to inspect `import` clauses.

Both walks can be merged into a single pass:
- First pass collects usage information **and** records the locations of all `import` clauses and their modifier nodes.
- Second pass only inspects the recorded import nodes.

Implementation sketch:
- Replace `private:mark-used-imports` with `private:collect-import-usages` that returns two values: `used-ht` and a list of import-clause records.
- Each import-clause record contains the index-node and enough context to avoid re-walking.
- `private:check-unused-imports` then iterates over the collected import-clause records.

Caveats:
- This is a refactoring, not a behavior change; existing tests must continue to pass.
- Performance gain is modest unless files have many imports or deep ASTs.

Acceptance criteria:
- All existing unused-import tests pass.
- No functional change in emitted diagnostics.

Estimated effort: 2–3 days.

---

### 2.8 Add tests for diagnostic codes and sources

**Files:** `tests/protocol/apis/test-document-diagnostic.sps`, `tests/analysis/identifier/test-unused-import.sps`, `tests/analysis/identifier/test-duplicate.sps`

Current tests verify messages but do not systematically verify `source` and `code` fields.

Add assertions for:
- `duplicate-identifier`: `source` = `"identifier"`, `code` = `"duplicate-identifier"`
- `library-not-found`: `source` = `"import"`, `code` = `"library-not-found"`
- `unused-import`: `source` = `"import"`, `code` = `"unused-import"`
- `syntax-error`: `source` = `"syntax"`, `code` = `"syntax-error"`

Also add tests for the new diagnostics in 2.4 and 2.5.

Acceptance criteria:
- At least one test asserts `source`/`code` for every common diagnostic.
- All tests pass.

Estimated effort: 1–2 days.

---

### 2.9 Update diagnostic documentation

**File:** `doc/protocol/diagnostic.md`

After the above changes, update the document:
- Add new codes: `duplicate-import`, `identifier-not-exported`.
- Document the `tags` field for `unused-import`.
- Add a section explaining each diagnostic code with examples.
- Remove or move items that were rejected.

Acceptance criteria:
- Documentation matches the implementation.
- No stale claims (e.g., claiming a feature is not implemented when it is).

Estimated effort: 1 day.

---

## 3. Items Requiring More Analysis / Discussion

### 3.1 Precise undefined-identifier diagnostic

Rejected for now because `find-available-references-for` returns empty for both:
- truly undefined symbols
- local bindings (let/lambda/define parameters)

To implement this reliably, we need binding-position tracking across all binding forms, including quoted symbols and library-name components. The previous attempt (`5545e4c`) was reverted for this reason.

Recommended next step: a small research spike to track whether local binding positions can be recorded without fragility.

---

### 3.2 Standard-library import false positives

Clarification: the current "unused import" logic reports `(import (foo))` only if **no binding from `foo` was referenced**. For standard libraries like `(rnrs)` or `(chezscheme)`, the file usually uses `define`, `+`, `library`, etc., so they are rarely flagged.

The concern is whether this is useful. If the team decides that standard-library imports should never be reported as unused (because they are implicitly justified), we can add an exclusion list.

No action until a decision is made.

---

## 4. Explicitly Rejected Items

| Item | Reason |
|------|--------|
| Type-mismatch diagnostics | Type inference in this system generates substitutions; it does not solve constraints, so it cannot determine that an argument type does not match a signature. |
| `relatedInformation` | Not understood / needs separate design discussion. |
| `codeDescription` per code | Not understood / needs separate design discussion. |
| Pull diagnostics (`textDocument/diagnostic`) | Not supported. |
| Library-name component errors | Not supported. |
| Macro expansion diagnostics | Not supported. |
| Incremental diagnostic computation | Performance cannot support it yet. |

---

## 5. Proposed Implementation Order

1. **2.1** Fix typo (quick win).
2. **2.2** Improve message text quality.
3. **2.7** Add tests for source/code fields.
4. **2.3** Add `tags: [Unnecessary]` for unused import.
5. **2.4** Unused local variable diagnostic.
6. **2.5** Duplicate import diagnostic.
7. **2.6** Import modifier errors.
8. **2.8** Reduce repeated AST scanning.
9. **2.9** Update documentation.

---

## 6. Notes

- All changes must go through the pre-commit hook (`bash test.sh` subset for protocol API tests).
- Do not use `git commit --no-verify`.
- Before each code change, clear `.akku/libobj/scheme-langserver` if `.sls` files are edited.
