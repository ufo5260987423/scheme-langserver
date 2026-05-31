# Diagnostic Improvement Plan

> Status: P0 in progress  
> Created: 2025-05-13

---

## 1. Current State

The diagnose subsystem uses a minimal 4-tuple internally:

```scheme
(range-start range-end severity message)
```

- `range-start/end` — byte offsets into the document text.
- `severity` — LSP severity integer (`1`=Error, `2`=Warning, `3`=Information, `4`=Hint).
- `message` — human-readable string.

LSP conversion (`protocol/apis/document-diagnostic.sls`) maps this to a
`Diagnostic` alist with only `range`, `severity`, and `message`.  The following
LSP fields are **not emitted**:

| LSP field | Current support | Impact when missing |
|-----------|----------------|---------------------|
| `source` | ❌ | Client cannot distinguish tokenizer / type / identifier diagnostics. |
| `code` | ❌ | No classification; blocks filtering, Code Actions, and quick-fix menus. |
| `tags` | ❌ | Cannot mark `Unnecessary` or `Deprecated`. |
| `relatedInformation` | ❌ | Cannot show "defined here" / "imported here" cross-references. |

### 1.1 Sources of diagnostics today

| Source | File | Severity | Message example |
|--------|------|----------|-----------------|
| Tokenizer / Parser | `analysis/tokenizer.sls` | `1` (Error) | `"Syntax error: ..."` |
| Abstract interpreter | `analysis/abstract-interpreter.sls` | `2` (Warning) | `"Scheme-langserver Warning: Fail to catch identifiers"` |
| Library import (r6rs) | `analysis/identifier/rules/library-import.sls` | `2` (Warning) | `"Fail to find library:..."` |
| Library import (r7rs) | `analysis/identifier/rules/r7rs/define-library-import.sls` | `2` (Warning) | `"Fail to find library:..."` |
| File load | `analysis/identifier/rules/load.sls` | `2` (Warning) | `"Fail to find file:..."` |
| Type inference | `analysis/workspace.sls` | `2` (Warning) | `"Type inference warning: ..."` |
| Type rules | `analysis/type/substitutions/generator.sls` | `2` (Warning) | `"Type rule warning: ..."` |

### 1.2 Known gaps

1. **No precise undefined-variable diagnostic.**  When the abstract interpreter
   cannot resolve an identifier it emits a single file-level warning that does
   not name the identifier or its location.
2. **No type-mismatch diagnostics.**  The type system (`type:->?`, `type:=?`)
   exists but is only used for hover tooltips.  It is not wired into
   `publishDiagnostics`.
3. **No unused-variable / unused-import diagnostics.**  The reference-tracking
   infrastructure exists (`document-ordered-reference-list`) but reference-count
   statistics are not collected.

---

## 2. Roadmap

### P0 — Enrich diagnostic metadata (`source` + `code`)

**Goal:** Make diagnostics professional and filterable.

**Changes:**
1. Extend the internal diagnose format to accept optional `source` and `code`:
   ```scheme
   ;; Backward-compatible forms
   (range-start range-end severity message)               ; source="scheme-langserver", code=#f
   (range-start range-end severity message source)        ; code=#f
   (range-start range-end severity message source code)   ; full
   ```
2. Update `private:make-diagnostic` in `protocol/apis/document-diagnostic.sls`
   to emit `source` and, when present, `code`.
3. Update every `append-new-diagnoses` call site to supply a `source`.
4. Assign `code` values to the most common diagnostics.

**Proposed `source` values:**

| Subsystem | `source` string |
|-----------|-----------------|
| tokenizer / parser | `"syntax"` |
| abstract interpreter (identifier resolution) | `"identifier"` |
| library / import resolution | `"import"` |
| file load | `"load"` |
| type inference / type rules | `"type"` |

**Proposed `code` values:**

| Diagnostic | `code` |
|------------|--------|
| Syntax error (tokenizer) | `"syntax-error"` |
| File not found (tokenizer) | `"file-not-found"` |
| Library not found | `"library-not-found"` |
| File not found (load) | `"load-file-not-found"` |
| Identifier resolution failure | `"identifier-resolution-failure"` |
| Type inference warning | `"type-inference-warning"` |
| Type rule warning | `"type-rule-warning"` |

**Estimated effort:** 1–2 days.

---

### P1 — Precise undefined-identifier diagnostic

**Goal:** Replace the blanket `"Fail to catch identifiers"` with
`"Undefined identifier: foo"` at the exact symbol location.

**Implementation sketch:**
- In `abstract-interpreter.sls`, when `head-expression` is a symbol and
  `private:find-available-references-for` returns `'()`, emit a diagnostic
  with:
  - severity `1` (Error)
  - range = the symbol's index-node start/end
  - message = `(string-append "Undefined identifier: " (symbol->string head-expression))`
  - source = `"identifier"`
  - code = `"undefined-identifier"`

**Caveats:**
- Must not fire inside `quote` / `quasiquote` (already guarded by existing
  `quote?` / `quasiquote?` branches).
- Must not fire for syntax-parameters or macro auxiliary identifiers.
- Free variables valid in the REPL but not in the workspace may produce false
  positives; consider making this a Warning instead of Error, or adding a
  suppression list.

**Estimated effort:** 2–3 days.

---

### P2 — Type-mismatch diagnostics

**Goal:** Use the existing type system to emit warnings when argument types do
not match a function's signature.

**Implementation sketch:**
- In `analysis/type/substitutions/rules/application.sls`, after inferring the
  callee's type and the arguments' types, call `type:->?` for each parameter.
- If `type:->? actual expected` is `#f` and neither type is `something?`
  (unknown), emit:
  ```scheme
  (start end 2                            ; Warning
    (string-append "Type mismatch: expected " (type->string expected)
                   ", got " (type->string actual))
    "type" "type-mismatch")
  ```

**Caveats:**
- Polymorphic types (`(list? a ...)`) must be instantiated before comparison.
- When the type inferrer returns multiple possible types (Cartesian product),
  a mismatch in one branch should not necessarily produce a diagnostic unless
  *all* branches mismatch.
- Scheme is dynamically typed; severity should remain `Warning` (not `Error`).

**Estimated effort:** 1–2 weeks.

---

### P3 — Unused variable / unused import diagnostics

**Goal:** Mark variables and imports that are declared but never referenced.

**Implementation sketch:**
1. Extend `abstract-interpreter.sls` `step` to count references:
   - Each time `find-available-references-for` returns a non-empty list for a
     symbol usage, increment a reference counter on the resolved
     `identifier-reference`.
2. After analysis, walk `document-ordered-reference-list` and emit diagnostics
   for items with zero references:
   - `variable` type → `"Unused variable: foo"` (tag `Unnecessary`)
   - `library-import` type → `"Unused import: (foo bar)"` (tag `Unnecessary`)

**Caveats:**
- Top-level bindings exported from a library are "used" by the export, not by
  local references.
- Mutually recursive definitions may need special handling.
- Requires extending the `identifier-reference` record or adding a side table.

**Estimated effort:** 2–3 weeks.

---

### P4 — `relatedInformation`

**Goal:** Cross-reference diagnostics with their definition / import sites.

**Example:** When a library is not found, include a `relatedInformation` entry
pointing to the `import` clause that requested it.

**Implementation sketch:**
- Extend `private:make-diagnostic` to accept an optional list of
  `DiagnosticRelatedInformation` objects.
- Each object is an alist with `location` (uri + range) and `message`.

**Estimated effort:** 1 week.

---

## 3. Priority Summary

| Priority | Item | Effort | User value |
|----------|------|--------|------------|
| **P0** | `source` + `code` metadata | 1–2 days | High (professionalism) |
| **P1** | Precise undefined identifier | 2–3 days | **Very high** (most-requested feature) |
| **P2** | Type-mismatch warnings | 1–2 weeks | High (core differentiator) |
| **P3** | Unused variable / import | 2–3 weeks | Medium (code quality) |
| **P4** | `relatedInformation` | 1 week | Medium (UX polish) |

---

## 4. Quick wins (no architecture changes)

If a full phase is too large, these can be done in minutes:

1. **Prefix diagnostic messages with category** (no `source` field change):
   ```scheme
   ;; Before
   "Fail to find library:rnrs-base"
   ;; After
   "[import] Fail to find library: rnrs-base"
   ```

2. **Fix severity inconsistencies:**
   - `"Fail to catch identifiers"` is currently Warning (`2`).  If it becomes
     an undefined-identifier diagnostic, it should be Error (`1`).

3. **Add tests for `load.sls`:**  The file has a `;;todo more test` comment but
   zero test coverage.
