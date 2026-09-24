# AGENTS.md — scheme-langserver Agent Guide

This file contains project conventions, build steps, test patterns, and gotchas
that are useful for AI agents working on the scheme-langserver codebase.

---

## 1. Project Overview

**scheme-langserver** is a Language Server Protocol (LSP) implementation for
Scheme, written in **Chez Scheme** and managed with the **Akku** package manager.

Key subsystems:
- `virtual-file-system/` — File-node tree, library-node tree, documents, index-nodes
- `analysis/` — Tokenizer, abstract interpreter, identifier reference resolution,
  type inference, dependency graph (file-linkage), document checkers
  (`document-checker/` — unused imports, unused local variables)
- `protocol/` — LSP message parsing and API handlers
- `util/` — Shared utilities (matrix, dedupe, path, io, etc.)

The server supports multiple Scheme dialects: **r6rs** (default), **r7rs**, **s7**, **fluent** (Ansys Fluent's embedded Petite Chez Scheme).

---

## 2. Build & Environment

### Prerequisites
- Chez Scheme (`scheme` binary)
- Akku (`akku` binary) for dependency management

### Activating the environment
```bash
# Always source this before running anything
source .akku/bin/activate
```

This sets `CHEZSCHEMELIBDIRS` so Chez can find libraries under `.akku/lib/` and
`.akku/libobj/`.

### Compiling the server

#### Release builds (static binary)
```bash
bash build.sh
```
This produces a static binary via `compile-chez-program --full-chez run.ss --static`.
`--full-chez` links against the full Chez runtime so that `fasl-write` works and
workspace cache can be saved from the compiled binary.

> **Note:** `--static` requires prerequisite tooling (e.g. musl libc toolchain). It is used in CI/release builds. For local development and testing, use the non-static build below.

#### Local development builds (non-static)
For local testing (faster, no extra dependencies):
```bash
source .akku/bin/activate
compile-chez-program --full-chez run.ss
```
This produces a dynamically linked `run` binary that is sufficient for local development, MCP integration testing, and log replay debugging. The resulting binary is much faster to build because it skips the static linking step. `--full-chez` is needed for workspace cache saving; the default petite runtime drops the `$write-fasl-bytevectors` primitive required by `fasl-write`.

### Running the server
```bash
scheme --script run.ss
# or
./run
```

---

## 3. Testing Conventions

### Test framework
Tests use **SRFI-64** (`(srfi :64 testing)`).

Boilerplate at the top of every test file:
```scheme
#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    ...)
```

Basic pattern:
```scheme
(test-begin "group-name")
  (test-equal expected actual)
  (test-equal #t (predicate? value))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
```

### Running tests

**Single test file** (fast, preferred during development):
```bash
source .akku/bin/activate
scheme --script tests/analysis/dependency/test-file-linkage.sps
```

**All tests** (slow, run via `test.sh`):
```bash
bash test.sh
```

### Key gotcha: stale `.so` cache

Akku caches compiled `.so` files under `.akku/libobj/`. **After editing any
`.sls` source file, delete the corresponding `.so` cache** before running tests,
or you will see errors like:

- `incompatible fasl-object version`
- `variable <name> is not bound`
- silent use of old code

Safe incantation after editing `analysis/**/*.sls`:
```bash
rm -rf .akku/libobj/scheme-langserver
```

If the error persists, also clear workspace-level caches:
```bash
rm -f .akku/libobj/scheme-langserver/analysis/workspace.chezscheme.so
rm -f .akku/libobj/scheme-langserver/analysis/workspace.chezscheme.wpo
```

---

## 4. Fixture Structure

Workspace fixtures live under `tests/resources/workspace-fixtures/<name>/`.

A minimal fixture for testing workspace / linkage / identifier analysis:

```
tests/resources/workspace-fixtures/simple-lib/
├── lib.scm.txt          # r6rs library source (renamed to .txt for txt-filter)
└── consumer.scm.txt     # another library that imports lib
```

Use `.scm.txt` extension so the `'txt` file-filter preset accepts them.
Initialize in tests with:

```scheme
(let* ([fixture (string-append (current-directory)
                               "/tests/resources/workspace-fixtures/simple-lib")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       ...)
  ...)
```

`init-workspace` arguments:
1. `path` — absolute path to fixture directory
2. `file-filter` — file filter configuration: `'akku`, `'txt`, a list of extensions such as `'(".sls" ".scm" ".scm.txt")`, or a custom predicate procedure
3. `'r6rs` — top environment (also `'r7rs`, `'s7`)
4. `#f` — threaded? (use `#f` in tests)
5. `#f` — type-inference? (use `#f` unless testing type inference)
6. (optional) `cache-path` — directory for the workspace FASL cache

Helper for locating children:
```scheme
(find (lambda (child) (string=? (file-node-path child) expected-path))
      (file-node-children root-file-node))
```

Or use `walk-file` for recursive lookup:
```scheme
(walk-file root-file-node (string-append fixture "/math.scm.txt"))
```

---

## 5. Coding Style

Observed conventions in existing code:

- **Indentation**: 2 spaces (no tabs). Indent by nesting level only; never align across lines.
  - General nesting: +2 spaces per level.
  - `let` / `let*` bindings: +4 spaces from the `let` keyword.
  - `let` / `let*` body: +2 spaces from the `let` keyword.
  - Function-call arguments inside `let` body: continue +2 per nesting level (do **not** flatten everything to the same column).
  ```scheme
  (import
    (chezscheme)
    (srfi :64 testing))

  (test-begin "group-name")
    (let* ([foo (init-foo)]
        [bar (workspace-bar foo)]
        [baz (construct-baz bar)])
      (process-baz baz)
      (test-equal #t
        (contain?
          (type:interpret-result-list baz)
          check-base)))
  (test-end)
  ```
- **Naming**:
  - Functions / variables: `kebab-case`
  - Record type accessors: `<record>-<field>` (e.g. `file-node-path`)
  - Private helpers: `private:<name>` or just internal `define`
- **Line endings**: LF.
- **Module headers**: `(library (scheme-langserver <path>) ...)`.
- **Imports**: group by subsystem, leave a blank line between groups.
- **Comments**: `;` for inline, `;;` for section dividers inside functions.

### Mutation style
Prefer `for-each` over `map` when the result is discarded (side-effect only).
This is a common fix in the codebase.

### Equality
- `string=?` for strings
- `equal?` for lists / deep structures
- `eq?` for symbols and small integers
- `=` for numeric comparison only

---

## 6. Module Dependency Rules

| Layer | May import from |
|-------|-----------------|
| `protocol/` | `analysis/`, `virtual-file-system/`, `util/` |
| `analysis/` | `virtual-file-system/`, `util/` |
| `virtual-file-system/` | `util/` only |
| `util/` | nothing inside the project (only standard libs) |

**Never** let `analysis/` import `protocol/`.

---

## 7. Common Traps & Debugging

### `directory-list` returns bare filenames
```scheme
(directory-list "/some/dir")
;; => ("foo.sls" "bar.sls")   -- NOT full paths
```
Always prepend the directory when constructing child paths:
```scheme
(string-append dir (string (directory-separator)) entry)
```

### `source-file->annotations` has two arities
- `(source-file->annotations path)` — re-reads from disk
- `(source-file->annotations source path)` — parses the provided string

Prefer the 2-arity version when you have already read the file into memory,
to avoid double I/O.

### Script files vs library files
- **Library files** have a `(library (name) ...)` header.
  `get-library-identifiers-list` returns a non-empty list.
- **Script files** have no library header.
  `get-library-identifiers-list` returns `'()`.

This distinction affects:
- `init-library-node` — script files attach directly under the root library-node
- `refresh-workspace-for` — script files bypass the linkage graph and go straight
to `undiagnosed-paths`

### `path->uri` and `uri->path`
Located in `util/path.sls`. The URI format is `file:///absolute/path`.
`path->uri` now correctly handles `.` and `..` in relative paths.

### Matrix operations in `util/matrix.sls`
- `encode` / `decode` use row-major order.
- `matrix-expand` grows a square matrix; `matrix-shrink` removes a row/column.
- Always derive dimension via `(sqrt (vector-length matrix))`.

### `inner:pair?` vs `inner:list?` in the type system
In Scheme a list is a chain of pairs terminated by `'()`, so `(cons x '())` is **both** a pair and a list. The langserver type system distinguishes them:

| Type | Meaning | Typical producers |
|------|---------|-----------------|
| `inner:pair?` | Any `cons` cell (proper or improper list) | `cons`, `list` (single element) |
| `inner:list?` | Proper list (chain of pairs ending in `'()`) | `'()`, `append`, `reverse`, `list` (≥0 elements) |

**Trap**: `cons`'s type rule in `rnrs-meta-rules.sls` returns `inner:pair?`, while `append` returns `inner:list?`. If you rewrite an accumulator loop from `(append result `(,x))` to `(cons x result)`, the type inferrer sees the recursive argument as `inner:pair?` instead of `inner:list?`, which can break substitution generation for named-let bindings. The fix is to keep `append` (or add a `reverse` at the return point and teach the type system that `(cons x <list>)` → `<list>`).

### `ufo-match` wildcard
`ufo-match` uses `:_` as the "match anything, don't bind" wildcard, **not** `_`.
`_` is treated as a normal pattern variable.

### Finding tests that exercise a module
```bash
grep -r "library-import-process" tests/
```

### Checking if a symbol is exported
Look at the `(export ...)` list at the top of the `.sls` file.

### Debugging from LSP client logs
The server can write a structured log (`read-message` / `send-message` pairs with timestamps) that is invaluable for tracking down latency or silent crashes.

**Key technique:** compare `read-message` timestamps with `send-message` timestamps for the same `id`.
```bash
# Extract request/response timeline
awk '
/^(read-message|send-message)$/ { mode=$0; next }
/^2026 / { if(mode!="") ts=$0; next }
mode=="read-message" && /"id":13,/ { printf "req  id=13 @ %s\n", ts }
mode=="send-message" && /"id":13,/ { printf "resp id=13 @ %s\n", ts }
' ~/ready-for-analyse.log
```

**If `send-message` stops but `read-message` continues**, the main loop is alive but the **request-queue worker thread is stuck or dead**. Check:
1. Did the last processed request trigger `init-references` under `workspace-mutex`?
2. Is `make-engine` + `expire` interacting badly with `workspace-mutex`?
3. Could a type-inference path (e.g. `type:interpret` → `private-generate-cartesian-product-procedure`) be throwing uncaught exceptions inside the engine wrapper?

**Replay scripts**
- `bin/log-debug.sps` — single-threaded replay (`#f` threaded). Fast, good for verifying fixes.
- `bin/parallel-log-debug.sps` — multi-threaded replay (`#t` threaded). Closer to real clients, but request ordering differs because all messages are injected instantly.

**A vector-in-list bug to watch for**
`analysis/type/substitutions/rules/trivial.sls` defines `index-of` using `car`/`cdr`/`null?`. If a caller passes a **vector** (e.g. `(index-of (list->vector rests) index-node)`), the `car` call throws `"~s is not a pair"`. In multi-threaded mode this exception may be swallowed by the engine layer instead of reaching `private:try-catch`, leaving the worker thread dead and all subsequent requests orphaned.

### `check-duplicate-identifiers` and `collect-parameter-pairs`
Two helpers live in `analysis/identifier/util.sls` (extracted from `reference.sls`):

- `check-duplicate-identifiers document pairs` — takes a list of `(symbol . index-node)` pairs, detects duplicates with an `eq-hashtable`, and appends a `"Duplicate identifier: ..."` diagnosis (severity 1 / Error).
- `collect-parameter-pairs index-node` — recursively extracts parameter symbols and their index-nodes from lambda/define parameter lists; handles flat lists, nested lists, and improper-list rest args. Returns a list of `(symbol . index-node)` cons cells.
- `dereference-index-node index-node` — returns the canonical definition node when `index-node-shared-reference` is set, otherwise the node itself.

Used in `lambda.sls`, `case-lambda.sls`, `let.sls`, `let*.sls`, `letrec.sls`, `let-values.sls`, `do.sls`, `define.sls`, `define-syntax.sls`, `with-syntax.sls`, `define-record-type.sls`, `fluid-let.sls`, `let-syntax.sls`, `letrec-syntax.sls`, `syntax-case.sls`, `syntax-rules.sls`, `s7/lambda*.sls`, and `s7/define*.sls`.

### `usage-count` tracking
The `identifier-reference` record has a mutable `usage-count` field (default 0).
- **Do not** increment it inside `find-available-references-for` (that function is called for internal lookups, guard checks, etc., not all of which represent a genuine "use").
- **Do** increment it explicitly in `abstract-interpreter.sls` when `step` successfully resolves a leaf symbol (the `[else` branch of the top-level `cond`).
- A post-phase `check-unused-imports` in `analysis/document-checker/unused-imports.sls` (called from `workspace.sls` after `step`) scans import clauses and reports imported references with `usage-count = 0` as `"Unused import: ..."` (severity 2 / Warning). Supports plain, `only`, `except`, `rename`, and `alias` imports. Unused local variables are checked separately by `check-unused-local-variables` in `analysis/document-checker/unused-local-variables.sls`.

### Pre-commit hook: never use `--no-verify`
The repository has a pre-commit hook (`.git/hooks/pre-commit`) that runs the protocol API test suite. **Do not bypass it with `git commit --no-verify`.** If the hook fails because tests are too slow or broken, fix the tests or the hook first, then commit normally.

> **Note:** The hook is intentionally slow (often 2–5 minutes on a cold cache) because it runs the full protocol API test suite in parallel. It first compiles shared modules via a warm-up test, then forks the remaining tests. If you see it hanging, it is usually waiting for Chez Scheme to compile `.so` files, not deadlocked. Be patient, or run `bash test.sh` manually beforehand to warm the cache.

---

## 8. Known Issues (as of current branch)

### Open issues

| Location | Issue | Impact |
|----------|-------|--------|
| `scheme-langserver` (general) | Cold initialization exceeds common LSP client timeouts (~38 s single-thread, ~48 s with type-inference). With a valid workspace cache this drops to ~2 s (after recent mtime-based consistency-check optimization), but the first run on a fresh machine / after cache invalidation can time out clients such as the MCP Bridge (30 s). | **High** — first-time user experience |
| `analysis/abstract-interpreter.sls:74` | Missing recursion guard for self-defined macro partial evaluation | Medium — can infinite-loop on certain macros |
| `analysis/identifier/rules/library-import.sls` | `alias` modifier does not add refs when used inside a `(library ...)` form (script-level `import-process` works fine) | Low — `alias` is rare in library headers |
| `protocol/apis/document-sync.sls:44` | Document sync has a TODO for optimization | Low — performance only |
| `protocol/analysis/request-queue.sls:59` | `expire` acquires `workspace-mutex` when `tickal-task-stop?` is true. Intent is correct (cancelled task may be updating workspace), but implementation is incomplete (does not wait for sub-threads to finish). Currently harmless because `with-mutex` is reentrant, but provides no actual protection either. | Low — retained for future completion |
### Resolved / fixed issues

| Location | Issue | Resolution |
|----------|-------|------------|
| `virtual-file-system/index-node.sls` + identifier rules | Cyclic and repeatedly-shared compound literals (e.g. `#1=((x 1) . #1#)`, `(#1=(x) #1#)`) caused identifier rules to crash with `~s is not a pair` or infinite-loop on raw S-expression recursion. `init-index-node` now builds an acyclic AST using a `shared-reference` field; binding-form rules (`let`, `let*`, `letrec`, `let-values`, `do`, `with-syntax`, `syntax-case`, `syntax-rules`, `define-syntax`, `define-record-type`, `fluid-let`, `let-syntax`, `letrec-syntax`, `s7/lambda*`, `s7/define*`) dereference shared nodes before accessing children, and `syntax-case`'s `get-all-symbols` now detects cycles. | **Fixed** (2026-07-02) — added `dereference-index-node` helper in `analysis/identifier/util.sls` and regression tests in `tests/analysis/identifier/rules/test-shared-reference-binding-forms.sps` |
| `analysis/identifier/rules/define-record-type.sls` | `process-define-record-type-tail` only handled the first clause of a `define-record-type` body and then stopped, so `(fields ...)` after `(nongenerative ...)` was ignored and setter/getter references were never created. | **Fixed** (2026-06-26) — continue the loop after `fields`, `parent`, and unmatched clauses |
| `analysis/identifier/rules/syntax-case.sls` | Parameter name typo: `root-librar-node` | **Fixed** (2026-06-26) — renamed to `root-library-node` |
| `analysis/workspace.sls` | `init-references` called `clear-references-for` on the `car` of `document-index-node-list`. Comment-only or otherwise empty files (common in S7 projects without `.akku/list`) produce an empty list, causing `car` of `()` and a `-32001` initialize failure. | **Fixed** (2026-06-13) — skip `clear-references-for` when `index-node-list` is empty in both threaded and single-threaded paths |
| `protocol/request.sls:26` | `read-message` now wraps `parse-content` in `guard`; malformed JSON and non-object roots return `'invalid` instead of crashing the server. | **Fixed** (2026-06-13) — `guard` catches parse errors |
| `protocol/request.sls:54` | `get-content-length` now validates the header as a non-negative integer and caps it at 10 MiB; malformed or negative values fall back to 0. | **Fixed** (2026-06-13) — validation added |
| `protocol/analysis/request-queue.sls` (`request-queue-pop`) | Multi-threaded runs intermittently lost queued requests and died with `Exception in thread-loop: ... no fail-handler` ("queue is empty" / "cdr: () is not a pair"), followed by a shutdown hang (`tests/robustness/robustness-concurrent.sps`). Long suspected to be an upstream Chez threading/GC race (cf. cisco/ChezScheme#1055, where a reproducer was posted). Root cause turned out to be plain logic: the empty-queue wait loop's recursive call was **not in tail position**, so after the inner frame dequeued and returned, the outer frame dequeued again — consuming tasks that were never delivered, leaving `first`/`last` inconsistent (duplicated elements, later invalid memory references downstream). Diagnosed by Matthew Flatt in #1055. | **Fixed** (2026-09-23) — dequeue moved into the loop's else branch (true tail recursion, matching `blocking-queue-pop`'s structure); fixed reproducer runs 20/20 clean |
| `analysis/tokenizer.sls` `consume-sps-auxiliary` | Only stopped at `(`, so a file whose first datum is not parenthesized (a string, symbol, `'`-quote, `#t`, char, `[` ...) was scanned through to EOF and the returned start position landed on the file's **last character** — parsing then started mid-datum (a dangling quote reads to EOF and raises "unexpected end-of-file"), and the resulting `tokenizer-error0` failed the whole workspace initialize (-32001). Seen in the wild on Chez mats build artifacts (`testfile-cp.ss`, a bare 21-byte string literal). | **Fixed** (2026-09-22) — stop at any datum start; `#`-forms keep their special cases (`#\|` block, `#\;` datum comment, `#!` directive line skipped to EOL); only whitespace and comments are consumed. `incomplete.ss.test` now yields 5 datums (the leading `import` symbol is recovered instead of silently swallowed). Defense in depth (per-file try/except in `init-document`) was considered and deliberately dropped to keep the change minimal; if a future tokenizer hard-error resurfaces, revisit then. |
| `analysis/tokenizer.sls` datum loop | Two compounding bugs killed whole-workspace `initialize` (-32001) on any file with content after `#!eof` (e.g. Chez's own `examples/template.ss`, which has a shell script after `#!eof`): (1) the loop's termination check only compared port positions, but the reader consumes `#!eof` and returns the eof-object with the port advanced, so parsing ran past `#!eof` into the trailing garbage and raised; (2) the loop's recursion sits inside its own `try`, so the resulting `tokenizer-error0` was re-caught and re-wrapped once per successfully parsed top-level form, each wrap embedding the whole source — an O(forms × size) condition (multi-MB dumps in the log). | **Fixed** (2026-09-21) — terminate on `eof-object?`; re-raise `tokenizer-error0` without re-wrapping; truncate the embedded source in irritants. Regression fixtures: `tests/resources/tokenizer-exceptions/hash-bang-eof-tail.ss.test`, `many-forms-then-error.ss.test`. The `if` at the tolerant-patch call site (~L703, equal-length vs different-length branches) is intentional, not inverted: the entire `private:tolerant-parse->patch` engine is strictly equal-length replacement (n bad chars -> n spaces, including `replace-token`/`replace-region`; the only length-changing exit is the old-fasl rule returning `""`), so equal length guarantees the bias invariant (positions in the patched source still match the original document) and earns a non-tolerant re-scan, while the different-length branch is effectively the fasl give-up. History: introduced in `1375539` (2024-09-07) as `(raise 'can-not-tolerant)`, softened by `dcd1954`/`8f5fa74`/`bba024f` (patch-internal raises became warnings), semantics preserved throughout because the engine was reworked to always satisfy the invariant. |
| `analysis/workspace.sls:150` | `threaded-map` calls `private-init-references` without exception guard. Sub-thread exceptions leave `optional-finished?` unset, causing `de-optional` to `condition-wait` forever while `workspace-mutex` is held, blocking all subsequent requests. | **Fixed** (2025-05-28) — `try`/`except` added in `threaded-map` lambda; errors written to `document-diagnoses` |
| `scheme-langserver.sls:235` | When the client closes the connection without sending `exit`, `read-message` returns `#f` on EOF. The main loop called `thread-pool-stop!`, but worker threads were blocked in `request-queue-pop`'s `condition-wait` and could never consume the `kill-thread` job. Deadlock caused the process to remain alive after the client disconnected. | **Fixed** (2025-05-26) — `(exit 0)` on EOF instead of waiting for `thread-pool-stop!` |
| `analysis/workspace.sls` | `bf98f11` added `clear-expander-doc-cache!` and `clear-references-for` inside `private-init-references`, which runs in parallel via `threaded-map`. Both mutate global/shared state without synchronization. | **Fixed** (2025-05-26) — moved to serial pre-phase before `threaded-map` (under `workspace-mutex`) |
| `analysis/abstract-interpreter.sls:270` | Global `eq-hashtable` `private:expander-doc-cache-ht` was accessed unsafely from `threaded-map`, causing bucket-list corruption (100% CPU hang or `nonrecoverable invalid memory reference`). Cache removed; `private:find-expander-doc-for-node` now computes directly. | **Fixed** (2025-05-26) |
| `analysis/type/domain-specific-language/interpreter.sls` | `private-with` used `candy:match-right` when `input` contained `**1`/`...`. This fragmented list-valued bindings (e.g. `map`'s higher-order params) into multiple flat pairs that overwrote each other during `fold-left` + `private-substitute`, causing type collapse. | **Fixed** (2025-05-11) — unconditional `candy:match-left` preserves bindings intact |
| `doc/analysis/dependency/file-linkage.md:148` | Matrix shrink on file deletion | **Resolved** — implemented via `shrink-file-linkage!` |
| `analysis/type/substitutions/rnrs-meta-rules.sls:182` | `cons` type rule returns `inner:pair?`, not `inner:list?`. `matrix-from`/`matrix-to` work around this with `cons` + `reverse`. | **Resolved** — workaround in place, no change to `cons` rule needed |
| `analysis/package-manager/file-filter.sls` + `run.ss` | `init-workspace` only accepted `'akku` or `'txt` filters; README claimed `.sld` support but `scheme-file?` did not include it. | **Fixed** (2026-07-08) — new `file-filter` module supports `'akku`, `'txt`, custom extension lists, and predicate procedures; CLI split into `--package-manager/-p` (preset) and `--file-filter/-f` (extension list); `.sld` included in the standard Scheme extension set |

### Withdrawn / retracted issues

| Location | Issue | Status |
|----------|-------|--------|
| `analysis/workspace.sls` | Attempted post-phase undefined-identifier diagnostic (`5545e4c`, reverted in `4a13a70`). `find-available-references-for` returns empty for local bindings (let/lambda/define params) as well as truly undefined symbols. Distinguishing the two requires reliable binding-position tracking across all binding forms (including quoted symbols and library-name components), which proved too fragile in the current AST-walker architecture. | Withdrawn — requires deeper binding-tracking before retry |

---

## 9. Useful One-Liners

```bash
# Run a single test file quickly
source .akku/bin/activate && scheme --script tests/analysis/test-workspace.sps

# Clear all compiled caches for the project
rm -rf .akku/libobj/scheme-langserver

# Find all .sls files under analysis/
find analysis -name "*.sls" | sort

# Check which tests import a specific module
grep -rl "library-import" tests/

# Count test assertions in a file
grep -c "test-equal\|test-assert" tests/analysis/dependency/test-file-linkage.sps

# Run LSP message-level robustness tests
source .akku/bin/activate && scheme --script tests/robustness-lsp-replay.sps

# Log replay — single-threaded (deterministic)
source .akku/bin/activate && scheme --script bin/log-debug.sps

# Log replay — multi-threaded (concurrent, closer to real-world)
source .akku/bin/activate && scheme --script bin/parallel-log-debug.sps

# Clear caches before replaying after any code change
rm -rf .akku/libobj/scheme-langserver

# Compare response counts between single-thread and multi-thread replays
# (different counts often reveal concurrency-related bugs)
grep -c '"id":' ~/scheme-langserver.out
```

### Log Replay Debugging Tips

Place production logs at `~/ready-for-analyse.log`. Both replay scripts reconstruct the LSP JSON-RPC stream and run the server, writing outputs to `~/scheme-langserver.out` (responses) and `~/scheme-langserver.log` (diagnostics).

**Key things to check when responses are missing:**

1. **Client cancellation** — Search the log for `$/cancelRequest` with the same `id`. LSP allows clients to cancel stale requests; the server silently drops them (no response is expected).

2. **I/O errors at EOF** — If the client disconnects without sending `exit`, `send-message` may fail with `Broken pipe`. This produces `error: failed on ...` + `Failed to send error response` pairs in the log. These are normal I/O errors, not logic bugs.

3. **didChange no longer auto-cancels** — As of the LSP-compliance fix, `textDocument/didChange` only enqueues itself; it no longer wipes pending hover/definition/documentSymbol requests. If you see massive response loss in multi-thread replay, suspect stale `.so` caches first.

4. **Response diffing** — `parallel-log-debug.sps` should now produce the same (or more) responses as `log-debug.sps`. If multi-thread returns fewer responses despite the fix, check `~/scheme-langserver.log` for exceptions.

---

## 10. Architecture Cheat Sheet

### Workspace lifecycle
1. `init-virtual-file-system` — scan directory tree, create file-nodes + documents
2. `init-library-node` — build library-node tree from library headers
3. `init-file-linkage` — build dependency adjacency matrix
4. `init-references` — run abstract interpreter (`step`) over all files

### Incremental update flow
1. File changed → `update-file-node-with-tail` (or `attach-new-file`)
2. If library header changed → rebuild **entire** file-linkage + library-node tree
3. If content changed (header same) → `refresh-file-linkage&get-refresh-path`
4. `shrink-paths` produces topological batches
5. `init-references` re-runs `step` on affected batches

### Key record types
| Record | Fields (mutable marked) | Purpose |
|--------|------------------------|---------|
| `file-node` | path, name, parent, folder?, children, document | VFS node |
| `library-node` | identifier, parent, children, file-nodes | Library hierarchy |
| `document` | uri, text, index-node-list, ordered-reference-list, diagnoses | Parsed source |
| `index-node` | datum/annotations, parent, children, excluded-references, import-in-this-node, export-to-other-node | AST node |
| `file-linkage` | path->id-map, id->path-map, matrix | Dependency graph |
| `identifier-reference` | identifier, document, index-node, initialization-index-node, library-identifier, type, parents, type-expressions, **usage-count** (mutable) | Symbol reference |


---

## 11. Workspace Cache Persistence

### Current implementation (`kimi` branch)

A FASL-based workspace cache is now implemented and benchmarked successfully:

| Fixture | Cold startup | Cached startup | Speedup |
|---------|--------------|----------------|---------|
| simple-lib | ~31 ms | ~1.3 ms | ~24x |
| two-libs | ~35 ms | ~1.4 ms | ~24x |
| Synthetic 100-copy simple-lib (200 files) | ~2484 ms | ~49 ms | ~50x |
| scheme-langserver itself (128 `.sls` files) | ~55,790 ms | ~1750 ms | ~32x |

Key design points:

- Uses Chez native `fasl-read` / `fasl-write` (binary ports, compressed).
- Persists the full object graph: `file-node` tree, `library-node` tree,
  `document` text + `index-node-list`, `identifier-reference` network,
  `file-linkage` matrix.
- Skips the heaviest phase: `init-references` (abstract interpreter / type
  inference).
- Manifest includes `format-version`, `langserver-version`, `chez-version`,
  `machine-type`, `record-fingerprint`, `file-filter`, and runtime flags; any mismatch
  falls back to cold start.
- `file-linkage-path->id-map` is an `equal-hashtable`; Chez `fasl-write` only
  supports `eq-hashtable`, so it is converted to/from an alist around save/load.
- Procedure-valued fields that cannot be FASL-serialized are cleared before save:
  - `index-node-expansion-generator` reset to `'()`.
  - `identifier-reference-syntax-expander` reset to `#f`.
- `workspace-undiagnosed-paths` is cleared before save. `document-diagnoses`
  are **persisted** (`workspace.sls` `private:prepare-workspace-payload`) so
  clients can pull diagnostics immediately after a cache load.
- **Trap: stale diagnoses after a rule-code fix.** Diagnoses are persisted
  together with file mtimes recorded *at save time*. If the langserver's own
  rule files (e.g. `analysis/identifier/**`) are fixed *after* a long-running
  server analyzed the workspace, the running server still holds pre-fix
  diagnoses in memory, and a graceful shutdown (LSP `shutdown`/`exit`) saves
  them into the cache. The next server then loads those stale diagnoses and
  the mtime consistency check passes (mtimes match the post-fix disk state),
  so no refresh is triggered. `rm -rf <cache>` before a normal restart does
  NOT help — the old server's shutdown save recreates the cache. To force a
  fresh analysis after fixing rule code: kill the server with `kill -9` (so
  no shutdown save happens), *then* delete the cache, then restart.
- Incremental refresh (Phase 3) is implemented: when only some files differ from
  the cache, only added/deleted/changed files are processed; unchanged files keep
  their cached analysis results. If cache loading fails for any reason (manifest
  mismatch, corrupted file, etc.), the server falls back to a cold start.

CLI usage:

```bash
./run --cache-path ~/.cache/scheme-langserver
```

### Historical: `ufo-persistence` attempt (removed)

We attempted to add workspace cache persistence using `ufo-persistence` so that
`init-workspace` could skip file I/O, parsing, and VFS construction on restart.
The implementation:

- Registered Chez built-ins (`annotation`, `source`, `source-file-descriptor`) and
  scheme-langserver record types with `ufo-persistence`.
- Stripped Chez annotation objects to plain s-expressions before save and
  reconstructed them from cached positions after load.
- Avoided persisting `file-linkage` (dense matrix, equal-hashtable) and rebuilt
  it from the loaded trees.
- Worked around shared-record-reference limitations by serializing
  `library-node-file-nodes` as paths and re-linking them after load.

### Why it was removed

Benchmarks showed **no meaningful speedup**:

| Fixture | Cold startup | Cached startup | Speedup |
|---------|--------------|----------------|---------|
| All real fixtures (~40 files) | ~250 ms | ~248 ms | ~1.01x |
| Synthetic 100-copy simple-lib (200 files) | ~1360 ms | ~1400 ms | ~0.97x |

Because the cache still has to:

1. Deserialize a large record graph from disk.
2. Reconstruct `annotation` objects for every index-node.
3. Rebuild `file-linkage` from scratch.
4. Re-run `init-references` (the abstract interpreter / type inference) over all
   files — the heaviest phase.

The savings from skipping file reads and directory scans were outweighed by
serialization/deserialization and annotation-reconstruction overhead. Achieving
a real startup speedup would require persisting the identifier-reference network
and `file-linkage` in full, which is a larger project and was not justified by
the measured gains.

### Lesson learned

**Serialization alone does not speed up startup** when the dominant cost is the
abstract interpreter and the cache cannot avoid the dominant phase. Before
re-introducing persistence, profile to ensure the saved phase is actually a
significant fraction of startup time, and design the cache to skip that phase
entirely rather than merely replacing file I/O with deserialization.


---

## 12. Identifier Rule Semantics — What `do-process` Teaches Us

A large part of the scheme-langserver codebase is dedicated to a single job:
**modeling Scheme lexical scope as edges between `index-node`s and
`identifier-reference`s**. Every rule under `analysis/identifier/rules/` is
essentially doing the same thing for a different syntactic form: it decides,
for each bound identifier, *where the binding is introduced*, *where it is
visible*, and *where it must be shadowed or excluded*.

When refactoring these rules, keep the scope model in mind rather than just
the surface syntax.

### The three `index-node` fields

| Field | Meaning | Typical use |
|-------|---------|-------------|
| `references-export-to-other-node` | "This node **defines** the identifier." | The variable/parameter symbol in `do`, `lambda`, `let`, etc. |
| `references-import-in-this-node` | "This node and all descendants can **see** these references." | The body of a `lambda`, the `do` form itself, a `let` body, ... |
| `excluded-references` | "These references are **not** visible from this node downward." | The init expression of a `do` variable, the parameter list of a `lambda`, the binding form of a `letrec` |

These three fields are how the abstract interpreter (`find-available-references-for`)
knows where a symbol resolves.

### `do-process` as a worked example

A `do` form such as:

```scheme
(do ((i i (+ i 1))
     (j 10 (- j i)))
    ((> i j) (+ i j))
  (display i))
```

is modeled like this:

1. **Export** each variable from its own symbol node.  
   The node for `i` in `(i i (+ i 1))` gets an `identifier-reference` for `i`.
2. **Import** all `do` variables at the `do` node itself.  
   This makes `i` and `j` visible in the test expression, the body, and every
   step expression, because all of those nodes are descendants of the `do` node.
3. **Exclude** the variables at each init-expression node.  
   The second `i` in `(i i (+ i 1))` is the init expression; it must **not**
   resolve to the `do` variable. Attaching the variable references to the
   `excluded-references` of that init node filters them out during lookup.

The current implementation (`analysis/identifier/rules/do.sls`) expresses this
with `match-index-node` / `ufo-match-steer`:

```scheme
(match-index-node index-node
  [(:_ (((? index-node-symbol? var-index-nodes) . fuzzy) **1) body ... )
    ...])
```

- `var-index-nodes` captures the bound variables.
- `fuzzy` captures the rest of each binding clause (init + step). The init
  nodes are excluded from the imported references.
- `body ...` matches the remaining test/body forms.

### Consequences for refactoring

- **Preserve scope semantics first, syntax second.** Rewriting a rule with
  `match-index-node` is fine, but verify that imports/exports/excludes land on
  the same nodes as before.
- **Use `dereference-index-node` before descending into children.** Shared
  compound literals (`#1=(x y)`) can make an `index-node` a reference to
  another node; rules must dereference it before calling `index-node-children`.
- **Binding forms are all cousins.** `lambda`, `case-lambda`, `let`, `let*`,
  `letrec`, `let-values`, `do`, `define`, `syntax-case`, etc. all use the same
  three-field mechanism. A fix or pattern learned in one rule usually applies
  to several others.
- **Test visibility, not just non-empty imports.** A good regression test
  checks both that a binding is imported where it should be and that it is
  *not* resolved where it should be excluded (e.g. a `do` variable in its own
  init expression).

### Refactoring helper: `match-index-node` follows shared references

`virtual-file-system/index-node.sls` defines `index-node-match-protocol`. Its
children/first-child/rest-children getters transparently follow
`index-node-shared-reference`, so `match-index-node` patterns such as
`(bindings **1)` or `((? index-node-symbol? x) . rest)` can safely descend
into shared compound literals (`#1=...`).

You still need to call `dereference-index-node` explicitly when you are about
to mutate the node (attaching exports/imports/excludes) or when you need the
canonical node's identity.
