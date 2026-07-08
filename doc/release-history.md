# Release History

## 2.1.6
Bug-fix release completing shared/cyclic literal handling in binding forms and enabling workspace cache saves from compiled binaries.

### What's new in 2.1.6
- **Identifier analysis**:
  - All remaining binding-form rules (`let`, `let*`, `letrec`, `let-values`, `do`, `fluid-let`, `let-syntax`, `letrec-syntax`, `with-syntax`, `syntax-case`, `syntax-rules`, `define-syntax`, `define-record-type`, `s7/lambda*`, `s7/define*`) now dereference shared index-nodes before accessing children, preventing crashes or infinite loops on cyclic parameter/binding lists.
  - New regression test `tests/analysis/identifier/rules/test-shared-reference-binding-forms.sps` covers these edge cases.
- **Build**:
  - Release and development builds now use `compile-chez-program --full-chez`, so the compiled `./run` binary retains the `$write-fasl-bytevectors` primitive and can save the workspace FASL cache.
  - `Dockerfile`, `Dockerfile.musl`, and CI workflows updated to install the static libraries (`libuuid`, `libncurses`, `libtinfo`) required for linking against `full-chez.a`.

## 2.1.5
Bug-fix release fixing infinite recursion and memory growth caused by cyclic/shared Scheme literals.

### What's new in 2.1.5
- **AST**:
  - `init-index-node` now detects cyclic and shared reader graph notation (`#n=` / `#n#`) via an internal `compound->node` hashtable.
  - Reference occurrences become leaf index-nodes with a new `shared-reference` field pointing to the defining node, keeping the AST acyclic.
- **Bug fixes**:
  - Fixes the memory leak/hang when analyzing files like swish `src/swish/db.ss`, which contains `#0=(101 . #0#)`.
- **Type inference**:
  - Reference nodes generate a type constraint equating their type with the referenced definition node.

## 2.1.4
Feature and bug-fix release improving rest/dotted parameter handling and type inference.

### What's new in 2.1.4
- **Type inference**:
  - Rest/dotted parameters in `define`, `lambda`, and `case-lambda` now produce function signatures containing `(inner:list? something? ...)`.
- **Identifier analysis**:
  - Rest/dotted parameter bindings in `define`, `case-lambda`, and `do` loop variables now point to the correct per-symbol index-node.
- **AST**:
  - Dotted-pair structure is now preserved in the index-node tree via synthetic annotations, preventing dot-position symbols from being dropped.

## 2.1.3

Feature release adding a Chez FASL workspace cache with incremental refresh.

### What's new in 2.1.3
- **Workspace cache**:
  - New `--cache-path <dir>` CLI option persists the analyzed workspace state to a Chez FASL file (`<cache-path>/workspace.fasl`).
  - On restart, unchanged files skip the expensive `init-references` phase; changed/added/deleted files are refreshed incrementally.
  - Cache manifest includes langserver version, Chez version, machine type, and record-layout fingerprint; any mismatch falls back to a cold start.
  - Non-serializable procedure fields (`index-node-expansion-generator`, `identifier-reference-syntax-expander`) are cleared before save and regenerated on demand.
  - Typical speedups: ~20x on small fixtures, ~30–50x on larger projects.
- **Docs**: Updated `README.md`, `AGENTS.md`, `doc/analysis/workspace.md`, and `doc/build-and-startup.md` with cache usage and implementation details.

## 2.1.2
Bug-fix release restoring bracket-mismatch diagnostics in the fault-tolerant tokenizer.

### What's new in 2.1.2
- **Tokenizer**:
  - Restore clear diagnostics for unmatched parentheses and brackets (e.g. `unclosed parenthesis`, `unexpected close bracket`) during fault-tolerant parsing, while keeping the R7RS/S7 compatibility fixes from 2.1.1.

## 2.1.1
Bug-fix release with R7RS/S7 tokenizer compatibility, identifier analysis fixes, and improved diagnostics.

### What's new in 2.1.1
- **Bug fixes**:
  - Fix `typed-lambda` / `lambda` dotted formals crash when encountering typed pair parameters like `(ht hash-table?)`.
  - Fix `identifier-compare?` to guard `symbol->string` with `symbol?` checks.
  - Fix `rename` / `alias` import unused-import false positives.
  - Skip reference initialization for files with empty `index-node-list`.
- **Tokenizer**:
  - R7RS compatibility: `#u8(...)` → `#vu8(...)`, `#\null` → `#\nul`, `#\escape` → `#\esc`, `#;` datum comments.
  - S7 compatibility: `#<eof>`, `#<undefined>`, `#<fails:...>`, `#<predicate?>`, `#_id`, `#"..."` raw strings.
- **Diagnostics**: Analysis errors now use `display-condition` for clearer compound-condition output.
- **Docs**: Added `doc/top-environment.md` documenting the `top-environment` mechanism.

## 2.1.0
Major release with expanded diagnostics, macro auto-resolution, performance optimizations, and Docker CI upgraded to Chez 10.4.1.

### What's new in 2.1.0
- **LSP protocol**: `workspace/symbol` search is now supported.
- **Diagnostics**:
  - Diagnostics now include standard LSP `source` and `code` fields.
  - **Duplicate identifier detection** in binding forms (`lambda`, `case-lambda`, `let`, `letrec`, `let-values`, `do`, `define`, `with-syntax`).
  - **Unused import detection** for `only`, `except`, `rename`, and `alias` modifiers.
  - Tokenizer syntax errors are now surfaced as document diagnostics.
  - Silent type-inference and type-rule failures are diagnosed.
  - Empty diagnostic arrays are sent to clear stale client-side errors.
- **Macro auto-resolution**: extended from `syntax-rules` to `syntax-case`, `let-syntax`, and `letrec-syntax`. Multi-layer macro cascade reference propagation is fixed.
- **Type inference**: `define-record-type` now infers record types; `car`/`cdr` family macros (`caar`, `cadr`, `caddr`, `cadddr`, `caaar`, `cadar`, etc.) have dedicated type rules.
- **Performance**: OPT-1~5 optimizations (expander-doc caching, hashtable reverse maps, tail-recursive accumulators, incremental all-pairs maintenance); MEM-1/3/6 memory optimizations for auto macro expansion; dedupe and reference hot-paths switched to `eq?` hashtables; matrix operations rewritten with `cons`+`reverse`.
- **Robustness**: hardened LSP message parsing against EOF, invalid `Content-Length`, and malformed JSON; `shutdown`/`exit` lifecycle fully compliant with the LSP spec; `didChange` auto-cancellation removed to comply with the spec; request-queue concurrency hardened with cancel barriers and log-mutex.
- **Infrastructure**: Docker build chain upgraded from Chez 9.6.4 to 10.4.1; `chez-exe` switched to the `ufo5260987423/chez-exe` fork for 10.x compatibility; test suite refactored to use AST search instead of hard-coded positions.

## 2.0.3
Fixed pretty-print bugs that were mixed with standard I/O.

## 2.0.2
Publish diagnoses, though now only can figure out "fail to find library".

## 2.0.1
Fix many bugs.

## 2.0.0
Fix many bugs and switch between different top environments.

## 1.2.9
Now, enjoy type inference!

## 1.2.8
Now hover and auto completion is ready for use. I also have done many things about parsing fault tolerance.

## 1.2.7
Fix bugs on uri parsing, do you know LSP request uri may wrongly process escape characters?

## 1.2.6
Fault tolerant parser.

## 1.2.5
Fix: Some protocol api bugs. And now it's basically smooth with Magic Scheme and Vscode.

## 1.2.4
Fix: hover api. It failed when processing meta.

## 1.2.3
Why completion api doesn't work well? I don't know and just fix.

## 1.2.2
I just fixed some bugs processing my own other projects.

## 1.2.1
I just fixed some bugs processing SS/SCM codes.

## 1.2.0
Re-construct the identifier catching mechanism with abstract interpreter.

## 1.1.1
Scheme-langserver now releases type information used in corresponding libraries! Its soundness is still not guaranteed!

## 1.1.0
Type inference has been embedded into autocompletion! And it uses a homemade DSL.

## 1.0.13
Fix bug: sometimes can't shutdown server. Optimization: re-construct document-sync mechanism.

## 1.0.12
Add ss/scm-import-rnrs option.

## 1.0.11
Gradual Typing system, all basic rules have been passed.

## 1.0.10
Fix bugs in 1.0.9.

## 1.0.9
Abandoned: add parallel and synchronize mechanism.

## 1.0.8
Build index as document synchronizing instead of workspace initializing.

## 1.0.7
Catch syntax-* identifier bindings.
