![](./doc/figure/logo-no-background.png)
# Scheme-langserver

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/ufo5260987423/scheme-langserver)

You may read my [paper](./doc/paper.pdf) and cite like this:

> WANG, Z. (2025, May 12). Scheme-langserver: Treat Scheme Code Editing as the First-Class Concern. The 18th European Lisp Symposium (ELS`25), Zurich. https://doi.org/10.5281/zenodo.15384882

Due to occasional GitHub access restrictions from China, this repository is also mirrored on [Codeberg](https://codeberg.org/ufo5260987423/scheme-langserver) and [Gitee](https://gitee.com/ufo5260987423/scheme-langserver). I collaborate with [XmacsLabs](https://github.com/XmacsLabs); a fork is available [here](https://github.com/XmacsLabs/scheme-langserver).

<video src="https://github.com/user-attachments/assets/893bba98-6709-4fac-a4d3-dc7b6aab46fb" controls="controls" width="500" height="300"></video>

**VSCode is now supported!** See the [setup guide](./doc/startup.md).

> **Note:** Auto-generated type information is available [here](https://ufo5260987423.github.io/scheme-langserver/doc/analysis/type-inference-result). It is mainly used for downstream development and debugging.

Implementing IDE features like autocomplete, goto definition, and hover documentation is a significant effort. Compared to languages like Java, Python, JavaScript, or C, language server implementations for Lisp dialects are still scarce. Existing tools such as [Geiser](https://gitlab.com/emacs-geiser), [racket langserver](https://github.com/jeapostrophe/racket-langserver), and [swish-lint](https://github.com/becls/swish-lint) rely primarily on a REPL or keyword tokenization rather than static program analysis.

For example, when editing an incomplete project whose code is not yet fully executable, Geiser can only complete top-level bindings listed by `environment-symbols` (on Chez Scheme) or raw symbols—not true identifiers. This means local bindings and unfinished code receive no help in recognizing valid identifier scopes. The same limitation applies to goto definition and other core IDE features.

The root cause is that Scheme and other Lisp dialects present a formidable challenge for program analysis: their rich data structures, flexible control flow, and especially macros make static reasoning difficult. But this does not mean Scheme is only for geniuses and meta-programming. With a better editing environment, Scheme can be accessible and productive for everyone.

**scheme-langserver** is a Language Server Protocol (LSP) implementation for Scheme that provides completion, goto definition, hover, and type inference through static code analysis based on the [R6RS standard](http://www.r6rs.org/). It handles incomplete code gracefully and is published via [Akku](https://akkuscm.org/), a Scheme package manager.

The server has been tested on [Chez Scheme](https://cisco.github.io/ChezScheme/) 9.4, 9.5, and 10.x.

## Compilation, Installation & Configuration
See the [setup guide](./doc/startup.md).

## Debugging
For troubleshooting tips, see [debugging.md](./doc/debugging.md).

## Recent Status
Active development is focused on bug fixes, performance profiling, and expanding the type inference system. The 2.1.1 release fixes several crashes and diagnostics issues, and adds R7RS/S7 tokenizer compatibility. The 2.1.0 release brings major improvements to diagnostics, macro auto-resolution, and LSP protocol robustness. Planned features include a dedicated [VSCode](https://code.visualstudio.com/) plugin and data-flow analysis.

## Release
2.1.1 — Bug-fix release with R7RS/S7 tokenizer compatibility, identifier analysis fixes, and improved diagnostics.

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

2.1.0 — Major release with expanded diagnostics, macro auto-resolution, performance optimizations, and Docker CI upgraded to Chez 10.4.1.

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

### Previous releases
See [doc/release-history.md](doc/release-history.md) for the full changelog.

### Features
1. Completion for top-level and local identifier bindings.
![Top-level and local identifiers binding](./doc/figure/auto-completion.png "Top-level and local identifiers binding")
2. Goto definition.
![Goto definition with telescope.nvim](./doc/figure/definition.png "Goto Definition with telescope.nvim")
3. Compatible with package manager: Akku.
4. File-change synchronization with corresponding index updates.
5. Hover.
6. References and document highlights (document-scoped).
![Find references with telescope.nvim](./doc/figure/find-references.png "Find references with telescope.nvim")
7. Document symbol.
![Find document symbols with telescope.nvim](./doc/figure/document-symbol.png "find document symbols with telescope.nvim")
8. **Workspace symbol search** (`workspace/symbol`).
9. Catching local identifier bindings in `define-syntax`, `let-syntax`, and other macro forms via hand-written rules.
10. **Automatic macro resolution** (experimental). The generic expander for `syntax-rules`, `syntax-case`, `let-syntax`, and `letrec-syntax`—plus multi-layer macro cascade propagation—is functionally correct but **not enabled in production** because it is too slow for real-world projects (it triggers heavy macro expansion and cross-document reference back-propagation for every macro use site). The routing code in `analysis/identifier/self-defined-rules/router.sls` currently falls back to hand-written rules such as `match-process` for `ufo-match`. If you are interested in pushing this research forward—e.g. via lazy expansion, incremental caching, or selective rule generation—contributions and discussions are very welcome!
11. Cross-platform parallel indexing.
12. Custom source-code annotator compatible with `.sps` files.
13. Peephole optimization for API requests using suspendable tasks.
14. Type inference via a homemade DSL interpreter, now integrated into auto-completion. Parameters whose types match the expected signature are ranked higher, as shown below where `length-a` and `length-b` (both `integer?`) appear first because they match the parameter type required by `<=`.
![Autocompletion with type inference](./doc/figure/auto-completion-with-type-inference.png "Autocompletion with type inference")
15. Supports R6RS, R7RS, and [S7](https://ccrma.stanford.edu/software/snd/snd/s7.html) by switching top environments.

```bash
send-message
2023 11 21 11 26 41 967266866
{"jsonrpc":"2.0","id":"3","result":[{"label":"length-a"},{"label":"length-b"},{"label":"lambda"},{"label":"latin-1-codec"},{"label":"lcm"},{"label":"least-fixnum"},{"label":"length"},{"label":"let"},{"label":"let*"},{"label":"let*-values"},{"label":"let-syntax"},{"label":"let-values"},{"label":"letrec"},{"label":"letrec*"},{"label":"letrec-syntax"},{"label":"lexical-violation?"},{"label":"list"},{"label":"list->string"},{"label":"list->vector"},{"label":"list-ref"},{"label":"list-sort"},{"label":"list-tail"},{"label":"list?"},{"label":"log"},{"label":"lookahead-char"},{"label":"lookahead-u8"}]}
```
16. Abstract interpreter that resolves identifiers across multiple file extensions: `.scm`, `.ss`, `.sps`, `.sls`, `.sld`.
17. Code diagnostics with LSP-standard `source` and `code` fields. Detects library-not-found, duplicate identifiers in binding forms (e.g. `(lambda (x x) ...)`), unused imports (e.g. `(only (rnrs) car)` where `car` is never referenced), and tokenizer syntax errors.
![Fail to find library](./doc/figure/diagnose-failt-to-find-library.png "Fail to find library")

### Roadmap
18. Renaming support (`textDocument/rename` + `prepareRename`).
19. Formatting (`textDocument/formatting`).
20. Signature help (`textDocument/signatureHelp`).
21. Code actions (`textDocument/codeAction`) — e.g. "Remove unused import", "Organize imports".
22. Full R6RS compatibility.
23. Step-by-step macro expander for self-defined macros.
24. Code evaluation within the language server.
25. Cross-language semantic support via AST transformers.
26. Extract expression/statement into a procedure (refactoring).

## Contributing
Pull requests are welcome! Please see [AGENTS.md](./AGENTS.md) for project conventions, build steps, and coding style before opening a PR.

### Vibe Coding with KIMI
Since mid-2025, active development on this project has been assisted by [KIMI](https://kimi.moonshot.cn/) (Moonshot AI) in a vibe-coding workflow: the maintainer describes intent in natural language, KIMI explores the codebase, proposes changes, and iterates with tests. If you notice commits authored or co-authored by `kimi`, that is the AI agent trail. Human review and final approval always remain with the maintainer.

## Testing
Almost all key procedures and APIs are covered by tests. Run the full suite with:
```bash
bash test.sh
```
For faster feedback during development, run a single test file:
```bash
source .akku/bin/activate
scheme --script tests/protocol/apis/test-definition.sps
```
> **Note:** Tests currently focus on single-threaded execution.

## Other Use Cases
### Script-Fu in GIMP
Script-Fu is based on Scheme. Using [this example](https://dalelane.co.uk/blog/?p=628), you can apply scheme-langserver to `.scm` files in GIMP.

### Other Potential Targets

Possible future targets include [OMN (Opusmodus Notation)](https://opusmodus.com/) and AutoLisp.

## Code Count
```bash
find . -name "*.sls" ! -path "./.akku/*" |xargs wc -l
```
## Detailed Document

### Core Analysis
1. [Catching identifier bindings](./doc/analysis/identifier.md) — how the abstract interpreter resolves `define`, `lambda`, `let`, `define-record-type`, etc.
2. [Macro auto-resolution](./doc/analysis/macro-auto-resolve.md) — generic `syntax-rules` expansion vs hand-written rules
3. [Type system & inference](./doc/analysis/type.md) — complete type-inference pipeline and DSL
4. [Workspace lifecycle](./doc/analysis/workspace.md) — initialization, incremental updates, and refresh batches
5. [File dependency graph](./doc/analysis/file-linkage.md) — topological sorting and linkage matrix

### Protocol & Concurrency
6. [API request scheduling](./doc/protocol/analysis.md) — request queue, engine time-slicing, cancellation, and document-sync protection
7. [Diagnostic publication](./doc/publish-diagnoses.md) — how diagnostics are generated, accumulated, and sent

### Debugging & Development
8. [Debugging guide](./doc/debugging.md) — enable logs, replay logs, and iterative printf debugging
9. [Development guide (中文)](./doc/development-guide.md) / [English version](./doc/development-guide-en.md)
10. [AGENTS.md](./AGENTS.md) — build steps, testing conventions, coding style, and common traps for contributors

### Research & Experiments
11. [Scheme-langserver paper (ELS'25)](./doc/paper.pdf) — academic paper on static analysis for Scheme
12. [Macro resolution notes](./doc/macro-resolution-notes.md) — debugging notes for macro identifier capture
13. [Syntax candy DSL](./doc/analysis/syntax-candy.md) — pattern matcher for type-rule authoring
14. [Record type inference analysis](./doc/record-type-inference-analysis.md) — `define-record-type` in the type system
15. [Type inference benchmark](./doc/analysis/type-inference-benchmark.md) — performance measurement methodology
16. [DeepWiki](https://deepwiki.com/ufo5260987423/scheme-langserver)

## License
[MIT](./LICENSE)

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=ufo5260987423/scheme-langserver&type=Date)](https://star-history.com/#ufo5260987423/scheme-langserver)

## Contributors

![Contributors](https://contrib.rocks/image?repo=ufo5260987423/scheme-langserver)