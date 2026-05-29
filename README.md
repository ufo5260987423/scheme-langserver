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

The server has been tested on [Chez Scheme](https://cisco.github.io/ChezScheme/) 9.4 and 9.5.

## Compilation, Installation & Configuration
See the [setup guide](./doc/startup.md).

## Debugging
For troubleshooting tips, see [debugging.md](./doc/debugging.md).

## Recent Status
Active development is focused on bug fixes, performance profiling, and expanding the type inference system. Planned features include a dedicated [VSCode](https://code.visualstudio.com/) plugin and data-flow analysis.

## Release
2.0.3 Fixed pretty-print bugs that were mixed with standard I/O.

### Previous releases
- 2.0.2 Publish diagnoses, though now only can figure out "fail to find library".
- 2.0.1 Fix many bugs.
- 2.0.0 Fix many bugs and switch between different top environments.
- 1.2.9 Now, enjoy type inference!
- 1.2.8 Now hover and auto completion is ready for use. I also have done many things about parsing fault tolerance.
- 1.2.7 Fix bugs on uri parsing, do you know LSP request uri may wrongly process escape characters?
- 1.2.6 Fault tolerant parser
- 1.2.5 Fix: Some protocol api bugs. And now it's basically smooth with Magic Scheme and Vscode.
- 1.2.4 Fix: hover api. It failed when processing meta.
- 1.2.3 Why completion api doesn't work well? I don't know and just fix.
- 1.2.2 I just fixed some bugs processing my own other projects.
- 1.2.1 I just fixed some bugs processing SS/SCM codes.
- 1.2.0 Re-construct the identifier catching mechanism with abstract interpreter.
- 1.1.1 Scheme-langserver now releases type information used in corresponding libraries! Its soundness is still not guaranteed!
- 1.1.0 Type inference has been embedded into autocompletion! And it uses a homemade DSL.
- 1.0.13 Fix bug: sometimes can't shutdown server. Optimization: re-construct document-sync mechanism.
- 1.0.12 Add ss/scm-import-rnrs option.
- 1.0.11 Gradual Typing system, all basic rules have been passed.
- 1.0.10 Fix bugs in 1.0.9.
- 1.0.9 Abandoned: add parallel and synchronize mechanism.
- 1.0.8 Build index as document synchronizing instead of workspace initializing.
- 1.0.7 Catch syntax-* identifier bindings.

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
8. Catching local identifier bindings in `define-syntax`, `let-syntax`, and other macro forms via hand-written rules.
9. **Automatic macro resolution** (experimental) for `syntax-rules` / `syntax-case` macros via generic expansion and `shallow-copy` reference back-propagation. The mechanism is functionally correct but too slow for production use on real-world projects (it triggers heavy macro expansion and cross-document reference back-propagation for every macro use site). For this reason it is **not enabled by default**; `(ufo-match)` currently falls back to a hand-written `match-process` rule instead. If you are interested in pushing this research forward—e.g. via lazy expansion, incremental caching, or selective rule generation—contributions and discussions are very welcome!
10. Cross-platform parallel indexing.
11. Custom source-code annotator compatible with `.sps` files.
12. Peephole optimization for API requests using suspendable tasks.
13. Type inference via a homemade DSL interpreter, now integrated into auto-completion. Parameters whose types match the expected signature are ranked higher, as shown below where `length-a` and `length-b` (both `integer?`) appear first because they match the parameter type required by `<=`.
![Autocompletion with type inference](./doc/figure/auto-completion-with-type-inference.png "Autocompletion with type inference")
14. Supports R6RS, R7RS, and [S7](https://ccrma.stanford.edu/software/snd/snd/s7.html) by switching top environments.

```bash
send-message
2023 11 21 11 26 41 967266866
{"jsonrpc":"2.0","id":"3","result":[{"label":"length-a"},{"label":"length-b"},{"label":"lambda"},{"label":"latin-1-codec"},{"label":"lcm"},{"label":"least-fixnum"},{"label":"length"},{"label":"let"},{"label":"let*"},{"label":"let*-values"},{"label":"let-syntax"},{"label":"let-values"},{"label":"letrec"},{"label":"letrec*"},{"label":"letrec-syntax"},{"label":"lexical-violation?"},{"label":"list"},{"label":"list->string"},{"label":"list->vector"},{"label":"list-ref"},{"label":"list-sort"},{"label":"list-tail"},{"label":"list?"},{"label":"log"},{"label":"lookahead-char"},{"label":"lookahead-u8"}]}
```
15. Abstract interpreter that resolves identifiers across multiple file extensions: `.scm`, `.ss`, `.sps`, `.sls`, `.sld`.
16. Code diagnostics. Currently supports detecting library-not-found errors.
![Fail to find library](./doc/figure/diagnose-failt-to-find-library.png "Fail to find library")

### Roadmap
17. Renaming support.
18. Full R6RS compatibility.
19. Step-by-step macro expander for self-defined macros.
20. Code evaluation within the language server.
21. Cross-language semantic support via AST transformers.
22. Extract expression/statement into a procedure (refactoring).

## Contributing
Pull requests are welcome! Please see [AGENTS.md](./AGENTS.md) for project conventions, build steps, and coding style before opening a PR.

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