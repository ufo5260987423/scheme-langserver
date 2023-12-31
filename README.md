![](./doc/figure/logo-no-background.png)
# Scheme-langserver
>NOTE: This language server is mainly for scheme code with .scm, .ss, .sls and .sld extensions. And packages should be imported with Akku for now. There may notice something about ".akku/list" file, please issue me, and I'll gradually fix them.

Implementing support like autocomplete, goto definition, or documentation on hover is a significant effort for programming. However, comparing to other language like java, python, javascript and c, language server protocol implementation for lisp language are just made in a vacuum. [Geiser](https://gitlab.com/emacs-geiser), [racket langserver](https://github.com/jeapostrophe/racket-langserver) and [swish-lint](https://github.com/becls/swish-lint) etc., their works are all based on `repl`(Read-Eval-Print Loop) or keyword tokenizer instead of programming. For example, if a programmer was coding on an unaccomplished project, in which the codes were not fully runnable, [Geiser](https://gitlab.com/emacs-geiser) or any others would only complete top-level binding identifiers listed by `environment-symbols` procedure (for [Chez](https://cisco.github.io/ChezScheme/)). Which means for local bindings and unaccomplished codes, though making effort for programming is supposed of the importance mostly, [Geiser](https://gitlab.com/emacs-geiser) and its counterparts help nothing. Familiar cases occur with goto definition and many other functionalities.

A primary cause is, for scheme and other lisp dialects, their abundant data sets and flexible control structures raise program analysis a big challenge. Especially the dynamic type system and macro, it seems like that scheme is mainly used for genius and meta/macro programming. But I say, no! Scheme can do many interesting things if a better programming environment was provided. And now I'll work on this.

This package is a language server protocol implementation helping scheme programming. It provides completion, definition and type inference. These functionalities are established on static code analysis with [r6rs standard](http://www.r6rs.org/) and some obvious rules for unaccomplished codes. This package itself and related libraries are published or going to be published with [Akku](https://akkuscm.org/), which is a package manager for Scheme. 

This package also has been tested with [Chez Scheme](https://cisco.github.io/ChezScheme/) versions 9.4 and 9.5. 

I do this open source work just in my spare time and I can contribute many splendid ideas to the community like embedding data flow analysis into scheme-langserver or many other things. And I'm continuously asking for much more donation or funding. You can click [this patron page](https://www.patreon.com/PoorProgrammer/membership) or [爱发电](https://afdian.net/a/ufo5260987423) to donate monthly, or just donate 10 USD just once time with the following paypal link. 

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/paypalme/ufo5260987423/10)

## Recent Status
I'll keep fixing bugs, profiling the code, and collecting information for my giant book(in Chinese, but if foreigners are interested in it, an English version will be considered.) on this homemade DSL and background mechanism. This will take me about 1 or 2 years. Further developments including a [VScode](https://code.visualstudio.com/) plugin and data flow analysis. But actually, I'm now setting this open source work a part-time job, and I can not guarantee a schedule.

I'm now visiting [Coimbra University](https://www.google.com.hk/maps/place/University+of+Coimbra/@40.2151996,-8.4224772,13z/data=!4m6!3m5!1s0xd22f909b72d402f:0x2c4969e6ec176a72!8m2!3d40.2076394!4d-8.4260932!16zL20vMDM1NjV5?entry=ttu), would anyone want to visit me?(Please make me an appointment)

### Release 
1.1.0: Type inference has been embedded into autocompletion! And it uses a homemade DSL(Domain Specific Language) making type representation and interpreting much easier.  But, I actually do not recommend anyone use this type inference in production because there are many efficiency and soundness problems which I haven't solved. A detailed outline should be referred in [documentation](#detailed-document).

More details refer to [this file](./doc/release-log.md).
## Setup
### Building
#### Pre-require
1. [Chez Scheme](https://cisco.github.io/ChezScheme/);
>NOTE
If you wanted to enable scheme-langserver's muti-thread feature, it would require [Chez Scheme](https://cisco.github.io/ChezScheme/) to have been compiled with the --threads option. I've test multi-thread feature for Linux, and according to [this page](https://github.com/cisco/ChezScheme/blob/main/BUILDING), it will also excutable for Windows.
2. [Akku](https://akkuscm.org/)；
3. [chez-exe](https://github.com/gwatt/chez-exe)；
>NOTE
[chez-exe](https://github.com/gwatt/chez-exe) requires boot files and kernel files of [Chez Scheme](https://cisco.github.io/ChezScheme/). So, the compile command maybe like follows:`scheme --script gen-config.ss --bootpath /path-to-ChezScheme/{machine-type}/boot/{machine-type}`

#### For Linux
```bash
git clone https://github.com/ufo5260987423/scheme-langserver
cd scheme-langserver
akku install
bash .akku/env
compile-chez-program run.ss
./run path-to-logfile
```
#### TODO: for Windows
The `run` file is also executable for windows WSL environment, and I'm waiting for [Chez Scheme 10](https://github.com/cisco/ChezScheme/wiki/Announcements). As their announcement, they will merge the racket implemented [Chez Scheme](https://cisco.github.io/ChezScheme/) into main branch. And in [this page](https://github.com/racket/ChezScheme/blob/master/BUILDING) it seems that the racket implementation has solved the multi-thread problem.

Further building is going to be done after Chez scheme 10 released. Because, there're many issues about building scheme-langsever on Windows: first, for Chez scheme, its library loading mechanism is *nix-liked, which means for [srfi](https://srfi.schemers.org/), whose library path is like `srfi/:1/lists.sls`, can't be handled on Windows7; Second, many instruments are based on *nix, like [Akku](https://akkuscm.org/), it depends on [Guile](https://www.gnu.org/software/guile/), which only has linux-based releases. Third, Chez now doesn't have portable bytecode (pb) mode, which is mainly useful for bootstrapping a build on any platform.

### Installation for [LunarVim(1.3)](https://www.lunarvim.org/)
I have pull request to [mason.nvim](https://github.com/williamboman/mason.nvim) and [mason-lspconfig.nvim](https://github.com/williamboman/mason-lspconfig.nvim). In that case, you can get this implementation automatically with [LunarVim](https://www.lunarvim.org/). 

But now, above configuration haven't been tested. So, manual installation is still needed: for installed plugin [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig/), after manually building from above step [Building](#building), an executable file `run` would be available at `{path-to-run}`. Then, create file `~/.local/share/lunarvim/site/pack/lazy/opt/nvim-lspconfig/lua/lspconfig/server_configurations/scheme_langserver.lua` as follows:
```lua
local util = require 'lspconfig.util'
local bin_name = '{path-to-run}'
local cmd = { bin_name }

return {
  default_config = {
    cmd = cmd,
    filetypes = { 'scheme' },
    root_dir = util.find_git_ancestor,
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/ufo5260987423/scheme-langserver
`scheme-langserver`, a language server protocol implementation for scheme
]]   ,
  },
}
```

Then configure your `~/.config/lvim/config.lua` and add following codes like:
```lua
vim.cmd [[ au BufRead,BufNewFile *.sld set filetype=scheme ]]
vim.cmd [[ au BufRead,BufNewFile *.sls set filetype=scheme ]]

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
  end,
})

-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
local lspconfig = require('lspconfig')
lspconfig.scheme_langserver.setup {}
lvim.builtin.cmp.enabled()
lvim.builtin.cmp.sources = {
  { name = 'nvim_lsp' },
  { name = 'luasnip' },
  { name = 'buffer' },
}
```
>NOTE: detailed configuration for `lvim.builtin.cmp.sources` and `LspAttach` can refer [this page](https://github.com/LunarVim/LunarVim/blob/b1c72541549d042487510ad3e676de7af046d410/lua/lvim/core/cmp.lua#L133) and [this page](https://github.com/neovim/nvim-lspconfig).

### Enable multi-thread, ss/scm-import-rnrs or type-inference
Scheme-langserver has facilitated many higher level functions, but they shouldn't be fully convinced and tested. If you want to have a try, just step [above instructions](#installation-for-lunarvim) and rewrite file `~/.local/share/lunarvim/site/pack/lazy/opt/nvim-lspconfig/lua/lspconfig/server_configurations/scheme_langserver.lua` as follows:

```lua
local util = require 'lspconfig.util'
local bin_name = '{path-to-run}'

--the first 'enable' is for multi-thread mechanism. 
local cmd = { bin_name ,'{path-to-log}','enable'}
--the second 'enable' is for for extensions SS, and SCM. Most programmers suppose their codes are writing for a running environment and don't provide any library information. However, some issues request for adding basic default context, so advanced language features could be used.
-- local cmd = { bin_name ,'{path-to-log}','disable', 'enable'}
-- the third 'enable' is for type inference.
-- local cmd = { bin_name ,'{path-to-log}','disable', 'disable' ,'enable'}

return {
  default_config = {
    cmd = cmd,
    filetypes = { 'scheme' },
    root_dir = util.find_git_ancestor,
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/ufo5260987423/scheme-langserver
`scheme-langserver`, a language server protocol implementation for scheme
]]   ,
  },
}
```
>NOTE: the type inference is in its very early stage. Even for small code file it may take much more time than your expectation.

### TODO: Installation for [VScode](https://code.visualstudio.com/)

### Features
1. Top-level and local identifiers binding completion.
![Top-level and local identifiers binding](./doc/figure/auto-completion.png "Top-level and local identifiers binding")
2. Goto definition.
![Goto definition with telescope.nvim](./doc/figure/definition.png "Goto Definition with telescope.nvim")
3. Compatible with package manager: Akku.
4. File changes synchronizing and corresponding index changing.
5. Hover.
6. References and document highlight (document-scoped references).
![Find references with telescope.nvim](./doc/figure/find-references.png "Find references with telescope.nvim")
7. Document symbol.
![Find document symbols with telescope.nvim](./doc/figure/document-symbol.png "find document symbols with telescope.nvim")
8. Catching *-syntax(define-syntax, let-syntax, etc.) based local identifier binding. 
9. Cross-platform parallel indexing.
10. Self-made source code annotator to be compatible with .sps files.
11. Peephole optimization for API requests.
12. Type inference with a homemade DSL interpreter(I'm very proud of it!). And now it has been embedded into the auto-completion. As the following figure indicated, the "length-b" and "length-a" having "integer?" type are in the front of those recommended options because they can match the parameter type requiring from "<=". 
![Autocompletion with type inference](./doc/figure/auto-completion-with-type-inference.png "Autocompletion with type inference")
A test in can prove this result, just run `scheme --script tests/protocol/apis/test-completion.sps` and the log file `scheme-langserver.log` would contain results like this:
```bash
send-message
2023 11 21 11 26 41 967266866
{"jsonrpc":"2.0","id":"3","result":[{"label":"length-a"},{"label":"length-b"},{"label":"lambda"},{"label":"latin-1-codec"},{"label":"lcm"},{"label":"least-fixnum"},{"label":"length"},{"label":"let"},{"label":"let*"},{"label":"let*-values"},{"label":"let-syntax"},{"label":"let-values"},{"label":"letrec"},{"label":"letrec*"},{"label":"letrec-syntax"},{"label":"lexical-violation?"},{"label":"list"},{"label":"list->string"},{"label":"list->vector"},{"label":"list-ref"},{"label":"list-sort"},{"label":"list-tail"},{"label":"list?"},{"label":"log"},{"label":"lookahead-char"},{"label":"lookahead-u8"}]}
```
13.  Virtual identifier catching machine for .sps, .ss, .scm files.

### TODOs
14. Renaming. 
15. Fully compatible with [r6rs standard](http://www.r6rs.org/).
16. Macro expanding.
17. Code eval.
18. Code diagnostic.
19. Add cross-language semantic supporting. Well, would java, c, python and many other languages can be supported with an AST transformer?

## TODO:Contributing 

## Debug

### How to Debug
https://www.scheme.com/debug/debug.html#g1

### Output Log
Following tips from [Building](#building), [Installation for Lunar Vim](#installation-for-lunarvim) and [Installation for VScode](#todo-installation-for-vscode), if anyone wants to do some developing and log something, it will be convenient to add `path-to-log-file` and re-write file `~/.local/share/lunarvim/site/pack/packer/start/nvim-lspconfig/lua/lspconfig/server_configurations/scheme_langserver.lua` as follows:
```lua
local util = require 'lspconfig.util'
local bin_name = '{path-to-run}'
local cmd = { bin_name ,"path-to-log-file"}

return {
  default_config = {
    cmd = cmd,
    filetypes = { 'scheme' },
    root_dir = util.find_git_ancestor,
    single_file_support = true,
  },
  docs = {
    description = [[
https://github.com/ufo5260987423/scheme-langserver
`scheme-langserver`, a language server protocol implementation for scheme
]]   ,
  },
}
```
### Recurring with Log 
With above [output log](#output-log), you may use `tests/log-debug.sps` recurring bugs:
1. Rename `{path-to-log}`(usually `~/scheme-langserver.log`) as `~/ready-for-analyse.log`;
2. run `scheme --script tests/log-debug.sps`. If you want to re-produce the multi-thread environment, it would also be available to run `scheme --script tests/log-debug.sps`.

## Test
Almost all key procedures and APIs are tested. My work is just so rough but useful, maybe you would like to find what I've done in `tests` directory or just run following command in `{scheme-langserver-root-directory}`
``` bash
bash test.sh
```
>NOTE
It's hard to do test with threaded environment. So, current tests focus on single thread.

## Code Count
```bash
find . -name "*.sls" ! -path "./.akku/*" |xargs wc -l
```
## Detailed Document
1. [Catching identifier bindings](./doc/analysis/identifier.md)
2. [Synchronizing](./doc/util/synchronize.md)
3. [Type inference](./doc/analysis/type-inference.md),~~[类型推断](./doc/analysis/type-inference.cn.md)~~(Deprecated, and I'm writing a Chinese book for it)
4. [API Analysis](./doc/protocol/analysis.md)

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=ufo5260987423/scheme-langserver&type=Date)](https://star-history.com/#ufo5260987423/scheme-langserver)

## Contributors

![Contributors](https://contrib.rocks/image?repo=ufo5260987423/scheme-langserver)