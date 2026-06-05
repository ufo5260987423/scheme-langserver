# Windows CI 构建任务书

## 背景

`scheme-langserver` 目前只通过 GitHub Actions 发布 Linux (glibc) 可执行文件。用户希望在 Windows 上也能获得原生 `.exe` 可执行文件。

## 核心方案（基于 `prepare-windows-build.sh`）

`prepare-windows-build.sh` 的设计非常巧妙：

- `compile-chez-program` 生成的 `run.generated.c` 只包含 Chez Scheme 虚拟机字节码，是**平台无关**的。
- 因此，**在 Linux 上生成 `.c` 文件，在 Windows 上只负责链接**，即可得到原生 Windows 可执行文件。

这个方案**完全绕开**了 Windows 构建的两个最大障碍：
1. **Akku 没有 Windows 原生版本** —— Akku 只在 Linux 上运行，安装好依赖即可。
2. **SRFI 库文件名中的冒号问题** —— Windows 不允许文件名包含冒号（如 `srfi/:37/args-fold`），但在 Linux 上编译 `.c` 文件时已经不涉及这些源文件。

## 分阶段 Workflow 设计

### 阶段 1：Linux Runner —— 生成平台无关的 C 文件

在 `ubuntu-latest` 上：
1. `actions/checkout@v4` 检出代码（需带完整 git 历史，用于生成版本号）
2. 安装 Akku（Linux amd64 预构建包）
3. `akku install` 安装所有 Scheme 依赖
4. `source .akku/bin/activate && compile-chez-program run.ss` 生成 `run.generated.c`
5. `git describe --tags --always --dirty > .version` 生成版本文件
6. 将 `run.generated.c` 和 `.version` 作为 artifact 上传

### 阶段 2：Windows Runner —— 链接成 `.exe`

在 `windows-latest` 上：
1. 下载阶段 1 的 artifact（`run.generated.c` + `.version`）
2. 下载并安装 Chez Scheme Windows 版（`ChezScheme10.4.1.exe`）
3. 激活 MSVC（`ilammy/msvc-dev-cmd@v1`，runner 自带 VS2022）
4. 克隆 `gwatt/chez-exe` 并构建 Windows 版（获取 `console_main.obj` 和 `petite-chez.lib`）
5. 用 `cl.exe` 链接生成 `run.exe`（使用 `/MT` 静态 CRT，不依赖 VC++ Redistributable）
6. 将 `run.exe` 和 `.version` 作为 release artifact 上传

## 链接命令

```batch
cl /nologo /MT /Fe:run.exe run.generated.c console_main.obj rpcrt4.lib ole32.lib advapi32.lib User32.lib petite-chez.lib
```

使用 `/MT`（静态 CRT 链接），目标机器**不需要安装任何额外的 VC++ 运行时库**。

## 三个风险点及解释

### 风险点 1：Chez Scheme Windows 安装程序的静默安装

**问题**：GitHub Actions 的 Windows runner 是无人值守环境，无法点击安装向导。需要确认 `ChezScheme10.4.1.exe` 支持**静默安装**（silent install），以及正确的命令行参数。

**常见安装程序类型的静默参数**：
- **Inno Setup**（很多开源软件使用）：`/SILENT` 或 `/VERYSILENT`，安装目录用 `/DIR="C:\ChezScheme"`
- **NSIS**（另一种常见安装程序）：`/S`
- **自定义安装程序**：参数可能不同

**当前状态**：`ChezScheme10.4.1.exe` 的具体安装程序类型和静默参数**尚未验证**。如果静默安装失败，整个 workflow 会在这一步中断。

**验证方法**：在本地 Windows 或 GitHub Actions 测试环境中尝试：
```batch
ChezScheme10.4.1.exe /?
ChezScheme10.4.1.exe /S
ChezScheme10.4.1.exe /SILENT
```

**备选方案**：如果静默安装不可行，可以改用 **MSYS2 的 Chez Scheme 包**（`mingw-w64-x86_64-chez-scheme`），通过 `pacman` 命令行安装，完全不需要交互式安装向导。

---

### 风险点 2：`gwatt/chez-exe` 原始仓库的 Windows 构建

**问题**：需要确认 `gwatt/chez-exe` 原始仓库在 Windows 上的构建流程是否完整可用。

> **注意**：`ufo5260987423/chez-exe` fork 仅用于解决 NixOS 构建问题，Windows 构建应基于原始仓库 `gwatt/chez-exe`。

**需要验证的内容**：
1. `gen-config.ss` 在 Windows 上是否能正确生成 `tools.ini`（从代码看，Windows 分支存在，生成 `tools.ini` 而非 `make.in`）
2. 仓库中是否有 `Makefile.win` 或类似的 nmake 构建文件
3. `nmake install` 在 Windows 上是否能正确编译出 `console_main.obj`、`gui_main.obj` 和 `petite-chez.lib`

**当前状态**：`gwatt/chez-exe` 的 Windows 构建流程**尚未在 CI 中验证过**。如果 `nmake` 构建失败，整个 workflow 会中断。

**验证方法**：在本地 Windows 或 GitHub Actions 测试环境中尝试：
```batch
git clone https://github.com/gwatt/chez-exe.git
cd chez-exe
scheme --script gen-config.ss --bootpath C:\ChezScheme\lib\csv10.4.1\ta6nt
dir Makefile.win  REM 确认 Makefile.win 是否存在
nmake /f Makefile.win
```

**备选方案**：如果 `nmake` 构建失败，可以尝试直接复制 Chez Scheme Windows 安装目录中的 `main.obj` 和静态库来替代 chez-exe 的构建产物。

---

### 风险点 3：静态链接 vs 动态链接（已解决）

**问题**：MSVC 的 `/MD`（动态 CRT）要求目标机器安装对应版本的 VC++ Redistributable，而 `/MT`（静态 CRT）则把所有运行时库打包进 `.exe`，无需额外依赖。

**决策**：使用 `/MT` 静态 CRT 链接。release 分发的 `.exe` 对终端用户最友好，开箱即用。

## 与其他 CI workflow 的关系

- `release.yaml`：在 `push: tags: "*.*.*"` 时触发，目前只构建 Linux glibc 版本。Windows 构建可以作为**新增 job** 加入同一个 workflow，也可以作为**独立的 workflow**（如 `windows-release.yaml`）。
- `manually-release.yaml`：手动触发，同样可以加入 Windows 构建 job。

## 没有 Docker 怎么调试？

GitHub Actions 本身就是**远程 CI 服务**，完全不需要本地 Docker。

调试方案：
1. **写一个 `workflow_dispatch` 手动触发的测试 workflow**（`.github/workflows/test-windows-build.yaml`）
2. **push 到 GitHub 后**，在仓库的 Actions 页面点击 "Run workflow" 手动触发
3. **每一步的日志**（stdout/stderr）都可以在 GitHub 网页上实时查看
4. **如果某一步失败**，根据日志调整 workflow 文件，重新 push，再次触发

这种迭代方式比本地 Docker 更直接，因为测试环境和生产环境完全一致（都是 GitHub 的官方 runner）。

## 建议的下一步行动

1. **推送测试 workflow**：创建 `.github/workflows/test-windows-build.yaml` 并 push 到 GitHub
2. **手动触发测试**：在 Actions 页面点击 "Run workflow"，观察日志
3. **逐个验证风险点**：
   - 阶段 1（Linux）：确认 `run.generated.c` 能正常生成
   - 阶段 2（Windows）：确认静默安装参数、`nmake` 构建、链接是否成功
4. **基于测试结果**，把验证通过的步骤固化到正式的 `.github/workflows/windows-release.yaml` 中

---

# tokenizer.sls R6RS / R7RS 双兼容改造方案

> 方案时间：2026-06-04  
> 核心约束：**Chez Scheme R6RS 解析器必须继续正常工作；R7RS 兼容通过 tokenizer 层适配实现，不修改 Chez 本身。**

## 1. 关键认知：`top-environment` 是解释器级别的

`top-environment`（`'r6rs`、`'r7rs`、`'s7`、`'goldfish`）模拟的是 Scheme **解释器/运行时的环境**，不是单个文件的属性。同一个 workspace（项目）中的所有文件共享同一个 `top-environment`。

**因此**：
- `document` record **不增加** `top-environment` 字段（document 是文件级别的抽象）。
- `source-file->annotations` 通过**显式参数**接收 `top-environment`。
- 调用者（`workspace.sls`）在调用 tokenizer 时传入 workspace 级别的 `top-environment`。

## 2. `top-environment` 完整传递链追踪

### 2.1 主启动路径

```
run.ss (命令行 --top-environment)
  └─(hashtable-ref args "top-environment" 'r6rs)
    └─> init-server (scheme-langserver.sls)           ; 参数7: top-environment
         └─> make-server (server record)               ; 字段: top-environment
              └─> initialize handler
                   └─(server-top-environment server-instance)
                     └─> init-workspace (workspace.sls) ; 参数4: top-environment
                          ├─> init-virtual-file-system
                          ├─> init-library-node
                          ├─> init-file-linkage
                          └─> init-document (workspace.sls:452)
                               └─> source-file->annotations
                                    【当前】5 参数，无 dialect 信息
                                    【需要】6 参数，传入 top-environment
```

### 2.2 增量更新路径

```
update-file-node-with-tail (workspace.sls:310)
  └─> source-file->annotations text path start-pos #t target-document
       【当前】5 参数，无 dialect 信息
       【需要】6 参数，传入 (workspace-top-environment workspace-instance)
```

### 2.3 宏展开路径

```
self-defined-syntax.sls / syntax-rules.sls / macro-expander.sls
  └─> source-file->annotations string path    ; 2 参数调用
       走默认路径 → top-environment='r6rs
       ✅ 合理：宏展开输出是 Chez R6RS 形式
```

### 2.4 测试/调试路径

```
parallel-log-debug.sps
  └─> init-server ... 'r6rs #t
       和主启动路径一致

tests/analysis/test-tokenizer*.sps
  └─> source-file->annotations path          ; 1 参数
       或 source-file->annotations source path  ; 2 参数
       默认 top-environment='r6rs，无需修改

tests/analysis/test-tokenizer-diagnose.sps:43,56
  └─> source-file->annotations source path start-pos #t d
       【需要】改为 6 参数，传 'r6rs
```

## 3. 需要修改的文件与调用点清单

| 文件 | 行号 | 修改内容 |
|------|------|----------|
| `analysis/tokenizer.sls` | `source-file->annotations` case-lambda | 增加第 6 参数 `top-environment`（默认 `'r6rs`） |
| `analysis/tokenizer.sls` | `consume-sps-auxiliary` | 修复 `#;` 处理（通用修复，见 4.3） |
| `analysis/tokenizer.sls` | tolerant parse `except` 分支 | 插入 dialect 感知逻辑（见 4.4） |
| `analysis/workspace.sls` | `init-document` 第 464 行 | `source-file->annotations` 改为 6 参数，传入 `top-environment` |
| `analysis/workspace.sls` | `update-file-node-with-tail` 第 327 行 | `source-file->annotations` 改为 6 参数，传入 `(workspace-top-environment workspace-instance)` |
| `analysis/tokenizer.sls` | 内部递归第 373 行 | `source-file->annotations` 改为 6 参数，传入 `top-environment` |
| `tests/analysis/test-tokenizer-diagnose.sps` | 第 43、56 行 | `source-file->annotations` 改为 6 参数，传 `'r6rs` |

**不需要修改的文件**：
- `virtual-file-system/document.sls`：`document` record 不增加字段
- 宏展开代码（`self-defined-syntax.sls`、`syntax-rules.sls`、`macro-expander.sls`）：2 参数调用默认 `'r6rs`
- 测试文件中的 1/2 参数调用：默认 `'r6rs`

## 4. 具体修改方案

### 4.1 `source-file->annotations` 接口扩展

在现有 5 参数 arity 基础上增加第 6 参数 `top-environment`，默认 `'r6rs`：

```scheme
(define source-file->annotations
  (case-lambda
    ([path]
      (source-file->annotations (read-string path) path 'r6rs))
    ([source path]
      (source-file->annotations source path (consume-sps-auxiliary source) #t #f 'r6rs))
    ([source path start-position]
      (source-file->annotations source path start-position #t #f 'r6rs))
    ([source path start-position tolerant?]
      (source-file->annotations source path start-position tolerant? #f 'r6rs))
    ([source path start-position tolerant? maybe-document]
      (source-file->annotations source path start-position tolerant? maybe-document 'r6rs))
    ([source path start-position tolerant? maybe-document top-environment]
      ; 实际处理逻辑...
      ...)))
```

**向后兼容性**：所有现有的 1~5 参数调用无需修改，自动默认 `'r6rs`。

### 4.2 `workspace.sls` 调用点修改

**`init-document` 中**（第 464 行）：
```scheme
; 修改前
(source-file->annotations s path (consume-sps-auxiliary s) #t d)

; 修改后
(source-file->annotations s path (consume-sps-auxiliary s) #t d top-environment)
```

**`update-file-node-with-tail` 中**（第 327 行）：
```scheme
; 修改前
(source-file->annotations text (uri->path (document-uri target-document)) 
  (consume-sps-auxiliary text) #t target-document)

; 修改后
(source-file->annotations text (uri->path (document-uri target-document))
  (consume-sps-auxiliary text) #t target-document
  (workspace-top-environment workspace-instance))
```

### 4.3 `consume-sps-auxiliary` 修复 `#;`（通用修复，不依赖 dialect）

当前代码遇到 `#` 后如果 lookahead 不是 `|`，就继续读取。下一个字符 `;` 触发行注释模式，导致 `#;datum` 只被跳过一行。

修复：在 `#` 分支中识别 `#;`，用 `get-datum` 跳过完整 datum：

```scheme
[(eqv? c #\#) 
  (cond
    [(and (not inline-comment?) (eqv? #\| (lookahead-char ip)))
      (get-char ip)
      (consume-block-comment ip)
      (loop (get-char ip) #f)]
    [(and (not inline-comment?) (eqv? #\; (lookahead-char ip)))
      (get-char ip) ; consume ;
      ; 跳过被注释的 datum。Chez 的 get-datum 本身支持 #;，
      ; 因此这对 R6RS 和 R7RS 都适用。
      (guard (e [else (void)])
        (get-datum ip))
      (loop (get-char ip) #f)]
    [else (loop (get-char ip) inline-comment?)])]
```

### 4.4 tolerant parse 增加 dialect 感知

在 `source-file->annotations` 的 `except` 分支中，当 `tolerant?` 为真时，先检查 `top-environment`，再决定修复策略：

```scheme
(except e
  [(and tolerant? (condition? e))
    (let ([error-position (private:compute-error-position e port)])
      (cond
        ; R7RS 模式下，先尝试 R7RS 特有修复
        [(and (memq top-environment '(r7rs s7 goldfish))
              (private:r7rs-fixable? e source error-position))
         => (lambda (patched-source)
              (source-file->annotations patched-source path start-position
                tolerant? maybe-document top-environment))]
        ; 否则走现有 R6RS tolerant parse
        [else
          (when maybe-document
            (append-new-diagnoses maybe-document ...))
          (let ([after (private:tolerant-parse->patch source error-position)])
            (if (= (string-length after) (string-length source))
              (source-file->annotations after path start-position #f
                maybe-document top-environment)
              (error 'tokenizer-error (condition-message e) (condition-irritants e))))]))]
  [(condition? e) ...]
  [else ...])
```

### 4.5 R7RS 错误映射函数

`private:r7rs-fixable?` 接收 condition、source、position，返回 `#f` 或修复后的 source 字符串。

**`#u8(...)` → `#vu8(...)`**：

当 `condition-message` 匹配 "invalid sharp-sign prefix" 且 `condition-irritants` 的字符是 `u` 时，检查 source 中 position 附近是否确实是 `#u8(`。如果是，将 `#u8(` 替换为 `#vu8(`。

**`#\null` / `#\escape` → `#\nul` / `#\esc`**：

当 `condition-message` 匹配 "invalid character name" 时，根据 `condition-irritants` 中的字符名映射：
- `"null"` → `#\nul`
- `"escape"` → `#\esc`
- 其他 → `#f`（不可修复，回退到现有 tolerant parse）

**为什么不全局预处理 source 字符串？**
- 全局替换 `"#u8("` → `"#vu8("` 会污染字符串内容，如 `"use #u8(1 2 3)"` 被错误修改。
- 在 condition handler 中修复：只在 Chez 明确报错的位置替换，不影响字符串和注释。

## 5. 实施步骤

1. **Phase 1**：修改 `tokenizer.sls`：
   - 扩展 `source-file->annotations` 接口（6 参数 arity）
   - 修复 `consume-sps-auxiliary` 的 `#;` 处理
   - 添加 `private:r7rs-fixable?` 及相关映射函数
   - 在 `except` 分支中插入 dialect 感知逻辑
2. **Phase 2**：修改 `workspace.sls`：
   - `init-document` 第 464 行：6 参数调用，传入 `top-environment`
   - `update-file-node-with-tail` 第 327 行：6 参数调用，传入 `(workspace-top-environment workspace-instance)`
   - 内部递归第 373 行：6 参数调用，传入 `top-environment`
3. **Phase 3**：更新测试：
   - `test-tokenizer-diagnose.sps` 第 43、56 行：改为 6 参数，传 `'r6rs`
   - 新增 R7RS 专项测试（`#u8`、`#\null`、多行 `#;`）
4. **Phase 4**：验证 R6RS 回归：跑完整 `test.sh`，确认 R6RS 模式下零行为变化

## 6. 兼容性保证

| 场景 | 预期行为 | 验证方式 |
|------|----------|----------|
| R6RS 代码 + `#u8(...)` | 报错（R6RS 不合法），tolerant parse 按现有逻辑降级 | `test-tokenizer.sps` 现有用例 |
| R7RS 代码 + `#u8(...)` | 不报错，解析为 `#vu8(...)` | 新增 R7RS 测试用例 |
| R6RS 代码 + `#\null` | 报错， tolerant parse 替换为空格 | 现有用例 |
| R7RS 代码 + `#\null` | 不报错，解析为 `#\nul` | 新增 R7RS 测试用例 |
| 任何代码 + 多行 `#;` | `consume-sps-auxiliary` 正确跳过 datum | 新增 `#;` 测试用例 |
| 纯 R6RS 项目（如 scheme-langserver 自身） | 100% 行为不变 | 跑完整 `test.sh` |
