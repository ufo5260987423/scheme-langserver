# Task: 改造并验证 output-type-analysis.ss 的双模式输出

## 1. 需求

`bin/output-type-analysis.ss` 目前只有一种行为：**对整个 folder 内所有 library 做批量类型导出**。

现需要改造为**双模式**，通过命令行参数区分：

| 模式 | 参数签名 | 行为 |
|------|----------|------|
| **Mode A: 单库模式** | `scheme --script bin/output-type-analysis.ss <target-dir> <library-name> <output-file>` | 只分析 `<target-dir>` 内指定的某个 library（如 `(fixtures simple-lib math)`），输出该库导出标识符的类型 |
| **Mode B: 批量模式** | `scheme --script bin/output-type-analysis.ss <target-dir> <output-file>` | 保持现有行为：遍历 folder 内**所有** library，输出每个 library 中 `(export ...)` 子句里标识符的类型 |

> **向后兼容**：原两参数调用 `<target-dir> <output-file>` 仍为批量模式，行为不变。

---

## 2. MCP 机制说明（关键）

### 2.1 MCP 调用链与可执行文件地址

scheme-langserver MCP 的完整调用链如下：

```
Kimi CLI
  → 读取 ~/.kimi/mcp.json
    → 启动 scheme_langserver_bridge Python 模块
      → 读取环境变量 SCHEME_LANGSERVER_PATH
        → 通过 asyncio.create_subprocess_exec 启动 scheme-langserver 子进程
          → 通过 stdin/stdout 进行 LSP JSON-RPC 通信
            → Bridge 将 LSP 消息封装为 MCP 工具响应
```

**当前配置**（`~/.kimi/mcp.json`）：
```json
{
  "mcpServers": {
    "scheme-langserver": {
      "command": "/home/ufo/Documents/workspace/uv-environment/scheme-langserver-kimi-plugin/.venv/bin/python3",
      "args": ["-m", "scheme_langserver_bridge"],
      "env": {
        "SCHEME_LANGSERVER_PATH": "/home/ufo/Documents/workspace/scheme-langserver/run"
      }
    }
  }
}
```

**结论**：MCP 调用的 scheme-langserver 可执行文件地址是：
```
/home/ufo/Documents/workspace/scheme-langserver/run
```
这是**本地开发版**的静态编译二进制（ELF 64-bit），而非 Nix Store 中的旧版 2.0.3。

### 2.2 本地改进代码 → 优化 MCP 表现的机制

`run` 是一个**静态编译的二进制**，由 `build.sh` 通过 `compile-chez-program run.ss --static` 生成。它包含了所有 `.sls` 源码的编译结果。

**关键机制**：

| 步骤 | 状态 | 影响 |
|------|------|------|
| 修改 `.sls` 源码（如 `output-type-analysis.ss`、类型推断规则等） | `run` **未更新** | MCP 调用的是旧 `run`，**看不到改进** |
| 执行 `bash build.sh` 重新编译 | `run` **已更新** | MCP 下次 `lsp_initialize` 启动新进程，**感知改进** |
| 清理 `.akku/libobj/scheme-langserver` 缓存 | 测试缓存已清 | 避免 `incompatible fasl-object version` 等错误 |

**因此，任何本地代码改进要反映到 MCP 表现上，必须满足**：
1. **修改源码**（`.sls` 或 `run.ss`）
2. **重新编译** `bash build.sh`（生成新的 `run` 二进制）
3. **MCP 重新初始化**（`lsp_initialize` 会启动新的 `run` 进程）

> **注意**：不需要修改 `~/.kimi/mcp.json`，因为 `SCHEME_LANGSERVER_PATH` 已经正确指向本地开发版。

### 2.3 改造期间的 MCP 使用策略

由于 `output-type-analysis.ss` 是一个**独立脚本**（非 LSP 分析的目标文件），MCP 在本次任务中的角色是：

- **验证改造后的源码静态正确性**：对 `output-type-analysis.ss` 做 hover/definition/diagnostics
- **验证底层类型推断子系统**：对 fixture 中的 `.scm.txt` 做 hover，确认类型推断是否产生正确结果
- **不可替代运行测试**：`output-type-analysis.ss` 的输出正确性最终必须通过 `scheme --script` 实际执行来验证

**MCP 与运行测试的协同节奏**：
```
修改 output-type-analysis.ss → MCP diagnostics 检查语法 → 运行测试验证输出
修改底层类型推断 .sls      → build.sh 编译 → MCP hover 验证 fixture → 运行测试验证输出
```

---

## 3. 改造内容

### 3.0 修复现有批量模式的参数错误

现有 `output-type-analysis.ss` 存在两处底层 bug，必须在叠加新功能之前修复：

1. **`import-from-external-index-node` 参数缺失**（第 30 行）：
   ```scheme
   ;; BUG：只传了 1 个参数
   (map import-from-external-index-node index-node-list)
   ```
   `import-from-external-index-node` 的签名是 `(document root-index-node)`，需要 2 个参数。修正为：
   ```scheme
   (map (lambda (index-node) (import-from-external-index-node target-document index-node)) index-node-list)
   ```

2. **文件过滤器与场景不匹配**：
   - 现有代码 `(init-workspace target-path #t #t #t)` 中 `#t` 走 else 分支，使用 **akku 过滤器**，只认 `.sps` `.sls` `.scm` `.ss`。
   - fixture 目录（`simple-lib`、`record-type`、`two-libs`）中的文件后缀是 `.scm.txt`，**会被 akku 过滤器排除**，导致输出为 0 字节空文件。
   - 项目自身源码目录有 `.akku/list` 和 `.sls` 文件，akku 过滤器能正常工作，但由于参数 bug 1，运行到文件处理阶段会直接崩溃。

**修复方案**：脚本内部根据目标目录特征**自动选择过滤器**：
```scheme
(define (detect-file-filter path)
  (if (file-directory? (string-append path "/.akku"))
    'akku
    'txt))
```
- 目标目录存在 `.akku` 子目录 → 用 `'akku`（适用于项目自身）
- 不存在 → 用 `'txt`（适用于 fixture）

> **注意**：`generate-txt-file-filter` 只包含 `.scm.txt` 和目录；`generate-akku-acceptable-file-filter` 包含 `.sps` `.sls` `.scm` `.ss` 及 `.akku/lib`。两种过滤器互不重叠，不能混用。

---

### 3.1 命令行参数解析

在 `output-type-analysis.ss` 顶部增加参数路由：

```scheme
(let* ([args (command-line-arguments)]
       [argc (length args)])
  (cond
    [(= argc 2)  ;; Mode B: 批量
     (let ([target-path (car args)]
           [output-path (cadr args)])
       ...现有逻辑...)]
    [(= argc 3)  ;; Mode A: 单库
     (let ([target-path (car args)]
           [library-name-string (cadr args)]  ;; e.g. "(fixtures simple-lib math)"
           [output-path (caddr args)])
       ...单库逻辑...)]
    [else
      (display "Usage:\n")
      (display "  Single library: scheme --script output-type-analysis.ss <dir> <lib-name> <out>\n")
      (display "  All libraries:  scheme --script output-type-analysis.ss <dir> <out>\n")
      (exit 1)]))
```

### 3.2 单库模式核心逻辑

单库模式需要新增一个函数 `step-single-library`：

1. 用 `init-workspace` 加载 `<target-dir>`，**通过 `detect-file-filter` 自动选择过滤器**
2. 在 `workspace-library-node` 树中**按 library name 查找**目标 library-node
3. 对该 library-node 下的所有 file-node 执行类型推断
4. 输出该 library 的**导出标识符**（而非导入标识符）的类型

> **关键决策**：单库模式输出什么？
> - 批量模式输出的是「每个 library 中 **`(export ...)` 子句里的标识符**」
> - 单库模式输出的是「指定 library 中 **`(export ...)` 子句里的标识符**」
> - 两者本质相同，区别仅在于批量模式遍历所有 library，单库模式只处理一个 library
> - 因此单库模式应遍历该 library 下所有 file-node，收集 `export-to-other-node`，对其中每个导出标识符运行 `type:recursive-interpret-result-list`

### 3.3 Library 查找辅助函数

在 `workspace-library-node` 树中按 name 查找节点。library name 是符号列表，如 `(fixtures simple-lib math)`，用户传入时可作为字符串 `"(fixtures simple-lib math)"`，内部用 `read` 或手动解析为符号列表。

```scheme
(define (find-library-node-by-name root-library-node name-list)
  ;; DFS/BFS 遍历 library-node 树
  ;; (library-node-name->string node) 可生成字符串用于比较
  )
```

### 3.4 修复批量模式的 export 标识符收集

**修复参数错误**：批量模式现有逻辑中 `(map import-from-external-index-node index-node-list)` 缺少 `document` 参数。修正后应为：
```scheme
(map (lambda (index-node) (import-from-external-index-node target-document index-node)) index-node-list)
```

> 注意：`import-from-external-index-node` 名字有误导性，它实际解析的是 library 的 **`(export ...)` 子句**，返回 `index-node-references-export-to-other-node`，即 export 中的标识符引用，而非 import。

### 3.5 单库模式的 export 标识符收集

批量模式已用 `import-from-external-index-node` 收集每个 library 的 **export 标识符**。
单库模式需要直接遍历指定 library 下所有 file-node，手动收集 `export-to-other-node`：

```scheme
(define (collect-exported-identifiers library-node)
  (apply append
    (map
      (lambda (file-node)
        (let ([doc (file-node-document file-node)])
          (apply append
            (map index-node-references-export-to-other-node
                 (document-index-node-list doc)))))
      (library-node-file-nodes library-node))))
```

---

## 4. 渐进验证方案

> **前置条件**：以下验证均基于「现有批量模式 bug 已修复 + 自动过滤器检测已生效」的代码。如果直接运行未修改的 `output-type-analysis.ss`，fixture 场景会输出 0 字节，项目自身场景会崩溃。

改造完成后，按以下 Stage 由小到大验证两种模式。

### Stage 1: simple-lib — 最小双模式验证

**Fixture**: `tests/resources/workspace-fixtures/simple-lib/`
- `math` 库：导出 `add`
- `main` 库：导出 `run`，导入 `add`

#### 1.1 单库模式（math）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  "(fixtures simple-lib math)" \
  /tmp/stage1-math.txt
```

**期望输出**（只包含 `math` 的导出）：
```
library:	(fixtures simple-lib math)
path:		.../math.scm.txt
identifier:	add
type:		(number? <- (inner:list? number? number?))
```

**正确性判定**：
- 输出中只有 `add`，没有 `run`
- `add` 类型包含 `number?`

#### 1.2 单库模式（main）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  "(fixtures simple-lib main)" \
  /tmp/stage1-main.txt
```

**期望输出**（只包含 `main` 的导出）：
```
library:	(fixtures simple-lib main)
identifier:	run
type:		(number? <- (inner:list?))
```

#### 1.3 批量模式（simple-lib 整体）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  /tmp/stage1-all.txt
```

**期望输出**：包含所有 library 中 `(export ...)` 子句的标识符（现有行为不变）
- `math` 中应有 `add`
- `main` 中应有 `run`
- `math` 和 `main` 不会包含系统库 `(rnrs)` 的标识符（`import-from-external-index-node` 只解析 `(export ...)` 子句）

**判定**：
- ✅ 三种调用输出均符合预期 → Stage 2
- ❌ 任一不符合 → 进入 Debug Loop

> 若输出为 0 字节，检查脚本内部是否已启用自动过滤器检测（`simple-lib` 无 `.akku/list`，必须走 `'txt` 分支）。若崩溃报 `incorrect number of arguments`，检查 `import-from-external-index-node` 是否已补传 `document`。

### Stage 2: record-type — Record 类型单库验证

**Fixture**: `tests/resources/workspace-fixtures/record-type/`

#### 2.1 单库模式（point）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/record-type \
  "(point)" \
  /tmp/stage2-point.txt
```

**期望输出**：包含 `make-point`, `point?`, `point-x`, `point-x-set!` 的类型

#### 2.2 验证点
- 单库模式必须能正确处理 `define-record-type` 的导出标识符
- 类型中应出现 `point?` 或 `[identifier-reference point?]`（取决于当前 `record.sls` 实现）

### Stage 3: two-libs — 跨库引用验证

**Fixture**: `tests/resources/workspace-fixtures/two-libs/`

#### 3.1 单库模式（helper）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/two-libs \
  "(fixtures two-libs helper)" \
  /tmp/stage3-helper.txt
```

**期望输出**：`helper-fn` 类型为 `(number? <- (inner:list? number?))`

#### 3.2 单库模式（main）
```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/two-libs \
  "(fixtures two-libs main)" \
  /tmp/stage3-main.txt
```

**期望输出**：`main` 的导出 `main` 函数类型

#### 3.3 批量模式
验证 `helper` 和 `main` 两个 library 的 export 标识符类型均被输出

### Stage 4: 项目自身 — 大规模验证

#### 4.1 单库模式抽样
```bash
scheme --script bin/output-type-analysis.ss \
  /home/ufo/Documents/workspace/scheme-langserver \
  "(scheme-langserver analysis type domain-specific-language interpreter)" \
  /tmp/stage4-single.txt
```

**期望**：输出 `type:interpret`, `type:recursive-interpret-result-list` 等核心函数的类型

#### 4.2 批量模式
```bash
scheme --script bin/output-type-analysis.ss \
  /home/ufo/Documents/workspace/scheme-langserver \
  /tmp/stage4-all.txt
```

**性能期望**：单线程 < 120s，输出文件 > 1MB（因项目规模大）

> 项目自身目录存在 `.akku/list`，自动过滤器检测应正确选择 `'akku`，扫描所有 `.sls` 源码文件。

---

## 5. Debug Loop（通用模板）

任一 Stage 失败时，按以下流程定位：

### Step 1: 确认改造后源码的静态正确性（MCP）

| MCP 工具 | 检查点 |
|----------|--------|
| `lsp_initialize` | 启动 scheme-langserver（确保 `run` 二进制已重新编译） |
| `lsp_open` | 打开 `bin/output-type-analysis.ss` |
| `lsp_diagnostics` | 有无语法错误、未绑定变量 |
| `lsp_document_symbol` | 新函数（`find-library-node-by-name`、`step-single-library` 等）是否被识别 |

### Step 2: 运行时快速定位（Shell）

若运行崩溃/输出为空：

| 检查项 | 方法 |
|--------|------|
| 参数是否解析正确 | 在脚本内临时插入 `(pretty-print args)` |
| library 是否被找到 | 打印 `(library-node-name->string found-node)` |
| 导出标识符列表是否为空 | 打印 `(length exported-identifiers)` |
| 类型推断是否返回空 | 打印 `type:recursive-interpret-result-list` 的 result 长度 |

### Step 3: 深入类型推断子系统（MCP + Shell）

若判定为底层问题（非脚本逻辑错误）：

| 手段 | 用途 |
|------|------|
| `lsp_open` + `lsp_hover` | 在 fixture 的标识符定义处查看 LSP 推断的类型 |
| `lsp_definition` | 跳转到 `generator.sls` / `trivial.sls` / `record.sls` 等规则定义 |
| `lsp_hover` | 在 `interpreter.sls` 的 `type:interpret` 处查看签名，确认 env 构造要求 |
| `bash build.sh` | 若修改了底层 `.sls`，重新编译 `run` 使 MCP 感知 |

**关键提醒**：若修改了 `analysis/` 下的任何 `.sls` 并用 MCP 验证，**必须先执行 `bash build.sh`**，否则 MCP 仍使用旧的 `run` 二进制，hover/definition 结果不会反映修改。

---

## 6. 执行顺序与编译检查点

```
改造 output-type-analysis.ss
    │
    ▼
运行 build.sh 重新编译 run
    │
    ▼
Stage 1: simple-lib（单库 + 批量）
    │
    ├── 失败 → Debug Loop → 修改源码 → build.sh → 重试
    │
    └── 通过
            │
            ▼
    Stage 2: record-type（单库）
            │
            └── 通过
                    │
                    ▼
        Stage 3: two-libs（跨库）
                    │
                    └── 通过
                            │
                            ▼
            Stage 4: 项目自身（抽样 + 全量）
                            │
                            └── 产出最终报告
```

---

## 7. 最终产出

1. **改造后的 `bin/output-type-analysis.ss`**：支持 2 参数（批量）和 3 参数（单库）
2. **重新编译的 `run` 二进制**：确保 MCP 使用的是最新版本
3. **单库模式验证报告**：Stage 1–3 的期望 vs 实际输出对比
4. **批量模式回归验证**：确认现有行为未被破坏
5. **Stage 4 大规模验证**：项目自身的单库抽样 + 全量批量输出
6. **问题清单**（如有）：未修复的类型推断子系统缺陷，附 MCP 诊断证据
