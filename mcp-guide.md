# scheme-langserver MCP 开发指南

本文档面向 scheme-langserver 的开发者与维护者，说明如何在本地利用 scheme-langserver MCP bridge 来加速开发、调试和回归验证。

---

## 1. 什么是 scheme-langserver MCP

scheme-langserver MCP 是一个 [Model Context Protocol](https://modelcontextprotocol.io/) 服务器，桥接 Kimi Code CLI 与 scheme-langserver。它把 LSP 能力暴露为 MCP tools，使我们可以在同一个仓库里修改代码并立即让 LSP 分析自己的代码。

MCP 配置位于 `~/.kimi/mcp.json`，bridge 源码位于：

```
/home/ufo/Documents/workspace/uv-environment/scheme-langserver-kimi-plugin/
```

当前项目通过 `.scheme-langserver.toml` 向 bridge 提供项目级配置。

---

## 2. 已注册的 MCP 工具

| 工具 | 用途 |
|------|------|
| `lsp_initialize` | 初始化 LSP 连接（指定项目根目录） |
| `lsp_open` / `lsp_change` / `lsp_close` | 文档同步 |
| `lsp_diagnostics` | 拉取诊断（括号匹配、未定义标识符、未使用变量等） |
| `lsp_definition` | 跳转到定义 |
| `lsp_references` | 查找全部引用 |
| `lsp_hover` | 悬停提示（类型/文档） |
| `lsp_complete` | 自动补全 |
| `lsp_document_symbol` | 文件符号大纲 |
| `lsp_workspace_symbol` | 工作区符号搜索 |
| `lsp_code_action` | 代码动作 |
| `lsp_signature` | 签名帮助 |
| `lsp_rename` | 重命名 |
| `lsp_restart` | 重启 LSP 并恢复已打开文档 |
| `lsp_shutdown` | 关闭 LSP |
| `lsp_export_debug_report` | 导出调试报告（用于上游 issue） |

---

## 3. 开发者如何利用 MCP

### 3.1 核心原则

把 MCP 当作**自带 IDE 的实时测试床**。每次修改 `.sls` 源码后，可以立即让 LSP 分析同一个代码库，验证修改是否破坏了引用解析、类型推断或诊断输出，而不必手动打开编辑器或写大量集成测试。

### 3.2 推荐工作流

```text
修改 .sls 源码
    ↓
rm -rf .akku/libobj/scheme-langserver    # 清除编译缓存
    ↓
source .akku/bin/activate && compile-chez-program run.ss   # 编译二进制（用于运行/测试）
    ↓
lsp_initialize(root_dir=本项目)            # 使用项目配置的 run-mcp-cache.sh
    ↓
lsp_open(目标文件)
    ↓
lsp_diagnostics / lsp_definition / lsp_references / lsp_document_symbol
    ↓
结果是否符合预期？
    ├─ 是 → 完成
    └─ 否 → 回到修改源码
```

> **注意**：不要通过 `langserver_path="./run"` 让 MCP 加载编译后的二进制，因为当前 `compile-chez-program` 编译的产物无法正确执行 `fasl-write`（见 4.1）。项目 `.scheme-langserver.toml` 已配置为使用 `run-mcp-cache.sh`（即 `scheme --script run.ss`），应保留该配置。

### 3.3 关键工具的使用场景

- **`lsp_diagnostics`**：Scheme 的 S-expression 对括号极度敏感，一次手误可能让整个文件无法解析。修改后务必调用此工具检查 `unclosed parenthesis`、`unexpected close bracket` 等结构性错误。
- **`lsp_definition` / `lsp_references`**：在重构 identifier reference、lambda 参数处理、语法规则等模块前，先用它们确认影响范围。
- **`lsp_document_symbol`**：验证 AST walker 是否正确识别了新语法形式引入的函数、变量或宏。
- **`lsp_workspace_symbol`**：在大型改动中快速定位某个 helper 函数的所有实现位置。
- **`lsp_export_debug_report`**：当 LSP 崩溃或返回明显错误结果时，一键生成包含完整 LSP 流量和项目源码的调试包，用于向 <https://github.com/ufo5260987423/scheme-langserver/issues> 提交 issue。

---

## 4. 当前已知问题

### 4.1 P0：workspace cache 保存失败（已修复）

在调用 `exit` 时，`private:save-workspace-cache-if-any` 会尝试保存 workspace cache。此前通过 MCP bridge 操作 scheme-langserver 自身项目时，cache 无法落盘。

#### 根因（三个叠加问题）

1. **编译后的 `./run` 二进制无法执行 `fasl-write`**（已修复）
   `compile-chez-program` 默认链接 `petite-chez.a`，会丢失 Chez 内部变量 `$write-fasl-bytevectors` 的绑定，直接调用 `fasl-write` 会报错：
   ```text
   variable $write-fasl-bytevectors is not bound
   ```
   解决方案是在编译 release 二进制时加上 `--full-chez`，让它链接完整的 `full-chez.a`。`build.sh`、`Dockerfile` 和 `Dockerfile.musl` 已同步更新。
   因此 MCP 场景可以继续使用 `scheme --script run.ss`（即 `run-mcp-cache.sh`），也可以使用编译后的 `./run`。

2. **bridge 的 graceful shutdown 时间太短**
   bridge 原实现在发送 `exit` 通知后仅等待 3 秒就 `SIGKILL` 子进程。scheme-langserver 自身项目的 cache 写入约需 40 秒，3 秒远远不够。

3. **虚拟地址空间限制（RLIMIT_AS）导致 SIGABRT**
   `.scheme-langserver.toml` 原设置 `max_memory_mb = 4096`。scheme-langserver 自身项目（128 个 `.sls`）在序列化 workspace 对象图时虚拟地址空间不足，保存 cache 期间触发 `SIGABRT`（exit code -6）。

#### 修复结果

| 运行方式 | 冷启动 | cache 保存 | 二次启动 |
|----------|--------|------------|----------|
| 修复前（./run + 3s kill + 4GB） | 超时/失败 | 失败 | 无 cache |
| 修复后（run-mcp-cache.sh + 120s wait + 8GB） | ~45-48 秒 | ✅ ~40 秒 | ✅ ~2 秒 |

修复提交：
- `scheme-langserver.sls`：防御性修正 guard 条件 `(and workspace (not (null? workspace)) cache-path)`。
- `scheme-langserver-kimi-plugin/src/scheme_langserver_bridge/lsp_client.py`：`stop()` 发送 `exit` 后等待最多 120 秒让子进程自然退出。
- `.scheme-langserver.toml`：`max_memory_mb` 从 4096 提升到 8192。

### 4.2 项目级 timeout 偏紧

默认 bridge timeout 为 30 秒，对 scheme-langserver 自身冷启动不足。实测：

- 使用 `.scheme-langserver.toml` 中的 `run-mcp-cache.sh`（`scheme --script run.ss`）：首次冷启动约 40-50 秒，30 秒 timeout 会超时。
- 一旦 cache 生成，二次启动约 2 秒（已优化）。

建议设置：

```bash
export SCHEME_LANGSERVER_TIMEOUT=120
export SCHEME_LANGSERVER_COMPLETION_TIMEOUT=120
```

### 4.3 项目配置建议使用解释执行脚本

`.scheme-langserver.toml` 当前指定：

```toml
langserver_path = "/home/ufo/Documents/workspace/scheme-langserver/run-mcp-cache.sh"
```

该脚本最终调用 `scheme --script run.ss`。这是为了兼容开发环境：

- 本地开发时通常不会每次都重新编译 `./run`；
- `scheme --script run.ss` 启动速度虽然比编译后的二进制稍慢，但代码改动后立即生效。

如果使用 release 构建的 `./run`（由 `build.sh` 生成，已使用 `--full-chez`），同样可以正确执行 `fasl-write` 并保存 workspace cache。MCP 场景下两种启动方式都有效。

---

## 5. 修复计划

### P0 修复：workspace cache 保存失败

#### 5.1 防御性修复 guard 条件（已完成）

在 `scheme-langserver.sls:36`：

```scheme
;; 修改前
(when (and workspace cache-path)
  ...)

;; 修改后
(when (and workspace (not (null? workspace)) cache-path)
  ...)
```

此修复已提交。

#### 5.2 编译产物现已支持 `fasl-write`

`build.sh`、`Dockerfile` 和 `Dockerfile.musl` 中的编译命令已改为：

```bash
compile-chez-program --full-chez run.ss --static
```

`--full-chez` 会让 `compile-chez-program` 链接 `full-chez.a` 而不是 `petite-chez.a`，完整运行时包含 `$write-fasl-bytevectors`，因此编译后的 `./run` 也能正常保存 workspace cache。

注意事项：
- `--full-chez` 编译的二进制体积比 petite 版本稍大；
- 静态链接时需要 `libuuid`、`libncurses`、`libtinfo` 等静态库，Dockerfile 已安装对应 `-dev`/`-static` 包；
- 开发环境仍可继续使用 `scheme --script run.ss`，无需每次编译。

#### 5.3 修复 MCP bridge 的 graceful shutdown 等待时间（已完成）

已修改 `scheme-langserver-kimi-plugin/src/scheme_langserver_bridge/lsp_client.py`，在 `stop()` 发送 `exit` 通知后等待最多 120 秒，让 scheme-langserver 完成 cache 保存并自然退出，超时后再强制 kill。

#### 5.4 调整 `.scheme-langserver.toml` 内存限制（已完成）

已把 `max_memory_mb` 从 4096 提升到 8192，避免保存 workspace cache 时因虚拟地址空间不足而 `SIGABRT`。

#### 5.5 增加测试

新增测试：

```text
用 MCP 初始化 scheme-langserver 项目本身，
打开 analysis/abstract-interpreter.sls，
请求 textDocument/documentSymbol，
断言返回非空且不崩溃，
最后 shutdown/exit 不卡住，
且 .scheme-langserver-cache/workspace.fasl 成功生成。
```

### P1 修复：优化 `.scheme-langserver.toml`（已完成）

当前配置：

```toml
langserver_path = "/home/ufo/Documents/workspace/scheme-langserver/run-mcp-cache.sh"
multi_thread = "disable"
type_inference = "disable"
top_environment = "R6RS"
cache_path = "/home/ufo/Documents/workspace/scheme-langserver/.scheme-langserver-cache"
max_memory_mb = 8192
```

同时在使用 MCP 时设置环境变量：

```bash
export SCHEME_LANGSERVER_TIMEOUT=120
export SCHEME_LANGSERVER_COMPLETION_TIMEOUT=120
```

---

## 6. 长期愿景

让 scheme-langserver 能够**自举式开发**：

1. 任何源码修改都能立即通过 MCP 在同一代码库上验证；
2. 冷启动一次后，后续启动依赖 workspace cache，秒级可用；
3. CI 中增加“MCP 自举测试”，防止回归；
4. 类型推断、多线程等高级能力也能在自身代码库上跑通。

---

## 7. 参考

- `AGENTS.md`：项目结构、构建步骤、测试约定、已知问题。
- `.scheme-langserver.toml`：当前 MCP bridge 项目级配置。
- `run-mcp-cache.sh`：bridge 启动脚本。
- `scheme-langserver.sls`：LSP 服务器入口与 cache 保存逻辑。
- `analysis/workspace.sls`：workspace cache 序列化准备。
- `analysis/workspace-cache.sls`：FASL cache 读写实现。
