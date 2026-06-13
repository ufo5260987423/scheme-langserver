# scheme-langserver 鲁棒性测试改进方案

> 目标：模拟真实用户在编辑器中使用 scheme-langserver 的全过程，确保服务器在面对各种手滑、粘贴、连续编辑时不会崩溃，不抛出未捕获异常。

## 1. 背景与问题

现有 `tests/robustness-lsp-replay.sps` 存在三方面不足：

1. **仅覆盖单线程**：`init-server` 以 `#f #f` 调用，多线程并发路径（`request-queue` + `thread-pool`）完全未测。
2. **人工构造代码，脱离真实场景**：27 个用例均为手写 `(define x ...)` / `(syntax-rules ...)` 等极简片段，未基于 `tests/resources/workspace-fixtures/` 下的真实项目代码。
3. **无连续编辑流**：每个用例只有一次 `didOpen` + 一次请求。真实用户在编辑器中是持续输入的：`didOpen` → `didChange` → `didChange` → `hover` → `didChange` ... 现有测试无法覆盖"错误状态累积"路径。

## 2. 改进方向

### 2.1 多线程并发鲁棒性测试

**目标**：验证多线程模式下并发请求不会导致崩溃或 workspace 状态损坏。

**方法**：
- 调用 `(init-server input output log #t #f 'r6rs)` 启动多线程服务器。
- 构造快速交替的消息流：`didChange` 与 `hover` / `completion` / `definition` 穿插发送。
- 典型场景：10 次 `didChange` + 10 次 `hover` 交错，最后 `shutdown`。
- 断言：`server-shutdown?` 为 `#t`，且未抛出非条件异常。

### 2.2 基于真实 Fixture 的随机字符变异（Fuzzing）

**目标**：模拟真实用户在真实代码上"手滑打错字"。

**方法**：
- 选取 `tests/resources/workspace-fixtures/` 下 3–5 个 fixture（如 `simple-lib`、`cascade-macro`、`let-syntax-auto-resolve`）。
- 读取 fixture 中的 `.scm.txt` 文件作为原始合法代码。
- 对每份代码进行 N 轮（建议 30–50 轮）随机变异，seed 固定以保证可复现：
  - **插入**：在随机位置插入一个字符（候选：`(`, `)`, `"`, `\\`, `#`, `;`, ` `, `\n`, `x`）
  - **删除**：在随机位置删除一个字符
  - **替换**：在随机位置替换为一个随机字符
- 每轮变异后发送完整 LSP 会话：
  1. `initialize`
  2. `didOpen`（变异后代码）
  3. `hover` 或 `completion`（请求位置随机，但尽量落在代码范围内）
  4. `shutdown`
- 断言：0 crashes。任何非条件异常（如 `json-error`、分析层 `car` 空列表等）均记为失败。

### 2.3 真实 Editor 连续编辑流模拟

**目标**：模拟"从合法代码开始，逐步写错、修正、再写错"的完整使用过程，覆盖错误状态累积路径。

**方法**：
- 以一份合法 fixture 代码为起点：
  1. `didOpen` 完整合法代码
  2. 连续发送 5–10 次 `didChange`，每次只做一个小修改（如删一个括号、多一个引号、打错一个标识符、粘贴半段宏）
  3. 每隔 2–3 次 `didChange` 插入一次 `hover` 或 `completion`
  4. 最后 `shutdown`
- `didChange` 使用全量替换（`text` 字段）以简化实现，不强制使用增量 `range` 模式。
- 这样可覆盖：前一次 `didChange` 引入的未闭合语法，后一次 `didChange` 让它变得更糟，而中间的 `hover` 请求恰好触发分析层在损坏的 AST 上工作。

## 3. 实施计划

| 阶段 | 任务 | 预计产出 |
|------|------|---------|
| 1 | 搭建多线程并发测试框架 + 基础 case | `tests/robustness-concurrent.sps`（或合并入现有文件） |
| 2 | 实现基于 fixture 的随机变异 fuzzer | `tests/robustness-editor-fuzz.sps` |
| 3 | 在连续编辑流中加入增量错误注入 | 扩展 `robustness-editor-fuzz.sps`，增加 `simulate-typing` 模式 |
| 4 | 跑完全部测试，确认 0 crash，纳入 `test.sh` | 更新 `test.sh`，移除 skip（如需要） |

## 4. 验收标准

- 多线程并发场景下，服务器完成 `shutdown` 且 `server-shutdown?` 为 `#t`。
- 对至少 3 个真实 fixture 各跑 30 轮随机变异，crash 数为 0。
- 连续编辑流（5–10 次 `didChange` + 穿插请求）跑 20 组，crash 数为 0。
- 所有新增测试文件纳入 `test.sh`，不再跳过。

## 5. 已知限制

- `protocol/request.sls` 的 `read-message` / `read-content` 缺乏输入校验，但本方案假设 LSP 客户端发送的是**合法 JSON-RPC**（仅内容恶意），因此不在这套测试中覆盖畸形协议头场景。
- `process-request` 在处理 `"exit"` 方法时会调用 `(exit ...)`，可能杀死测试进程。本方案的测试会话**不发送 `exit`**，以 `shutdown` 结束并检查 `server-shutdown?`。
