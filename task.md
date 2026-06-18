# 任务：用 `output-type-analysis.ss` 验证类型推断正确性

## 1. 目标

使用 `bin/output-type-analysis.ss` 的单库模式，对 `scheme-langserver` 自身的若干 library 运行类型推断，检查输出结果是否符合源码实际类型，从而评估当前类型推断子系统的正确性、精确度和常见失效模式。

## 2. 测试库选取

按**复杂度递增**和**类型系统特性覆盖**两个维度，挑选 6 个 library：

| 序号 | Library | 选择理由 | 预期特点 |
|------|---------|----------|----------|
| 1 | `(scheme-langserver util contain)` | 纯函数、无递归、无 record | 类型应较准确，适合作为基线 |
| 2 | `(scheme-langserver util json)` | 字符串/列表操作、少量分支 | 测试 list/vector/string 推断 |
| 3 | `(scheme-langserver virtual-file-system file-node)` | 含 `define-record-type`、getter/setter | 测试 record 类型间接表示 |
| 4 | `(scheme-langserver analysis identifier reference)` | 含 records、多字段、导出过程 | 测试复杂 record + 过程签名 |
| 5 | `(scheme-langserver analysis type domain-specific-language interpreter)` | 类型系统自身 | 自举测试，观察递归/宏相关函数 |
| 6 | `(scheme-langserver util binary-search)` | 已知在类型推断中表现差（文档 §10） | 作为压力/边界案例 |

## 3. 执行步骤

### 3.1 环境准备

```bash
source .akku/bin/activate
rm -rf .akku/libobj/scheme-langserver
mkdir -p /tmp/type-analysis-results
```

### 3.2 单库运行

对每个选定的 library 执行：

```bash
scheme --script bin/output-type-analysis.ss \
  . \
  "(<library-name>)" \
  /tmp/type-analysis-results/<sanitized-library-name>.txt
```

例如：

```bash
scheme --script bin/output-type-analysis.ss \
  . \
  "(scheme-langserver util contain)" \
  /tmp/type-analysis-results/util-contain.txt
```

### 3.3 输出收集

所有结果保存到 `/tmp/type-analysis-results/`。同时记录：

- 每次运行的 wall-clock 时间
- 输出文件大小/行数
- 是否超时或报错

## 4. 审查标准

对每个 library 的每个 export 标识符，按以下维度判断：

| 维度 | 合理（✅） | 部分合理（⚠️） | 不合理（❌） | 无输出（➖） |
|------|------------|----------------|-------------|-------------|
| 是否有类型输出 | 有具体类型 | 有但含 `something?` | 有但明显错误 | 完全无输出 |
| 过程参数数量 | 与源码一致 | 部分一致 | 明显不一致 | — |
| 返回类型 | 与源码一致 | 过于保守 | 明显错误 | — |
| record 相关 | predicate/constructor/accessor 类型清晰 | 只有 `something?` | signature 明显错误 | — |

### 明显错误的例子

- 一个已知返回 `boolean?` 的 predicate 被推断为 `something?`
- 一个接受 2 个参数的函数被推断为 `(something? <- (inner:list? something?))`（1 个参数）
- 一个 record constructor 被推断为普通 `procedure` 而非返回 record predicate

## 5. 报告结构

最终报告将包含：

1. **执行摘要** — 运行时间、成功/失败的 library、总体结论。
2. **逐库结果表** — 每个 library 的 export 数量、有具体类型的数量、`something?` 占比、明显错误数量。
3. **典型案例分析** — 挑选 3–5 个代表性标识符，贴出源码与推断类型，逐条点评。
4. **发现的问题清单** — 按严重程度分类：
   - 严重：推断结果与源码语义冲突
   - 中等：结果过于保守但可用
   - 轻微：输出格式或细节问题
5. **改进建议** — 针对发现的问题，提出可落地的修复方向。

## 6. 风险与应对

| 风险 | 应对 |
|------|------|
| `binary-search` 等递归库运行极慢 | 设置 300 秒超时；若超时则记录并跳过后续递归压力库 |
| 某些 library 无 export（脚本文件） | 记录为 N/A，不纳入统计 |
| 输出量过大难以人工审查 | 先按 "有输出 / 全 something? / 无输出" 做粗筛，再重点抽查 |
| 缓存导致结果不反映最新代码 | 每次运行前清 `.akku/libobj/scheme-langserver` |

## 7. 时间预估

| 阶段 | 预估时间 |
|------|----------|
| 环境准备 + 试运行 1 个库 | 5–10 分钟 |
| 运行全部 6 个库 | 10–30 分钟（取决于 `binary-search` 等递归库） |
| 审查输出 + 整理报告 | 30–60 分钟 |
| **总计** | **约 1–1.5 小时** |

## 8. 交付物

- `/tmp/type-analysis-results/*.txt` — 原始推断输出
- `type-inference-evaluation-report.md` — 本次验证的总结报告

---

## 9. 进展更新与结论

> 更新日期：2026-06-18

### 9.1 已完成的修复

在正式执行 Step 3 前，已先修复了若干会严重影响输出正确性的基础 bug：

| 修复项 | 涉及文件 | 说明 |
|--------|----------|------|
| Bug 3 方案 A：Akku 文件过滤器 | `analysis/package-manager/akku.sls` | 移除错误的 `private:percent-decode`，使 percent-encoded 的 included `.scm` 文件（如 `%3a13/srfi-13.scm`）能正确进入 VFS。 |
| Bug 2：rest/dotted 参数 | `analysis/identifier/rules/define.sls`<br>`analysis/type/substitutions/rules/define.sls` | `define-process` 与类型规则均支持 `(define (f x . rest) ...)` 形式的函数。 |
| Bug 3：include 引用挂点 | `analysis/identifier/self-defined-rules/srfi/include-resolve.sls` | 被 include 文件的 references 现在挂到最近的 `(library ...)` / `(define-library ...)` 祖先节点，而不是 `SRFI-23-error->R6RS` 宏调用节点。 |
| Bug 1：输出工具 union 合并 | `bin/output-type-analysis.ss` | 当结果中存在 `something?` 时，合并更具体的 top-type 子类型，避免 `assq-ref` 等被错误显示为返回 `boolean?`。 |

相关测试已全部通过：

- `tests/analysis/identifier/rules/srfi/test-include-resolve.sps`
- `tests/analysis/type/substitutions/rules/test-define.sps`
- `tests/analysis/package-manager/test-akku.sps`
- `tests/virtual-file-system/test-vfs.sps`
- `tests/analysis/identifier/test-reference.sps`

### 9.2 关于 `uri-is-path?` 返回 `something?` 的结论

`uri-is-path?` 目前仍被推断为：

```text
(something? <- (inner:list? something? ) )
```

而不是 `boolean?`。经确认，这是**合理且可接受的当前限制**：

1. `uri-is-path?` 依赖 `(srfi :13 strings)` 的 `string-prefix?`；
2. `string-prefix?` 在 `srfi-13.scm` 中通过 `let-string-start+end2` 等宏实现；
3. **当前类型推断子系统未将宏展开引入类型推断**，因此无法从宏实现的 predicate 自动推导出 `boolean?`；
4. 这不同于 task.md 第 4 节列举的 "明显错误"：它不是推断器对普通源码分支的误推，而是宏抽象层造成的固有信息损失。

因此，**不会为 SRFI-13 添加显式类型签名**；`uri-is-path?` 返回 `something?` 视为当前架构下的已知限制，不在本次任务的可修复范围内。

### 9.3 下一步

执行 **Step 3**：对第 2 节指定的 6 个 library 运行 `bin/output-type-analysis.ss`，生成 `/tmp/type-analysis-results/*.txt`，并撰写 `type-inference-evaluation-report.md`。在审查时应把宏依赖导致的 `something?` 与真正的推断错误区分开。
