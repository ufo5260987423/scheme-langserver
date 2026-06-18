# 类型推断子系统评估报告

> 评估工具：`bin/output-type-analysis.ss` 单库模式  
> 评估对象：`scheme-langserver` 自身 library  
> 运行时间：2026-06-18  
> 代码基线：commit `22f14f8` 之后的状态

---

## 1. 执行摘要

本次评估按 `task.md` 的规划，对 6 个复杂度递增的 library 运行了类型推断。运行前已先修复了若干会严重影响输出正确性的基础 bug（详见 `task.md` 第 9 节）。

| Library | 运行结果 | 耗时 | 输出大小 |
|---------|----------|------|----------|
| `(scheme-langserver util contain)` | ⏱️ 超时（300s） | >300s | 0 B |
| `(scheme-langserver util json)` | ✅ 完成 | ~65s | 213 B |
| `(scheme-langserver virtual-file-system file-node)` | ✅ 完成 | ~62s | 2.1K |
| `(scheme-langserver analysis identifier reference)` | ✅ 完成 | ~96s | 7.6K |
| `(scheme-langserver analysis type domain-specific-language interpreter)` | ✅ 完成 | ~118s | 21K |
| `(scheme-langserver util binary-search)` | ⏱️ 超时（300s） | >300s | 0 B |

**总体结论**：

- 简单库（`util/json`、`virtual-file-system/file-node`）能给出函数签名，但返回类型大多退化为 `something?`。
- 复杂库（`analysis/identifier/reference`、`analysis/type/interpreter`）出现严重的 union 爆炸，一个标识符可能输出十几到几十种签名，可读性较差。
- 两个递归/边界库（`util/contain`、`util/binary-search`）在 300s 内无法完成，说明类型推断对递归函数仍不稳定。
- 修复后的 `output-type-analysis.ss` 已能正确合并 `something?` union，不再出现 `assq-ref` 被误推断为返回 `boolean?` 的情况。

---

## 2. 逐库结果统计

| Library | Export 数量 | 有类型输出的标识符 | 类型输出条数 | `something?` 占比（近似） | 明显错误 |
|---------|-------------|---------------------|--------------|----------------------------|----------|
| `util/json` | 2 | 2 | 2 | 100% | 0 |
| `virtual-file-system/file-node` | 16 | 14 | 21 | ~95% | 0 |
| `analysis/identifier/reference` | 32 | 27 | 65 | ~95% | 0 |
| `analysis/type/interpreter` | 15 | 14 | 156 | ~99% | 0 |
| `util/contain` | — | — | — | — | 超时 |
| `util/binary-search` | — | — | — | — | 超时 |

说明：
- `something?` 占比按类型字符串中出现 `something?` 的比例估算。
- "明显错误"指返回类型与源码语义明显冲突（如 predicate 被推断为非 `boolean?`、参数数量明显不一致等）。
- 本次未发现明显错误，主要问题是**过度保守**和**union 爆炸**。

---

## 3. 典型案例分析

### 3.1 `virtual-file-system/file-node`: `make-file-node` — 较好案例

源码：

```scheme
(define-record-type file-node ...)
```

推断类型：

```text
([identifier-reference file-node?] <- (inner:list? something? ... ) )
```

点评：
- ✅ 正确识别为函数。
- ✅ 返回类型识别为 `file-node?`。
- ⚠️ 参数类型为 `something? ...`，没有精确到构造 record 所需的各个字段类型。这是可以接受的，因为 record constructor 的字段类型需要额外规则支持。

### 3.2 `virtual-file-system/file-node`: `file-node-children` — 过度保守

源码：

```scheme
(define file-node-children
  (record-accessor file-node-type 4))
```

推断类型：

```text
(something? <- (inner:list? [identifier-reference file-node?] ) )
```

点评：
- ✅ 参数识别为 `file-node?`。
- ⚠️ 返回类型应为 `(inner:list? [identifier-reference file-node?])` 或类似列表类型，但推断为 `something?`。
- 原因是 `record-accessor` 的类型规则不够精确，无法从 record 定义推导出具体字段类型。

### 3.3 `util/json`: `read-json` — 完全保守

源码：

```scheme
(define (read-json port)
  ...)
```

推断类型：

```text
(something? <- (inner:list? something? ) )
```

点评：
- ✅ 参数数量正确（1 个参数）。
- ⚠️ 参数和返回类型均为 `something?`。
- 说明对于涉及大量字符串解析/递归的函数，当前推断器无法给出更具体的类型。

### 3.4 `analysis/identifier/reference`: `meta?` — 精确的 predicate

源码：

```scheme
(define (meta? identifier)
  (not (null? (identifier-reference-top-environment identifier))))
```

推断类型：

```text
([identifier-reference boolean?] <- (inner:list? [identifier-reference identifier-reference?] ) )
```

点评：
- ✅ 正确识别为 predicate，返回 `boolean?`。
- ✅ 参数类型识别为 `identifier-reference?`。
- 这是当前推断器表现较好的典型案例：源码结构简单，不依赖宏，返回类型明确。

### 3.5 `analysis/type/interpreter`: `type:interpret` — union 爆炸

推断类型（节选）：

```text
(something? <- (inner:list? [identifier-reference identifier-reference?] something? (inner:list? something? ... ) [identifier-reference real?] ) )
(something? <- (inner:list? [identifier-reference identifier-reference?] [identifier-reference type:environment?] something? [identifier-reference real?] ) )
(something? <- (inner:list? (inner:pair? something? something? ) [identifier-reference type:environment?] (something? <- (inner:list? ) ) [identifier-reference real?] ) )
... 共 39 条
```

点评：
- ⚠️ `type:interpret` 一个标识符输出了 39 种签名，且返回类型全部为 `something?`。
- 这是当前类型推断对复杂递归/多态函数处理不佳的典型表现：无法合并分支，导致 union 爆炸。

---

## 4. 发现的问题清单

### 4.1 严重

暂无。修复 Bug 1 后，未发现返回类型与源码语义明显冲突的案例。

### 4.2 中等

1. **递归函数推断不稳定**
   - 影响库：`util/contain`、`util/binary-search`。
   - 现象：300s 超时，无输出。
   - 原因：`contain?`、`ordered-contain?`、`binary-search` 等函数具有递归结构，类型推断可能无法终止或极其缓慢。

2. **复杂函数 union 爆炸**
   - 影响库：`analysis/type/interpreter`、`analysis/identifier/reference`。
   - 现象：一个标识符输出十几到几十种签名，难以阅读。
   - 原因：条件分支、case-lambda、多态参数等场景下，推断器没有有效合并等价的类型结果。

3. **record accessor/setter 类型不精确**
   - 影响库：`virtual-file-system/file-node`。
   - 现象：`file-node-children`、`file-node-document` 等 getter 返回 `something?`。
   - 原因：`record-accessor` / `record-mutator` 的类型规则未与 `define-record-type` 的字段定义联动。

### 4.3 轻微

1. **简单函数过度保守**
   - 影响库：`util/json`。
   - 现象：`read-json`、`generate-json` 的参数和返回均为 `something?`。
   - 原因：涉及字符串/IO 的函数缺乏精确规则。

2. **输出可读性差**
   - `inner:list?`、`inner:pair?` 等内部类型表示对人类阅读不够友好。

---

## 5. 改进建议

按投入产出比排序：

1. **稳定递归函数推断**（高优先级）
   - 为 `type:interpret` / `type:recursive-interpret-result-list` 增加递归深度限制或循环检测。
   - 对无法终止的递归函数，优雅地返回 `something?` 而不是超时。

2. **合并等价类型结果**（高优先级）
   - 在类型推断阶段（而不仅是输出工具）对 union 进行简化：
     - 若集合中包含 `something?`，丢弃其他 top-type 子类型。
     - 合并参数结构相同、仅返回不同的函数类型。
   - 这能显著缓解 `analysis/type/interpreter` 等库的 union 爆炸。

3. **增强 record 类型规则**（中优先级）
   - 让 `record-accessor` / `record-mutator` 能从 `define-record-type` 的字段定义中获取字段类型。
   - 这能提升 `virtual-file-system/file-node` 等库的 getter/setter 精度。

4. **为常用 IO/字符串函数添加规则**（低优先级）
   - 如 `read-json`、`generate-json` 等可保守地标记为 `(something? <- (inner:list? [identifier-reference input-port?]))`。
   - 但这类规则收益有限，且容易过拟合。

---

## 6. 已知限制说明

- `uri-is-path?` 在 `(scheme-langserver util path)` 中仍被推断为 `(something? <- (inner:list? something?))`，而不是 `boolean?`。
- 原因是其依赖的 `string-prefix?` 在 `srfi-13.scm` 中通过 `let-string-start+end2` 等宏实现，而**当前类型推断子系统未将宏展开引入类型推断**。
- 按项目决策，**不通过显式 SRFI-13 类型签名绕过该限制**，因此该现象属于当前架构下的合理限制。

---

## 7. 交付物

- `/tmp/type-analysis-results/scheme-langserver-util-json.txt`
- `/tmp/type-analysis-results/scheme-langserver-virtual-file-system-file-node.txt`
- `/tmp/type-analysis-results/scheme-langserver-analysis-identifier-reference.txt`
- `/tmp/type-analysis-results/scheme-langserver-analysis-type-domain-specific-language-interpreter.txt`
- `/tmp/type-analysis-results/scheme-langserver-util-contain.txt`（空，超时）
- `/tmp/type-analysis-results/scheme-langserver-util-binary-search.txt`（空，超时）
- `type-inference-evaluation-report.md`（本文件）
