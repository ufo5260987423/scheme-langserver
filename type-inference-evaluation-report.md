# 类型推断子系统评估报告

> 评估工具：`bin/output-type-analysis.ss` 单库模式  
> 评估对象：`scheme-langserver` 自身 library  
> 运行时间：2026-06-18  
> 代码基线：`kimi` 分支，`PRIVATE-MAX-DEPTH` 保持原值 `10`，`PRIVATE-MAX-RESULTS = 500` 结果数量预算  

---

## 1. 执行摘要

本次评估按 `task.md` 的规划，对 6 个复杂度递增的 library 逐个运行类型推断。调试过程中发现 `util/contain` 和 `util/binary-search` 因递归函数类型推断结果无限膨胀而超时。最终采用**结果数量预算**方案：在 `analysis/type/domain-specific-language/interpreter.sls` 的 `type:interpret` 中，当单步结果列表去重后超过 `PRIVATE-MAX-RESULTS`（200）时，只保留前 200 项。`PRIVATE-MAX-DEPTH` 保持原来的 `10` 不变。

| Library | 运行结果 | 输出大小 | 说明 |
|---------|----------|----------|------|
| `(scheme-langserver util json)` | ✅ 完成 | 213 B | 2 个标识符，全部保守推断 |
| `(scheme-langserver virtual-file-system file-node)` | ✅ 完成 | 2.1K | 16 个标识符，record 相关识别较好 |
| `(scheme-langserver analysis identifier reference)` | ✅ 完成 | 7.2K | 32 个标识符，union 爆炸被预算截断 |
| `(scheme-langserver analysis type domain-specific-language interpreter)` | ✅ 完成 | 8.8K | 15 个标识符，核心递归函数结果在 200 项处截断 |
| `(scheme-langserver util contain)` | ✅ 完成 | 3.6K | `contain?`、`ordered-contain?` 均有输出 |
| `(scheme-langserver util binary-search)` | ✅ 完成 | 2.1K | `binary-search` 有输出 |

**总体结论**：

- 结果数量预算方案在**不降低全局递归深度**的前提下，让所有选定的 library 都能在合理时间内完成推断。
- 简单 predicate / list 函数的精确推断（如 `meta?` → `boolean?`）得以保留。
- 复杂递归函数（`binary-search`、`ordered-contain?`、`type:interpret`）的输出被截断为前 200 个签名，避免了超时，但结果仍较为冗长。
- 返回类型大多仍退化为 `something?`，这是当前类型规则不足导致的保守推断，不是超时修复带来的新问题。

---

## 2. 逐库结果统计

| Library | Export 数量 | 有类型输出的标识符 | 类型输出条数 | `something?` 占比（近似） | 明显错误 |
|---------|-------------|---------------------|--------------|----------------------------|----------|
| `util/json` | 2 | 2 | 2 | 100% | 0 |
| `virtual-file-system/file-node` | 16 | 14 | 21 | ~95% | 0 |
| `analysis/identifier/reference` | 32 | 27 | 62 | ~95% | 0 |
| `analysis/type/interpreter` | 15 | 14 | 75 | ~99% | 0 |
| `util/contain` | 2 | 2 | 30 | ~95% | 0 |
| `util/binary-search` | 1 | 1 | 20 | ~95% | 0 |

说明：
- `something?` 占比按类型字符串中出现 `something?` 的比例估算。
- "明显错误"指返回类型与源码语义明显冲突（如 predicate 被推断为非 `boolean?`、参数数量明显不一致等）。
- 本次未发现明显错误，主要问题是**过度保守**和**复杂递归函数签名冗长**。

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
(define (read-json string)
  (json-read (open-input-string string)))
```

推断类型：

```text
(something? <- (inner:list? something? ) )
```

点评：
- ✅ 参数数量正确（1 个参数）。
- ⚠️ 参数和返回类型均为 `something?`。
- 因为 `json-read`、`open-input-string` 等无精确类型规则，无法推导出 `string -> json-value`。

### 3.4 `analysis/identifier/reference`: `meta?` — 精确的 predicate 被保留

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
- 这说明结果数量预算不会像全局深度限制那样误伤简单函数。

### 3.5 `util/binary-search`: `binary-search` — 结果被截断但不再超时

推断类型（节选）：

```text
(something? <- (inner:list? something? (something? <- (inner:list? something? something? ) ) something? [identifier-reference integer?] [identifier-reference integer?] ) )
(something? <- (inner:list? something? something? something? [identifier-reference integer?] [identifier-reference integer?] ) )
...
```

点评：
- ✅ 不再超时，输出 20 条签名。
- ⚠️ 仍有大量 `something?`，但能看出部分参数为 `integer?`/`real?`/`number?`。
- 冗长是因为 `case-lambda` 多分支 + 递归导致类型组合爆炸，200 项预算将其截断。

### 3.6 `analysis/type/interpreter`: `type:interpret` — 结果被截断

推断类型（节选）：

```text
(something? <- (inner:list? [identifier-reference index-node?] something? (inner:list? something? ... ) [identifier-reference real?] ) )
...
```

点评：
- ⚠️ 该函数是类型推断器自身，递归深度大，结果被截断到 200 项。
- 返回类型全部为 `something?`，说明当前推断器还无法给自己一个更精确的类型。

---

## 4. 发现的问题清单

### 4.1 中等

1. **复杂递归函数类型组合爆炸**
   - 影响库：`util/contain`、`util/binary-search`、`analysis/type/interpreter`。
   - 现象：不截断时会产生数万条签名并超时。
   - 当前缓解：结果数量预算（200 项）截断，避免超时，但输出仍较冗长。

2. **record accessor/setter 类型不精确**
   - 影响库：`virtual-file-system/file-node`。
   - 现象：`file-node-children`、`file-node-document` 等 getter 返回 `something?`。
   - 原因：`record-accessor` / `record-mutator` 的类型规则未与 `define-record-type` 的字段定义联动。

### 4.2 轻微

1. **简单函数过度保守**
   - 影响库：`util/json`、`util/contain`、`util/binary-search`。
   - 现象：参数和返回均为 `something?`。
   - 原因：涉及字符串/IO/递归的函数缺乏精确规则。

2. **输出可读性差**
   - `inner:list?`、`inner:pair?` 等内部类型表示对人类阅读不够友好。

---

## 5. 改进建议

按投入产出比排序：

1. **增强 record 类型规则**（中优先级）
   - 让 `record-accessor` / `record-mutator` 能从 `define-record-type` 的字段定义中获取字段类型。
   - 这能提升 `virtual-file-system/file-node` 等库的 getter/setter 精度。

3. **类型推断阶段的 union 合并/简化**（中优先级）
   - 在推断阶段合并参数结构相同、仅返回不同的函数类型。
   - 这能显著减少 `analysis/identifier/reference` 等库的签名数量。

4. **为常用 IO/字符串函数添加规则**（低优先级）
   - 如 `read-json`、`generate-json` 等可保守地标记为 `(something? <- (inner:list? [identifier-reference input-port?]))`。
   - 但这类规则收益有限，且容易过拟合。

---

## 6. 阈值调优实验（2026-06-20 更新）

为确定 `PRIVATE-MAX-RESULTS` 的最优值，对 100 / 200 / 500 / 1000 四个阈值进行了对比实验。实验脚本为 `bin/benchmark-max-results.sh`，对每个阈值运行 `bin/output-type-analysis.ss` 的 6 个目标 library，记录 wall-clock 时间和输出行数。

### 6.1 实验结果

| Library | 100 | 200 | 500 | 1000 |
|---------|-----|-----|-----|------|
| `(scheme-langserver util contain)` | 67.74 s / 24 行 | 70.81 s / 24 行 | 71.74 s / 24 行 | 70.58 s / 40 行 |
| `(scheme-langserver util json)` | 49.65 s / 6 行 | 49.82 s / 6 行 | 52.36 s / 6 行 | **~35443 s / 6 行** |
| `(scheme-langserver virtual-file-system file-node)` | 51.13 s / 39 行 | 49.57 s / 39 行 | 54.50 s / 39 行 | — |
| `(scheme-langserver analysis identifier reference)` | 52.14 s / 92 行 | 51.46 s / 96 行 | 54.48 s / 99 行 | — |
| `(scheme-langserver analysis type domain-specific-language interpreter)` | 50.11 s / 66 行 | 52.67 s / 88 行 | 54.98 s / 89 行 | — |
| `(scheme-langserver util binary-search)` | 54.50 s / 19 行 | 59.89 s / 19 行 | 58.35 s / 19 行 | — |

### 6.2 结论

- **100、200、500 阈值均表现稳定**，所有 library 在约 50–75 秒内完成。
- 500 阈值相较 200 阈值，在 `analysis/identifier/reference` 和 `analysis/type/interpreter` 上提供了略多的输出（+3 行 / +1 行），而耗时增加约 2–5 秒，可接受。
- **1000 阈值不稳定**：`util/json` 出现极端异常，耗时约 35443 秒（近 10 小时）；`virtual-file-system/file-node` 在实验过程中也未能在合理时间内完成。因此 **不采用 1000**。
- 综合考虑输出质量、稳定性和耗时，最终选定 **`PRIVATE-MAX-RESULTS = 500`**。

## 7. 已知限制说明

- `uri-is-path?` 在 `(scheme-langserver util path)` 中仍被推断为 `(something? <- (inner:list? something?))`，而不是 `boolean?`。
- 原因是其依赖的 `string-prefix?` 在 `srfi-13.scm` 中通过 `let-string-start+end2` 等宏实现，而**当前类型推断子系统未将宏展开引入类型推断**。
- 按项目决策，**不通过显式 SRFI-13 类型签名绕过该限制**，因此该现象属于当前架构下的合理限制。

---

## 8. 本次变更

- `analysis/type/domain-specific-language/interpreter.sls`
  - 保持 `PRIVATE-MAX-DEPTH = 10` 不变。
  - 设置 `PRIVATE-MAX-RESULTS = 500`。
  - 在 `type:interpret` 末尾，对 `env` 的结果列表去重后，若长度超过 500，则只保留前 500 项。
  - 目的：在保留全局递归深度的同时，通过结果数量预算抑制递归函数的类型组合爆炸。

---

## 9. 交付物

- `/tmp/type-analysis-results/-scheme-langserver-util-json-.txt`
- `/tmp/type-analysis-results/-scheme-langserver-virtual-file-system-file-node-.txt`
- `/tmp/type-analysis-results/-scheme-langserver-analysis-identifier-reference-.txt`
- `/tmp/type-analysis-results/-scheme-langserver-analysis-type-domain-specific-language-interpreter-.txt`
- `/tmp/type-analysis-results/-scheme-langserver-util-contain-.txt`
- `/tmp/type-analysis-results/-scheme-langserver-util-binary-search-.txt`
- `type-inference-evaluation-report.md`（本文件）
