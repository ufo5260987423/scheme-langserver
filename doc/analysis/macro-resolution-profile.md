# 自动宏解析（路线二）match 宏展开性能 Profile

> 范围：一次完整 `match` 宏调用点（5 子句）经 `expansion-generator->rule` 驱动的端到端级联——generator → 模板实例化 → 重解析 → 配对 → step（抽象解释器分析生成代码）→ shallow-copy 回传，含 memory cap 15 内层宏的完整递归。
> 方法：对 `expansion-wrap.sls` 内部组件（generator 调用 / step / shallow-copy / store-template / flush-inserts / sync-to-parent）挂 `current-time` 计时器逐段累计（临时插桩，测量后已还原，不影响任何数字的代码路径）。真实 ufo-match 家族 6 宏，工作区为本项目自身（128 个 .sls）。
> 日期：2026-10-10（同日修订：修复了模板存储的 root-pair 门槛——见 §2 与 §3，端到端数字为修复前测量；修复后相同调用点的重复展开坍缩到亚毫秒级）。本文只描述当前代码的开销结构；更早的 profile 与优化迭代记录见 git 历史。

---

## 1. 端到端分解（每顶层 match 调用点 ~228 ms）

基准：连续 3 次全新调用点的 rule 调用，墙钟 683 ms（228 ms/调用点）。

| 组件 | ms/调用点 | 占比 | 调用次数/调用点 | 单次均值 |
|---|---|---|---|---|
| **step（抽象解释器对展开树的逐步分析，排他时间）** | **~156** | **~69%** | ~248（递归） | ~0.6 ms |
| generator（syntax-rules 机器：子句选择、dispatch-matcher 求 bindings、模板实例化、pretty-print + 重解析、配对） | ~51 | ~22% | ~300 | ~171 µs |
| shallow-copy（引用回传：export/import 双侧 + sync） | ~19 | ~8% | ~275 | ~70 µs |
| flush-inserts（ordered-reference-list 批量插入） | ~0.3 | <1% | ~118 | ~3 µs |
| store-template（模板捕获） | ~0 | <1% | ~275 | <5 µs |

step 列为排他时间（= 墙钟 − 上列其余组件）。其含嵌套累计为 2533 ms/调用点（嵌套深度 ~11 层），即每次级联 hop 的 step 内部又触发更深层宏的完整展开循环。

**大头是 step**。抽象解释器按节点 match-index-node 分派到各标识符规则、叶子符号经 reference 解析、route&add 调度内层宏——对生成代码的常规分析成本，与生成代码节点数成正比（match 家族生成代码相对手写代码膨胀约一个数量级）。

### 1.1 step 内部逐段分解（冷调用，插桩 abstract-interpreter.sls 实测）

step 在复合节点上的四个阶段（n=1144 复合节点访问/调用点）加叶子解析（n=1735）：

| step 子组件 | ms/调用点 | 占墙钟 | 内容 |
|---|---|---|---|
| **rules-run（排他）** | **~60** | **~40%** | 标识符绑定规则体执行（let/lambda/define 等的 rules/*.sls 处理）+ route&add 调度中非宏的部分；含嵌套累计 1510 ms（内层宏的完整展开循环嵌在里面） |
| rules-prep | ~21 | ~14% | 头部符号引用解析：`find-available-references-for` + `establish-available-rules-from` |
| leaf | ~20 | ~13% | 叶子符号解析：`find-available-references-for` + usage-count 记账 |
| post-rules | ~16 | ~10% | 规则的后置段（cddr f） |

引用解析（rules-prep + leaf）合计 ~27%，与 generator 的 ~16% 相当；规则体执行（rules-run 排他）是 step 里的最大单项。热调用（相同 datum 二次进入）整体走 replay：墙钟 ~1 ms，step 各组件零调用。

---

## 2. 级联形状与缓存现状

- **级联呈宽展形**：单次顶层调用产生 ~300 次 generator 调用、~248 次 step 递归（在 cap 15 截断前）。内层宏（match-next/match-one/…）的相同子树被反复展开、反复 step——DAG 共享未利用。
- **子句选择器缓存工作正常**：generator 均值 171 µs/次，说明 selector-cache（按 (宏, 调用点 datum) memoize 子句确认）在级联内命中良好，子句 eval 编译不构成热点。
- **跨调用点模板重放已修复并生效**（2026-10-10）：此前 `private:store-template!` 被 `root-pair` 门槛挡住（list 形模板实例化的展开根在 pairs 中无对应项），模板存不进去，相同 datum 每次全价 full-run。修复：存储放开门槛，无根对时以调用节点为锚点（所有 path 降级为 #f，重放时引用附着到调用节点——有界降级，引用不丢）。实测冷调用 ~83 ms，相同 datum 的热调用坍缩到亚毫秒。回归测试 `tests/analysis/identifier/test-expansion-template-replay.sps`（5 断言）。

---

## 3. 优化靶子优先级（按本 profile）

1. ~~展开级 memoize / replay 通路修复~~（2026-10-10 已修复，见 §2）：无根对形状的存储已放开，相同 (宏定义, 调用点 datum) 的级联直接重放。剩余空间：memoize 目前跳过 step 但 replay 仍重跑 generator（~171 µs/次）做路径重定位；以及首调（冷）成本。
2. **冷调用成本（~83 ms/调用点）**：级联首调仍需完整展开+step。方向：级联内 memoize（同一首调内相同子树的兄弟重复仍全价，见 §2 的宽展形状）或按需展开调度。
3. **step 内部（冷路径剩余主体，见 §1.1）**：规则体执行（rules-run 排他 ~40%）与引用解析（rules-prep + leaf ~27%）。方向：生成代码的节点数仍由级联形状决定，级联内 memoize（同一次冷调用内相同子树的兄弟重复仍全价）同时压缩两者；引用解析可考虑按调用点缓存。
4. **shallow-copy（8%）**：锚点去重已落地，剩余为线性复制成本，优先级下降。
5. **generator 内部细化**（22% 合计）：dispatch-matcher / 模板实例化 / 重解析各占其中若干，单独优化收益有限。

---

## 4. 测试盲区

级联正确性由 `test-match-cascade-auto-resolve.sps`（14 断言）等 9 个测试锚定；模板存储/重放通路由 `test-expansion-template-replay.sps`（5 断言：list 根模板入库、#f path 降级语义、二次调用引用不丢）覆盖。本 profile 覆盖性能结构。
