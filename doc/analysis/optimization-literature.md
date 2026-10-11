# 性能优化文献综述：从 profile 症状到文献解法

> 2026-10-11。动机：`macro-resolution-profile.md` §1.2–§1.5 的实测瓶颈——
> ① 引用查找 4.6 s/init（`(节点,符号) → 引用列表` 重复计算 53 万次）；
> ② 宏级联生成巨量代码被反复分析（run 13.7 s 嵌套累计）；
> ③ 冷启动 15 s vs LSP 客户端 30 s 超时、didChange 重分析。
> 本文把每个症状映射到文献/工业界的已知解法，标注"借什么、怎么借、别借什么"。

---

## 症状 ①：重复的引用/作用域查找（step 内部最大头）

**文献坐标：把 name resolution 当作可缓存的属性/查询。**

| 文献 | 核心思想 | 我们借什么 |
|---|---|---|
| Pugh & Teitelbaum, *Incremental Computation via Function Caching*, POPL 1989 | 纯函数结果按参数缓存（function caching / memoization 的经典奠基） | 我们版的最简形态：`find-available-references-for` 是（近似）纯查询，key=(node-identity eq?, symbol)，value=refs，一次分析趟内有效 |
| Hedin, *Reference Attribute Grammars*, Informatica 2000；Söderberg & Hedin, *Automated Selective Caching for RAGs*, SLE 2010；*A Comparative Study of Incremental AG Solutions to Name Resolution*, 2012 | 名称解析 = AST 节点上的**参数化引用属性**，按需求值 + 缓存 + 编辑后按动态依赖图增量失效（JastAdd 生产级实现） | 我们的 (node, symbol)→refs 缓存就是手写的参数化 reference attribute。RAG 文献补全我们缺的另一半：**失效边界**——属性依赖图决定编辑后哪些缓存失效；我们的简化版 = 文档级失效（didChange 清空该文档缓存），粗但 sound |
| Liu, Stoller & Teitelbaum, *Static Caching for Incremental Computation*, TOPLAS 1998 | 不动点计算的静态缓存：缓存粒度选择（全缓存 vs 选择性缓存）的工程权衡 | 警示：全缓存引用表内存翻倍；选择性缓存（只缓存叶子符号解析、复合头符号解析）可能够拿 80% 收益 |
| Acar et al., *Adaptive Functional Programming*, POPL 2002（self-adjusting computation 一脉） | 把程序写成可由变更传播自动修复的计算 | 理论背景；不必整体采用 |
| Hammer et al., *Adapton*, PLDI 2014；*Incremental Computation with Names*, OOPSLA 2015 | 按需（demand-driven）+ **名义 memoization**（用 name 标识缓存槽，跨顺序重用） | "names" 概念对应我们的 (node, symbol) key；demand-driven 对应症状②。PLDI'14 的 DCG（demanded computation graph）是 salsa 的理论祖先 |
| **salsa / rust-analyzer**（matklad；*Durable Incrementality* 博文 2023；rust-analyzer dev/architecture） | 一切语义计算 = 纯查询 K→V，memoize；**revision + red-green 验证**决定复用；durability firewall（库代码编辑不使工作区派生数据失效） | 工业界证明：name resolution、宏展开、类型推导全部查询化后毫秒级响应。我们的中期目标形态：**step 的产物按查询缓存，didChange 只失效受影响文档的查询槽**——salsa 的 per-document 简化版（我们已有 per-document 分析隔离，天然是粗粒度 salsa） |

**落地建议（按顺序）**：
1. 一趟一清的分析内缓存（纯 memo，无失效逻辑）——直接吃掉 4.6 s 的大头，1-2 天工作量；
2. 与 workspace cache（fasl）集成时注意：缓存的引用列表是 mutable record 列表，memo 表存引用列表**快照或 id**，防别名 bug；
3. 长期再考虑 didChange 细粒度失效（RAG 动态依赖图或 salsa 式 revision）。

> **2026-10-11 实验修正（节点级缓存被证伪）**：按上述思路实现了全链签名验证缓存
> （reference.sls，(node, identifier)→refs + 父链身份签名，pass 级 parameter 生命周期，
> 零写入方改动；安全性前提"所有列表更新均为函数式"已 grep 验证），实测 **命中率 0.037%**
> （532,722 次查找仅 197 次命中，99.96% 签名失效），墙钟不降反升（签名构建的分配使
> GC 1.4s→2.3s），已完整撤回。根因：级联期间 back-propagation 持续重建节点链上的
> import/excluded/export 列表，节点级"链不变"条件在级联主导的流量下几乎永不成立。
> **结论：RAG/salsa 式节点属性缓存假设属性在计算间稳定，而 route-2 级联把节点列表
> churn 到缓存失效——文献路线的正确落点是 demand（症状②的按需级联），不是 cache。**
> 若将来重启缓存方向，唯一可行形态是"级联平息后的稳态缓存"（如按需级联落地后，
> 对已展开调用点的重复查询），且需先量化级联后段的链稳定性。

---

## 症状 ②：宏级联生成巨量代码被反复分析

**文献坐标：demand-driven——不分析没被问到的；以及把绑定解析从展开中解耦。**

| 文献 | 核心思想 | 我们借什么 |
|---|---|---|
| Heintze & Tardieu, *Demand-Driven Pointer Analysis*, PLDI 2001；Sridharan et al., OOPSLA 2005；Boomerang, ECOOP 2016 | 全程序指针分析太贵 → 只从查询点向后求解 | 方法论先例：IDE 场景天然是查询驱动的 |
| **Stein, Chang & Sridharan, *Demanded Abstract Interpretation*, PLDI 2021** | 把**抽象解释器**（就是我们 step 这类）的语法+分析状态提升为图：编辑时 eager 标脏、查询时 lazy 重算；95% 查询 <1.2 s，与批量分析结果一致（from-scratch consistency 有证明） | **与我们 situation 字面最像的一篇**。对应做法：match 调用点的 route-2 级联不在 init 时全跑，而是 hover/goto 落到某个 body 标识符时才按需展开该调用点，结果按 (宏, datum) memo（模板缓存已经是这个 memo 的雏形）。unused-imports 诊断需要全文件绑定 → 打开的文件仍 eager，其余 lazy |
| rust-analyzer 实践 | 宏按调用点 lazy 展开（salsa 查询）；ItemTree 摘要"对函数体修改稳定"——**签名/结构级摘要作为防火墙**，体编辑不失效全局派生数据 | 两个直接可抄的点：(a) 按需展开；(b) 让 import/define 级别的摘要与体分析分离，压 didChange 成本 |
| Flatt, *Binding as Sets of Scopes*, POPL 2016 | 绑定不靠展开历史重命名，而靠标识符携带 scope 集合、解析=子集检查——**绑定解析与宏展开解耦** | 终极架构方向：给 index-node 标识符挂 scope 集，让绑定解析像 Chez 展开器一样独立于"生成代码再分析"。工程量大，但是文献里"正确做法"；也直接回答"为什么我们的级联必须存在" |
| Ullrich & de Moura (Lean 4 hygiene implementation, 2020) 对 sets-of-scopes 的实现注记 | naive sets-of-scopes 可能二次方；需要 lazy scope propagation | 警示：若走 scope-set 路线，先读这节的工程教训 |
| Flatt, *Composable and Compilable Macros*, ICFP 2002 | 相位分离（phase separation） | 我们已在用（expand-time vs runtime 环境），文献背书 |
| Pugh-Teitelbaum 1989 的 structural memoization（hash-consing） | 相同子结构物理共享，memo 按结构 key 命中 | 级联内 DAG 共享未利用的对策之一：hash-cons 生成代码子树。比 shape 归一模板（P4 撤回的思路）更通用但侵入更大 |
| Szabó, Erdweg & Voelter, *IncA*（ASE 2016 / OOPSLA 2018 / PLDI 2021） | 把程序分析写成 Datalog+格，获得免费的增量求值 | 备选架构（重写式），仅当查询化改造走到尽头时再考虑，不作为第一步 |

**落地建议**：
1. 短期：**按需级联**——打开文件的 match 调用点仍 eager（诊断需要），其余文件调用点 lazy（首个 hover/goto 触发 + 全结果 memo）。预期把 init 的级联份额从 ~7 s 压到打开文件的量级；
2. 中期：shape-归一模板（相同形状的调用点共享推导结果——名义 memoization 的手工版）；
3. 长期：scope-set 化（Flatt 2016），从根上消除"生成代码再分析"。

> **2026-10-11 实验修正（奖池远小于嵌套口径外推）**：router 加 lazy 开关实测天花板——
> 68 个 match 调用点全部延迟后 init 14.16 s vs eager ~15.6 s（**省 ~1.4 s / 9%**），事后补跑全部
> 级联仅 65 ms（首个冷级联 ~40 ms，其余靠模板 replay 亚毫秒）。两个连带修正：
> ① §1.3/§1.5 里"match 家族规则体 10.2 s（嵌套累计）"曾暗示级联是大头——**嵌套累计口径严重高估
> 了级联份额；53 万次节点访问的主体是普通 Scheme 代码的常规分析**；
> ② 1.4 s 的奖池配不上生产化成本（protocol 全 API 触发 + didOpen 强制 + 诊断顺序，约半天 +
> 协议语义风险），**暂不实施**，已完整撤回。若未来重启：private-init-references 内
> step 与 check-unused-imports 之间按"是否 didOpen 路径"门控补跑即可，骨架保留在 git 历史。

---

## 症状 ③：冷启动与增量更新（LSP 交互性）

**文献坐标：预计算索引 + 细粒度失效。**

| 文献/系统 | 核心思想 | 我们借什么 |
|---|---|---|
| **Kythe**（Google）、**LSIF/SCIP**（Sourcegraph）、**clangd index/dex** | 跨文件交叉引用预计算成索引，当前文件 AST + 全局索引分离；查询时无需重新分析全仓 | 我们的 workspace fasl 缓存就是该模式的实现（命中时 ~2 s）。改进方向不在"要不要索引"而在**索引的新鲜度与一致性检查成本**（现 mtime 一致性检查已是该文献方向的工程实践） |
| rust-analyzer：*from-scratch parsing is fast enough*（dev/syntax） | 解析不值得增量化；**增量性全部放在查询图** | 我们的 tokenizer 重建整文档（0.27 s/834 文件）完全不用动——文献背书 |
| salsa 的 durability firewall | 输入分耐久级：库/依赖级输入永不因工作区编辑失效 | 若做细粒度 didChange 失效：`.akku/lib` 依赖文档的派生数据标记为高耐久，编辑工作区文件时整段跳过验证 |
| Adapton / DAI | 变更传播只重算被 demand 的脏节点 | 同上，长期形态 |

**落地建议**：短期靠现有 fasl 缓存 + 把症状①的 memo 和症状②的 lazy 级联落地，冷启动即达标（15 s → 预期 6-8 s，缓存命中 2 s）；细粒度 didChange 失效（salsa 式）作为独立立项，不在本轮。

---

## 不借什么（同样重要）

- **不整体重写为查询系统/salsa 风格**——Chez Scheme 无现成框架，自研 dependency-tracking 引擎成本远超收益；per-document 隔离已是 80% 答案。
- **不做增量解析/tree-sitter 化**——解析实测只占 4%（§1.4），rust-analyzer 文献同样结论。
- **不动 steer / syntax-rules 机器**——§1.3：分派 2 µs/访问、generator 149 ms/全程，无优化空间。
- **不急于 sets-of-scopes 重写**——方向正确但工程量以月计，先把 memo + lazy 两个几天的改动吃掉。

## 推荐执行序列（两次实验后修正）

两个文献级杠杆均已实测：**引用缓存证伪**（0.037% 命中）、**按需级联奖池仅 ~1.4 s**（暂缓）。
剩余的大头全部是 mundane 工程热点（无特定文献，直接小剖+优化）：

| 步骤 | 内容 | 实测依据（§1.3/§1.5/§1.4） | 预期收益 |
|---|---|---|---|
| 1 | define-process 内部剖析优化（135 µs × 7,830 次 = 1.0 s） | 规则体直方图最大单项 | -0.5~0.8 s |
| 2 | library-import / import 规则体优化（0.8 + 0.5 s） | 同上 | -0.5~0.8 s |
| 3 | unused-imports（2.1 s）+ file-linkage（1.7 s）+ excluded-refs（0.8 s）阶段级优化 | §1.4 阶段分解 | -2~3 s |
| 4 | fasl 缓存一致性检查加固 | Kythe/LSIF 模式 | 首跑体验（已有 ~2 s 命中） |
| 5（立项，暂缓） | 按需级联（奖池 ~1.4 s）；didChange 细粒度失效 / scope-set 化 | 本文实验修正 | 视优先级重启 |
