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

### 1.2 复测（2026-10-10 晚，f95d0b54 模板存储修复后；插桩已还原）

> 口径：3 个全新形状的真实 repo 调用点 datum（合成节点，ufo-match 文档上下文），每进程 init-workspace
> 后测量——init 自身的 eager 展开已把全部规范化内层形式（match-next/match-one/match-two 标准调用）
> 填入模板缓存，因此本次测量的是**顶层冷调用 + 内层热 replay** 的真实生产形态。计数器 ns 级。

| 调用点 | wall | full-run 次数 | replay 命中 | generator | shallow-copy | store |
|---|---|---|---|---|---|---|
| site1（2 子句） | 22 ms | 18 | 7 | 0 ms / 20 次 | 0 ms / 18 次 | 0 ms / 18 次 |
| site2（1 子句） | 13 ms | 12 | 0 | 0 ms / 13 次 | 0 ms / 12 次 | 0 ms / 12 次 |

**结构变化（对比 §1）**：f95d0b54 的模板无条件存储让内层宏全部走 replay（7 次 replay 总耗时 <0.5 ms），
generator 调用从 ~300 次降到 12-20 次且单次亚毫秒——**冷调用点的成本 100% 落在 step 上**，
generator/shallow-copy/store 在热路径上趋零。旧 §1 的 228ms 分解（step 69% / generator 22% /
shallow-copy 8%）是模板修复前的口径，保留作历史参照。

**step 内部四段（ns 插桩，run 为含嵌套级联的累计口径）**：

| 调用点 | rules-prep | rules-run | post-rules | leaf | n-composite | n-leaf |
|---|---|---|---|---|---|---|
| site1 | 2.4 ms | **170 ms** | 1.8 ms | 2.7 ms | 261 | 403 |
| site2 | 1.0 ms | **64 ms** | 2.3 ms | 0.9 ms | 180 | 256 |

**step 的开销花在哪（回答本轮复测的核心问题）**：

1. **rules-run（规则体执行）独占大头**：170ms/64ms（嵌套累计），其余三段合计仅 ~5-7ms。
   每次复合节点访问 prep≈9µs、post≈7µs、leaf≈6.7µs——引用解析（rules-prep + leaf）和
   后置段在 ns 口径下也只占 step 的 ~3%，**不是热点**。
2. rules-run 的主体又是**递归**：级联各层的 step 几乎都经规则体触发（step-incl 计数 172ms/18 次
   ≈ rules-run 总量），即"规则体执行 → route&add → 内层 full-run/replay → 内层 step"的嵌套循环；
   手写核心形规则（let/lambda 的参数收集、重复检查、绑定建立）摊销在 ~660 次节点访问上。
3. **优化含义**：模板修复后，进一步压缩冷调用成本的方向不再是 generator/shallow-copy，
   而是 ① 减少 step 访问的生成代码节点数（级联形状决定）② 手写规则体本身的常数
   （check-duplicate-identifiers/collect-parameter-pairs 每次调用成本）③ 引用解析按调用点缓存
   （当前每次 ~7-9µs × 660 次 ≈ 5ms/调用点，占 wall 的 ~20-25%，是 step 之外最大的单项）。

### 1.3 init 全口径分解 + 规则体直方图（2026-10-11；插桩已还原，回归 14/14、5/5 全绿）

> 口径：整个 `init-workspace`（本仓 834 文档，墙钟 22.7 s）。计数器 ns 级；规则体与 step 为含嵌套级联的
> 累计口径（内层 full-run 的 step 会再触发内层规则体，层层相含）。

**expansion 组件（全部 full-run 累计）**：

| 组件 | 总时间 | 调用次数 | 单次均值 |
|---|---|---|---|
| **step（inclusive）** | **9.87 s** | 673 | 14.7 ms |
| generator（syntax-rules 机器：子句选择+模板实例化+重解析） | 149 ms | 874 | 171 µs |
| shallow-copy（引用回传+sync） | 114 ms | 759 | 150 µs |
| store-template | 3 ms | 759 | 4 µs |
| replay | 0 | 0 | init 期间无重复 datum |

**step 内部四段（init 全口径）**：run=13.2 s / prep=2.3 s / leaf=2.6 s / post=0.9 s；
节点访问 **198,817 复合 + 329,545 叶子 ≈ 53 万次**。

**rules-run 规则体直方图（init 全口径，按累计时间排序，节选）**：

| 规则体 | 累计 | 次数 | 单次 |
|---|---|---|---|
| match-next | 3.80 s | 177 | 21.5 ms |
| match-one | 2.12 s | 598 | 3.5 ms |
| match-two | 1.50 s | 310 | 4.8 ms |
| match-check-ellipsis | 1.25 s | 93 | 13.5 ms |
| match | 1.20 s | 45 | 26.8 ms |
| define | 1.06 s | 7,830 | 135 µs |
| library | 0.81 s | 665 | 1.2 ms |
| import | 0.46 s | 782 | 589 µs |
| match-check-identifier | 0.32 s | 39 | 8.3 ms |
| **ufo-match 家族小计** | **≈10.2 s** | | 占 run 的 77% |
| let/lambda/let*/case-lambda 等核心形 | ≈0.02 s | ~11k 次 | **1-5 µs/次** |

**"规则体执行为何慢 / 是不是 ufo-match-steer 的问题"——实测结论：**

1. **不是 steer 慢。** generator（就是 steer 驱动的 syntax-rules 机器：match-check-ellipsis 子句
   选择、dispatch-matcher、模板实例化）全 init 仅 149 ms（171 µs/次）；连调用最频繁的核心形规则
   （let-process 5,064 次、lambda-process 4,590 次，自身都用 match-index-node 解析形参）也只要
   1-5 µs/次。steer 的匹配常数不是问题。
2. **慢的是被分析对象，不是分析器。** match 家族规则体的 10.2 s 几乎全是其内部 step 的嵌套累计
   （与 step-incl 9.87 s 高度重叠）——级联为每个顶层 match 调用点生成并逐步分析了数千个节点的
   展开代码；DAG 共享未利用，相同子树在兄弟分支被反复 step（§2 的宽展形状）。
3. **init 的真实瓶颈排序**：① step 对 53 万次生成代码节点访问（含级联嵌套重复）≈10 s；
   ② 每节点常数成本 prep+leaf ≈4.9 s（~15 µs/节点 × 33 万叶子 + 引用解析，其中
   find-available-references-for 按节点解析是主体）——**不依赖级联形状、可独立优化**
   （按调用点缓存引用解析）；③ 手写规则体（define/library/import，~2.3 s，真实排他成本）。

### 1.4 整程序视角：init 墙钟的阶段分解（2026-10-11；step 只是其中一个组件）

> 口径：整个 `init-workspace`（本仓 834 文档），workspace/abstract-interpreter 临时插桩（已还原），
> (time ...) 提供 GC 数据。这一节回答的问题：**跳出宏解析，全程序的时间花在哪**。

**修复前**（墙钟 21.0 s，GC 1.4 s，分配 10.3 GB）：

| 阶段 | ms | 占比 |
|---|---|---|
| **vfs parse+build** | **7,450** | **35%** |
| **step 总计** | **9,870** | **47%** |
| unused-imports 检查 | 2,130 | 10% |
| file-linkage 构建 | 1,860 | 9% |
| excluded-refs 处理 | 860 | 4% |
| unused-locals 检查 | 110 | 0.5% |
| library-node | 2 | ~0 |

vfs 细分（修复前）：read 140 / annotations(含 consume-sps-aux) 270 / index-node 190 / **make-document 7,000（94%！）** /
其余壳操作 ~0。make-document 的成本几乎全部在 `text->line-length-vector`：
**每行调用两次 srfi-13 `string-index`（~2 µs/char）+ quasiquote `,@` 拼接建表（O(行数²)）**，
单 40 KB 文件实测 81 ms；全仓累计 ~7 s。

**修复**（`virtual-file-system/document.sls` text->line-length-vector 改为单次 string-ref 扫描，
O(n) 且零分配；15 个边界用例含 \r\n/\n\r/孤立 \r 与旧实现对拍全等）：

| 指标 | 修复前 | 修复后 | 倍数 |
|---|---|---|---|
| vfs parse+build | 7,450 ms | **659 ms** | 11.3× |
| **init 墙钟** | **21.0 s** | **14.7 s** | **-30%** |

**修复后的全景**（墙钟 14.7 s）：step 9.1 s（62%，内部见 §1.3）、unused-imports 2.1 s（14%）、
file-linkage 1.7 s（12%）、excluded-refs 0.8 s（5%）、vfs 0.66 s（4%）。
**下一步优化排序**：① unused-imports / file-linkage / excluded-refs 三个后置阶段（合计 ~4.6 s，
step 之外的 max 单项）；② step 本身（§1.3 的引用解析缓存与级联形状）；
③ GC 1.4 s 与 10.3 GB 分配主要产自 step/级联，随 ② 一并下降。
**教训**：vfs 曾被认为是"解析"主项，但真正的成本藏在 make-document 的一个 O(n²)+慢原语
组合里——阶段级 profile（各项之和 ≈ 墙钟）是定位这类黑洞的唯一可靠手段。

### 1.5 step 内部深剖面（2026-10-11；插桩已还原，回归 14/14、5/5）

> 口径：整个 init（墙钟 15.8 s）。prep/leaf/dispatch 为排他叶测量；run/children-recursion 为含嵌套
> 级联的累计口径。比 §1.3 多拆三层：prep 拆 find-refs vs establish-rules；leaf 拆 find-refs vs
> usage 记账；children 递归单独计时（反推 step 自身分派开销）。

n = 198,784 复合访问 + 329,514 叶子访问（≈53 万）：

| step 内部组件 | 时间 | 单次 | 口径 |
|---|---|---|---|
| **prep-find-refs（头部符号引用查找）** | **2,067 ms** | 10 µs | 排他 |
| **leaf-find-refs（叶子符号引用查找）** | **2,509 ms** | 8 µs | 排他 |
| post-rules（后置段） | 891 ms | 4 µs | 排他 |
| **step 自身分派（match-index-node + let 开销）** | **337 ms** | 2 µs | 排他 |
| establish-rules（规则表构建） | 6.5 ms | ~0 | 排他 |
| usage-count 记账 | 0.5 ms | ~0 | 排他 |
| run（规则体执行） | 13,672 ms | — | 嵌套累计 |
| children-recursion（子节点递归） | 49,083 ms | — | 嵌套累计（深度 ~5 层） |

**结论（step 优化依据）**：

1. **引用查找是 step 内部唯一的大头**：prep-find + leaf-find = 4.6 s（占 step 墙钟的一半），
   每次 8-10 µs。这是与级联形状无关的纯查找成本——`find-available-references-for` 按
   (节点, 符号) 缓存后，step 墙钟可直接砍 ~4 s 级。**#1 优化靶**。
2. **steer/分派假设彻底排除**：step 自身 match-index-node 分派 2 µs/访问、establish-rules
   ~0、usage 记账 ~0——分派层无任何优化空间。
3. 规则体（run 的排他部分）≈ define/library/import 的 ~2.3 s（§1.3 直方图），其中 define 单次
   135 µs × 7,830 次是最大单项——值得单独剖（check-duplicate-identifiers / 引用建立）。
4. GC 1.5 s / 10.4 GB 分配分布在级联分配上，随引用缓存与级联压缩联动下降。

### 1.6 2026-10-11 追加测量（b2fc5b41 之后的 9.6 s 基线）

**引用表 merge 成本坐实**：`append-references-into-ordered-references-for` 全程 **1,851 ms /
51,600 次调用 / 36 µs 每次**——step 5.1 s 的 ~36%。每次插入走 `merge-ordered-lists` O(表长)，
是 define-process 135 µs 单次成本的主构成。根治需动表表示（差分+惰性排序视图 / 分桶），
风险中高，暂记为候选；调用端批量化（同一 pass 内小批量合并）是较温和的变体。

**threaded init 挂死/崩溃（已解决，2026-10-11）**：`(init-workspace path #t)` 在裸 bench 下
要么 20 分钟无进展，要么 6 秒 `nonrecoverable invalid memory reference`（core dump）。
根因：`b2fc5b41` 引入的 `sorted-list-vector-cache` 是**全局 weak-eq 哈希表**，由
threaded-map 工作线程并发读写 → 桶链表损坏——与当年 `private:expander-doc-cache-ht`
事故完全同类。`expansion-wrap.sls` 的 `expansion-template-cache` 同病（注释错误声称
"operation-level thread-safe"）。修复：reference.sls 两处 memo 改为 make-thread-parameter
+ get-thread-id 属主标记的每线程缓存（fork 线程继承参数值，单查 #f 不够）；模板缓存走
互斥锁（冷路径，保住跨线程 replay）。修复后 threaded init 连续 6 次干净通过，
~6.0s vs 单线程 9.6s。

**unused-imports 连带收益**：修复引用查找后从 2.1 s 降到 0.62 s（其内部也走
find-available-references-for）——确认查找基建是跨阶段公共成本。

### 1.7 2026-10-12 复测（e44f36c4 之后，插桩已还原；复测脚本 `bin/profile-workspace-init.ss`）

> 口径：本仓 'akku 过滤 829 路径、type-inference #f、单线程；init 墙钟 ~8.5-10.5 s
> （脚本轮 1 手动跑阶段 1-4 + 轮 2 全量 init-workspace，计数器只来自轮 2）。

**init-references ~7.8 s 的相位分解**：step 5.63 s / excluded-refs 1.32 s /
unused-imports 0.62 s / unused-locals 0.12 s。step 之外合计 ~2.1 s（27%），其中
excluded-refs 比 §1.4 的 0.86 s 涨了一半，值得单独剖。

**§1.5 的“#1 优化靶”已解决**：prep-find-refs 2.07 s → 0.38 s（174k 次，2.2 µs/次），
leaf-find-refs 2.51 s → 0.51 s（330k 次，1.5 µs/次）——合计 4.6 s → 0.89 s（-81%），
b2fc5b41 + e44f36c4 的 memo/快路径效果坐实。establish-rules 0.06 s、leaf-usage 0.09 s、
post-rules 0.93 s 均为噪声级。

**剩余 step 墙钟 5.6 s 的构成（run-rules 10.5 s / children-rec 10.8 s 为嵌套累计口径，
compound-total 39.8 s）**：规则体直方图（嵌套累计）——ufo-match 家族 ≈8.3 s
（match-next 3.17 + match-one 1.72 + match-two 1.18 + match-check-ellipsis 0.99 +
match 0.99 + match-check-identifier 0.24）仍是最大项，与 §1.3 结论一致（慢的是被反复
step 的生成代码，不是 steer）；手写规则 library 0.79 + import 0.45 + define 0.36 +
set! 0.32 + include/resolve 0.19 ≈ 2.1 s。

**新靶子：file-linkage 1.7 s ≈ load-process 1.7 s（99%）**。`init-matrix` 里
`load-process` 对**每个文档的每个 index-node** 做 match-index-node 分派只为找稀有的
`(load/load-library/load-program "...")` 形式（`analysis/dependency/rules/load.sls`），
矩阵写入仅 3.5 ms、import 库解析 12 ms、init-maps 0.4 ms。优化方向：分派前先做
头符号预判（非 load 家族直接 cheap-reject），或对 load 形式做一遍独立扫描。

**excluded-refs 内部剖析（1.32 s 墙钟）**：`process-library-identifier-excluded-references` 的
"涨到 1.32 s"主要是 GC 记账差异，真实构成——`find-available-references-for` 双参变体
（无 symbol，沿父链收集全部可见引用）2,992 次调用占 ~1 s（每次 ~330 µs，内含大量分配）；
`library-identifier?` 135,580 次仅 0.086-0.13 s（0.6-1 µs/次）；遍历本身可忽略。
该变体的优化方向是按 (document, node) memo，但 step 的 quote/quasiquote 子句在分析中途
调用它时 document-ordered-reference-list 仍在被 mutate——memo 需要按 step-pass 失效，
设计上要先解决失效粒度（参照 optimization-literature.md 中 per-node 缓存被证伪的教训）。

**load-process 已修复（2026-10-12，e44f36c4 之后）**：file-linkage 1.7 s ≈ 99% 在
`analysis/dependency/rules/load.sls` 对每文档每 index-node 的 match-index-node 分派
（外加每次递归重算 uri->path）。改为头符号预判 cheap-reject + uri->path 提升到入口一次：
file-linkage 1,727 ms -> 63 ms（27×），init 墙钟 8.5 s -> ~7.0 s（-18%）。
回归：test-file-linkage 84、test-library-import 5、test-workspace 68、
test-match-cascade-auto-resolve 14 全绿。

**方法备注**：eq-hashtable 不能拿 (cons 'rule name) 当 key（每次 fresh pair，
直方图退化成每调用一条）；计数 key 用 symbol； Chez 对 (define x (case-lambda ...))
的右侧体内放内部 define 会报 invalid syntax（变量 define 无函数 define 的体）；
排他段计时器会把 GC 停顿记到当时活跃的调用帧上（excl-walk 含 GC 口径 3.4-5.0 s
波动 vs 外部墙钟 1.3 s，判断热点时须用外部相位计时交叉验证）。

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
