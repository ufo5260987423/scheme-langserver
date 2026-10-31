# 类型推断 DSL 外部先例深度调研

> 调研日期：2026-10-31。动机：为 scheme-langserver 的类型推断 DSL（谓词符号树 +
> 规则表 + 候选并集，见 `domain-specific-language/`）寻找外部的理论先例、算法蓝本与
> 可借鉴机制。四个方向各由独立调研通道完成，文献阅读完整性在附录 A 逐份声明。
>
> 快速结论：**没有完全一样的系统**——"内置过程的类型写成可声明的规则表 + 模板宏 +
> 解释器"的组合未见第二家；但拆开来，每个特性都有 30 年内的成熟对应，且四个方向
> 在文献中互相引用、最终汇聚到 Castagna 的集合论类型框架。

## 总览：四条线其实是一条线

```
内部表示：  谓词符号树 + 候选并集  ──升级──▶  集合论内部表示（方向④，基础设施）
产品语义：  有类型就显示             ──补纪律──▶  success typing 判据（方向①，零误报诊断）
精度来源：  规则表手写门            ──自动化──▶  occurrence typing 分支细化（方向②，提升最大）
规则理论：  ad-hoc with-gate       ──找背书──▶  conditional types（方向③，理论正当性）
```

①②③④ 可独立落地，但组合起来构成自洽体系：④给判定、①给语义纪律、②给精度、③给
规则层形式化。长期看 ②的 restrict/remove 与 ④的 meet/diff 会是同一套原语——现在做
②时把 restrict/remove 实现为"候选集操作 + 留交集接口"，就是在为④铺路。

## 阅读指南

- §1 Success Typing（Dialyzer）——语义内核：过近似 + 空交判据 + widening
- §2 Occurrence Typing（Typed Racket）——分支细化，用户可见收益最大
- §3 Conditional Types + 宏类型规则——with-gate 的理论谱系与规则注册先例
- §4 Semantic Subtyping——`type:→?` 重写的算法蓝本与硬性差距
- §5 综合建议（性价比排序）
- 附录 A：阅读完整性声明；附录 B：全部来源 URL

---

# §1 Success Typing（Dialyzer）——给"候选并集"立语义

**核心一手资料**：
[Lindahl & Sagonas PPDP'06 全文](https://user.it.uu.se/~kostis/Papers/succ_types.pdf)（DOI 10.1145/1140335.1140356）、
[TypEr 全文](https://user.it.uu.se/~kostis/Papers/typer.pdf)、
[contracts 论文 Erlang'07](https://user.it.uu.se/~kostis/Papers/contracts.pdf)、
[Bugs'05 经验](https://user.it.uu.se/~kostis/Papers/bugs05.pdf)、
[Falsification 视角 arXiv:1502.01278](https://arxiv.org/abs/1502.01278)、
[etylizer 对比评估](https://arxiv.org/html/2302.12783v2)、
[OTP 源码 lib/dialyzer/src](https://github.com/erlang/otp/tree/master/lib/dialyzer/src)。

## 1.1 形式定义

**Definition 1（PPDP'06 §4.1）**：函数 f 的 success typing 是签名 `(ᾱ)→β`，满足：
*每当 `f(p̄)` 归约到值 v，就有 `v∈β` 且 `p̄∈ᾱ`*。关键在补集方向：`p̄∉ᾱ ⟹
该应用必定失败*（crash 或不终止）。推断目标是**过近似**"能成功"的集合，与
Hindley-Milner 收缩"被允许"集合的方向相反。

试金石（论文用布尔 `and/2` 比较三个系统）：

| 系统 | 推断类型 | 问题 |
|---|---|---|
| HM | `(bool,bool)->bool` | 拒绝 `and(false,42)`——但运行时不报错 |
| Marlow-Wadler 子类型 ICFP'97 | `(any,false)->bool` | 反直觉；域欠近似 |
| **Success typing** | `(any,any)->bool` | 域**过近似**：包含所有可能成功的调用 |

本质差异：静态类型域 ⊆ 动态类型域（欠近似，pessimistic）；success typing 域 ⊇
动态类型域（过近似，optimistic）。HM 用**合一**解约束；success typing 用**子类型**，
且域是无限制并集类型（`atom|pid` 直接写并，无需构造器包装），单子类型免费获得。

Jakob & Thiemann 的再表述（模型检测术语）：**标准类型系统做 verification（证明无错），
success typing 做 falsification（证伪，只报告 definite errors）**。

与 soft typing（Wright/Cartwright 一脉、MrSpidey）的区别：soft typing 报告**完备**
警告并插装运行时检查；success typing 报告**可靠**警告、零程序拒绝、不插装。

**类型错误判据**（PEPM'13 形式化，全部"用户可见价值"就这一句）：

```
e₁ 产生类型错误 ⟺ type(e₁) = τ₁ ∧ 上下文期望 τ₂ ∧ τ₁ ∩ τ₂ = none()
```

## 1.2 推断算法：两阶段

**阶段一：约束生成**（推导规则要点）：

- `ABS`：函数体约束绑定到函数类型上（constrained type），但**不外泄**；
- `CASE`：每个子句生成一个析取支，不转 DNF（多 case 嵌套时防爆炸）——这是
  path-insensitivity 的精确来源（TypEr §4.4.3 原话："making the analysis
  path-insensitive, thereby gaining time at the cost of precision"）；
- `APP`（非常规）：应用结果的类型 β 是函数值域的**上界**（β⊆α），而非传统系统的
  下界——success typing 只关心避免误报，乐观假设成立。

**阶段二：约束求解**：

- `Sol`：类型变量 → 具体类型；初始全部 `any()`，**递归变量初始 `none()`**
  （第一轮递归调用无解=⊥，被 join 吸收，leaf 子句先给闭式解）；
- 子类型约束求解 = meet：`T = Sol(α) ⊓ Sol(β)`；T=none() ⟹ 该子句不可满足 ⟹
  该子句**不贡献类型**；
- 析取的解 = 各支逐点最小上界（join）；`⊥ ⊔ Sol = Sol`；
- 整体：调用图 → SCC 凝缩 DAG → 拓扑逆序自底向上，递归组内迭代至不动点。

**并集与终止保障**：

- 类型格：单子类型 + 基类型 + 结构类型（元组按 arity；list 是**唯一递归类型**，
  `list(T) = cons(T,list(T)) ∪ nil`，参数必为具体类型）+ 函数类型 + 任意并。
- **union limit**：并集成员超限加宽到超类型（limit=3 时 `1∪2∪3∪4 → integer()`）；
  现代 Dialyzer 有 range 与 byte/char 阶梯式加宽。
- **depth-k 抽象**：复合项深度超阈值 k 的子项抽象为 `any()`，与 union limit 配合
  保证格有限 ⟹ 不动点必达。
- 下界：树自动机包含问题 EXPTIME-complete；实践中毫秒级（见 §4 性能数据）。

**精度来源——第二遍 refinement（PPDP'06 §6，全文最精巧的部分）**：

- *Definition 3*：refined success typing `(ᾱ')→β'` 满足 `ᾱ'⊆ᾱ`、`β'⊆β`，对所有
  `p̄∈ᾱ'` 能成功者 `f(p̄)∈β'`；额外约束有两个来源：调用点与程序员 spec；
- **闭世界 refinement**：模块系统不可绕过。escape analysis 区分 escaping（导出
  函数 + 闭包逃逸的局部函数）与 internal 函数；**internal 函数的所有调用点已知**，
  定义域 refine 为各调用点实参类型之并（经典例子：`length3(List,N)` 累加器从
  `(list,any)->any` 沿调用链 refine 成 `(list,integer)->integer`）；
- 前向数据流：从 escaping 函数入口启动（参数类型 jump-start 为 success typing 的
  域——安全，因为域包含一切可成功输入）；worklist 风格；函数体内 path 独立
  （case 各支 join），函数间有限 path 敏感（每个函数按**具体调用类型**分别分析，
  调用类型种数达上限时 widen 为并集）；
- **注意**：逃逸函数（= 库导出函数 = LSP hover 最常展示的）refine 不了。

## 1.3 零误报的本质与"报不出"的边界

三道保险：

1. **乐观初始化**：所有未知函数/不可达代码默认 `(any,...,any)->any()`
   （PPDP'06 §4.2 "a final note"——这同时是模块化手段：缺代码不影响可靠性）；
2. **path 不敏感 + join**：合并路径丢精度时只可能把类型变大（过近似方向），
   不会制造假冲突；
3. **主动压制**：Bugs'05 原话 "Dialyzer currently **suppresses all discrepancies
   that might be due to losing information when joining**"——宁可漏报，不误报。

诚实注脚：零误报是设计目标而非绝对定理。etylizer 在其 321 例基准上发现 Dialyzer
2 例假阳性（1 例确认 bug，OTP issue #6333，已修）。

**"报不出"的统一机制**：过近似太大 ⟹ 交集判据失效。具体病灶：

1. 恒等/透传函数退化：`id(X)->X` 推断 `(any)->any`——任何调用都与域相交，永远不报
   （LYSE 的 `convert/1` 例子：两处**交换后必崩**的调用，Dialyzer 全静默；补重载
   spec 后才报出）；
2. 控制流信息丢失：success typing 不维护"输入 integer 则输出 integer"的依赖；
3. 深度截断（§2.3 的 `{succ,...}` 例子：第 4 层被 depth-k 吃掉，必崩调用漏报）；
4. 尾部递归累加器；
5. 量化：[etylizer 基准](https://arxiv.org/html/2302.12783v2)（156 个含已知类型
   错误的用例）Dialyzer 只检出 89 个——**约 44% 的 definite type error 漏报**
   （基准偏小，量级感真实，不可外推大库）。

PEPM'13 展示了"报不出"并非全坏：调用点能**反向约束**变量类型（"even uses of
variables can restrict their values"）——success typing 的调用点既能前向也能
后向收窄。

## 1.4 多态的缺陷与补救

- 系统只有子类型多态，没有参数多态（TypEr §4.8 明确）。`id → (any)->any`；
  `lists:map/2` 是 `(((any)->any), [any]) -> [any]`；"success typing 准确捕获了
  所有不崩溃的调用，包括非预期调用"（`map(3.14, [])` happily 返回 []）。
- LPAR'18（仅摘要）批评："Dialyzer exhibits two main weaknesses: on the practical
  side, its ability to deal with functions that are typically polymorphic is rather
  poor; and on the theoretical side, a fully developed theory ... does not seem to
  exist"——多态 success type 的推导系统与正确性证明。
- **补救一：refinement 兜底**——但只对非逃逸函数有效。
- **补救二：user specs / 重载 contract**（contracts 论文，OTP `-spec` 的原始设计）：
  - spec 与 success typing 取 **infimum（协变 meet）**，四种情况中唯一报警的是
    **交集 = none**（contract violation）；spec 声明点不能被证伪则被信任，之后
    调用点违例算调用方责任（"soundness for contract violations"）；
  - 多态 type variable 实例化：单变量实例化为调用点实参类型；多变量实例化为各
    实参类型的**最小上界**（作者自认 ad-hoc）；
  - 重载 spec：域重叠时 **collapse 为支的并集**或**直接忽略**（现代 Dialyzer
    报 "Overloaded contract has overlapping domains; such contracts are currently
    unsupported and are simply ignored"）。

## 1.5 IDE 集成与规模化经验

- **TypEr**：输出源码级 `-spec` 注释文件；`--byte` 模式直接读 BEAM debug_info
  （对 LSP"只开 buffer 未存盘"场景有直接借鉴意义）；持久表（PLT 雏形）。
- **性能实测**：

| 数据点 | 数值 | 来源 |
|---|---|---|
| 旧版吞吐 | ~800 LOC/s（2GHz P4） | bugs05 |
| OTP 全库（~70 万 LOC）success typing | ~30 min | PPDP'06 §6.4 |
| PLT 构建（现代） | 示例 6m39s；R15B02 起并行 | LYSE |
| 增量 | `--incremental`：变更模块→依赖模块最小集重跑 | OTP User's Guide |
| LSP 场景（ElixirLS） | 首启 PLT 10+ 分钟 CPU 打满，之后自动增量 | elixir-lsp/elixir-ls |

- **ElixirLS**（IDE 先例）：LSP server 内嵌增量 Dialyzer + "automatic inline
  suggestion of @specs based on inferred success typings"——证明零误报纪律是 IDE
  场景被接受的必要条件（误报在编辑器里比 CI 更致命）。
- **错误解释**（PEPM'13）：约束求解器携带源位置集合，对每个报警计算**最小程序
  切片**——LSP 诊断需要定位到子表达式而非整函数。
- **PLT ↔ workspace cache**：PLT 存逐模块的 success typings + contracts + 调用图；
  未变模块不重分析。对应我们的 workspace FASL cache（mtime 一致性校验已有），
  把"逐 library 的推断签名"作为缓存内容即可。

## 1.6 与我们 DSL 的对应与落地路径

| Dialyzer | scheme-langserver DSL |
|---|---|
| `any()` | `something?` |
| union 过近似 | substitution 候选列表 |
| 空交判据 | **没有**——只有 `type:→?` 树匹配 |
| depth-k / union limit | `PRIVATE-MAX-DEPTH 10`、`chez-ir-rebuild-budget`（同哲学） |
| 闭世界 refinement | library-node 树（天然对应） |
| PLT | workspace FASL cache |

**结论（调研原文）**：success typing 的语义内核（过近似 + 空交判据 + 乐观 any
初始化 + 闭世界 refinement + union/depth widening）与 substitution 式规则 DSL
**完全兼容，其中判据与 widening 可立即落地**；约束求解器本身（meet/join 不动点）
不必搬——用谓词格写一个 **~200 行的交集检查 post-pass** 即可等价获得"零误报
诊断"能力。具体：generator 产出 substitution 后加 post-pass——对每个应用节点，
实参候选与规则定义域做谓词符号求交，空则报 "definite type error"。

**固有代价（接受）**：参数多态缺失（高阶标准库函数 hover 长期 `something?`，
与 lambda 参数槽退化同款病，可接受——目标是"不错"而非"证明"）；零误报纪律主动
压制诊断（"Dialyzer 没报 ≠ 没问题"要写进预期管理）；副作用盲区（`none()` 与
失败的混淆，诊断文案写 "will never return a value" 而非 "type error"）。

---

# §2 Occurrence Typing（Typed Racket）——最大的精度提升机会

**核心一手资料**：
[POPL'08 全文](https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf)、
[ICFP'10《Logical Types for Untyped Languages》全文](https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf)
（现行实现对应的最终形式化，DOI 10.1145/1863543.1863561）、
[Castagna POPL'22](https://www.irif.fr/~gc/papers/popl22.pdf)、
[Utah PLT ifT-benchmark](https://github.com/utahplt/ifT-benchmark)、
Typed Racket 参考手册（ts-reference 8.17）。

## 2.1 两代模型

**POPL'08（λ_TS，可见谓词版）**。类型语法：⊤、基类型、有限并集 `(S τ ...)`
（`(S)` = ⊥）、带潜在谓词的函数类型 `τ φ→ σ`。判断形式 `Γ ⊢ e : τ ; ψ`。

- 潜在谓词 φ（标注在函数类型上，"该函数是类型 τ 的判别器"）：
  `number? : ⊤ Number→ Boolean`；对单变量参数的应用把潜在谓词实例化为可见谓词；
- 可见谓词 ψ：`τx`（x 当前具有类型 τ）、`x`（x 不是 #f）、`true`/`false`、`•`；
- **环境更新规则（Fig. 4，细化规则的全部核心）**：

```
Γ + τx  =  Γ[x := restrict(Γ(x), τ)]     ; then 分支：收窄
Γ + x   =  Γ[x := remove(Γ(x), false)]   ; then 分支：去掉 #f
Γ + •   =  Γ
Γ − τx  =  Γ[x := remove(Γ(x), τ)]       ; else 分支：挖掉 τ
Γ − x   =  Γ[x := false]                 ; else 分支：必为 #f

restrict(σ,τ) = σ        若 ⊢ σ <: τ
restrict(σ,(S τ…)) = (S restrict(σ,τ) …)        ; 并集逐分量
restrict(σ,τ) = τ        其他情况（退化为 τ）
remove(σ,τ)   = ⊥        若 ⊢ σ <: τ              ; 分支不可达
remove(σ,(S τ…)) = (S remove(σ,τ) …)
remove(σ,τ)   = σ        其他情况
```

- **T-IF**：测试 e₁ 产生 ψ；then 分支在 `Γ+ψ` 下，else 在 `Γ−ψ` 下。例：
  `(λ (x : (S Number Boolean)) (if (number? x) (= x 1) (not x)))`——then 分支
  x:Number，else 分支 `remove((S Number Boolean), Number) = Boolean`；
- and/or 的组合（combpred 元函数）是**特设的**——ICFP'10 明确列为旧系统三大缺陷
  之一；
- 声音性麻烦：β 归约后的中间项可能不可类型化（如 `(if (number? #f) ...)`），
  论文加了仅用于证明的辅助规则。模型经 Isabelle/HOL + PLT Redex 双重机械化
  验证（github.com/samth/popl08-model）。

**ICFP'10（λ_TR，命题逻辑版，现行基础）**。判断升级为：

```
Γ ⊢ e : τ ; ψ⁺ | ψ⁻ ; o
```

e 具有类型 τ；若 e 为真则命题 ψ⁺ 成立，为假则 ψ⁻ 成立；o 是**对象**（e 访问的
运行时环境片段：∅、变量、或访问路径 π(x) 如 `car(cdr(x))`）。

- 命题语法：`τx`、`¬τx`、`ψ∧ψ`、`ψ∨ψ`、`ψ⊃ψ`、`tt`、`ff`；
- 函数类型携带**潜在命题对**（箭头上方）与**潜在对象**（箭头下方），参数命名
  使命题良作用域：

```
number? :  y:⊤  N(y) | ¬N(y)  ──→(∅)  B
car     :  x:⟨⊤,⊤⟩  ⊤car(x) | ⊤car(x) ──→(car) ⊤
```

- **关键洞察：两个潜在命题不一定互补**。`(and (number? x) (> x 100))` 的真命题
  是 `N(x)`，假命题却是 `¬N(x) ∨ ¬(>x100)`（x 可能是 97）——**这就是 and 的
  else 分支不能简单取反的原因**（ICFP'10 §3.1）；
- **T-VAR**：变量类型**从命题环境推导**——若 Γ 能证明 `τx` 则 x:τ；类型环境降级
  为命题环境的特例；
- **T-LET**：环境加 `τx` 与两条蕴含（`#f(x) ⊃ ψ₀⁺` 等），处理 or 展开中
  "tmp 为真 ⟹ (number? x) 为真"的推理；
- **and/or/not 不设特殊规则**：由 if 展开 + T-IF 传播自然导出。or 的真命题 =
  `ψa⁺ ∨ ψb⁺`，假命题 = `ψa⁻ ∧ ψb⁻`；and 反之；
- 证明系统（Fig. 4，自然演绎）关键原子规则：
  - L-SUB：`τx ⊢ σx` 若 τ<:σ；
  - L-SUBNOT：`¬τx ⊢ ¬σx` 若 σ<:τ（否定单调性反向）；
  - L-BOT：`⊥x` 推出任何命题；
  - L-UPDATE：`update(τ,σ) = τ∩σ`、`update(τ,¬σ) = τ−σ`；带路径时先沿路径进入
    类型再细化（环境证 `⟨⊤,⊤⟩x` 与 `Ncar(x)` 可推 `⟨N,⊤⟩x`）；
- cond 多子句推理（§5.3 例 14 全文推导）：第二子句环境已有第一子句测试失败的
  命题，叠加本子句测试命题，由矛盾消去得细化结果——"程序员式推理"的完整机器化。

## 2.2 算法化：语法导向单遍检查，不是约束求解

- ICFP'10 §7.4：类型系统非算法化；实现需 (a) 消去 subsumption（把子类型义务分配
  进各规则）；(b) 实现 T-VAR（从命题环境推导变量类型的过程）；
- Typed Racket 实现形态（typed-racket-lib/typecheck/）：命题实现为受限 ADT——
  `Top`/`Bot`/`(: obj type)`/`(! obj type)`/`(and …)`/`(or …)`/`(when p q)`/
  `(unless p q)`/`(if p q r)`/`(linear-comp …)`；对象 = 标识符 + 路径元素列表。
  环境操作 = lookup/update（即 restrict/remove），推理**只在需要给变量定类型时**
  对原子命题做 restrict/remove，复合命题靠构造时的规范化（CNF/DNF 化）——
  **"证明系统"退化为命题集合的增量规范化 + 原子查询，无需一般定理证明**；
- 语法导向：类型检查在完全宏展开后的核心形式上单遍进行（Typed Racket 把检查器
  **嵌入宏展开器**）；
- 子类型关系本身平凡（顶层/自反/并集两条/函数逆变协变 + 潜在谓词可丢弃）；
  **细化全部发生在环境操作层，与子类型判定正交**——并集子类型是"候选并集"细化
  能工作的前提：then 分支 = 过滤，else 分支 = 删除；
- 可变性处理（ICFP'10 §7.1）：λ_TR 假设不可变；Racket 中可变字段的 selector
  被赋予平凡潜在命题/对象；被 `set!` 赋值的变量**禁用 L-UPDATE**（模块级静态
  分析找所有赋值点）——对 LSP 好消息：静态分析 set! 目标并禁用细化即可，
  模块化且便宜；
- case-lambda/多值（§7.2）：函数值域变为"每个返回值各带类型 + 潜在命题对 +
  潜在对象"的序列。

## 2.3 工具链中的同类实现

| 系统 | 关系 |
|---|---|
| Typed Clojure / core.typed | 直接改造 TR 的 occurrence typing（ESOP'16）；后有 symbolic closure types（POPL'22） |
| Flow | 官方术语 "type refinements"；用**函数类型交集**表达谓词分支精确类型 |
| TypeScript | "type guard recognition"/narrowing；occurrence typing 的**直系后代但刻意简化**（无否定/交集的系统内账户，谓词细化硬编码于 CFG 走查；不完备也不保真）。Castagna《Revisiting Occurrence Typing》(SCP 2022)：narrowing ≡ occurrence typing，且二者从诞生起就与集合论语义类型纠缠 |
| Whiley | Pearce 独立再发明 flow typing |
| Rosette | 符号执行语言中的 occurrence typing（POPL'17） |
| mypy/Pyright 等 | 现代工业形态；ifT-benchmark 用 ICFP'10 例子做跨语言基准 |

**Occurrence Typing Modulo Theories**（Kent, Kempe, Tobin-Hochstadt, PLDI'16，
arXiv:1511.07033）：命题可携带线性整数约束交外部求解器；56k 行 TR 代码案例：
vector 访问 50% 零注解可证安全，加少量注解达 ~80%。对轻量 DSL 超预算，但其
"命题可扩展"架构（`(linear-comp obj obj)` 作为 prop 语法一格）值得借鉴。

**理论下限（对轻量实现最有用）**：Castagna POPL'22《On Type-Cases, Union
Elimination, and Occurrence Typing》——不需要效应系统、不需要流分析、不需要发明
新类型，只要 type-case + MacQueen et al. 1986 的三条 union elimination 规则，
即覆盖 occurrence typing 的全部用例。**restrict/remove 正是这三条规则的算法形态**。

## 2.4 限制、代价与元理论警告

1. **宏交互**（POPL'08 自承局限）：必须先宏展开再检查；"深刻改变绑定结构"的复杂
   宏无法处理。Takikawa 博士论文记录了 class 系统的宏插桩工程；
2. **可变性**：路径细化对可变数据不可靠（别名）；samth 确认"occurrence typing
   不细化 mutable 变量"；
3. **元理论（重要）**：Fu, Angiuli & Tobin-Hochstadt 2026《Revisiting Soundness
   for Occurrence Typing, Semantically》(arXiv:2609.16299) 证明 ICFP'10 的替换式
   元理论**存在多处错误、声音性定理表述甚至不是良构的**，缺陷被 Kent'16、
   Fu'21、Bonnaire-Sergeant'16 继承，并**在 Typed Racket 实现中表现为真实的
   声音性 bug**；修复用 step-indexed logical relations（Lean 形式化）。**含义：
   即便原作者团队，这套机制的保真元理论也花了 16 年才理顺——轻量实现应定位为
   不完备的启发式细化（像 TypeScript 那样），不要背负保真承诺**；
4. **性能**：occurrence typing 本身开销平凡（语法导向 + 小命题规范化）。Typed
   Racket 的著名性能灾难（渐进类型化某些配置慢 90–121 倍，Takikawa POPL'16）
   来自**运行时契约包装**，与 occurrence typing 无关——LSP 无运行时边界，无此
   负担；
5. 作者整体反思：SNAPL'17《Migratory Typing: Ten Years Later》。

## 2.5 落地设计（裁剪决策表）

| λ_TR 概念 | DSL 对应物 | 一期决策 |
|---|---|---|
| 类型 τ | 谓词符号树节点 + 候选并集 | 已有 |
| then 分支 | `restrict(σ,τ)` = 候选集**过滤**（保留 <:τ 的分量，全失败退化为 τ） | **实现核心，几十行** |
| else 分支 | `remove(σ,τ)` = 候选集**删除**（σ<:τ 得 ⊥ ⟹ 分支标不可达） | **实现核心** |
| and/or/not | 小命题代数：and 假命题 = 析取、or 真命题 = 析取；从 if 展开自然推导 | 只处理**单层谓词组合**，不实现蕴含/路径 |
| 潜在命题（用户谓词函数） | 谓词类型声明表（alist） | **一期放弃**；二期支持"谓词类型声明表" |
| 对象/路径 π(x)（car/cdr 细化） | `inner:pair?` 有分量类型，可加 car/cdr 两路径 | **一期放弃**（ICFP'10 §7.1 可变性问题随之规避） |
| 命题环境 Γ,ψ⁺/ψ⁻ | 分支环境 = 原替换环境副本 + 每变量收窄后的类型；两分支各持一份，join 点取并集 | 与现有 substitution 机制天然兼容 |
| 可变性 | 对 `set!` 目标（DSL 已能识别）禁用细化 | 照搬 TR 方案 |
| cond 多子句 | 顺序累积 else 命题 | **二期**；一期只做单 if 即可覆盖多数 LSP 场景 |

接入点：类型 pass 已是 expansion-driven（chez-ir->annotated-sexp + 虚拟文档），
细化作用于**展开后的 if 形式**，与 macro-expansion.sls 同层。语义定位明示：
**细化是"提示增强"而非"错误保证"**——细化失败（类型没有变窄）永远静默，不会报
假错。

---

# §3 Conditional Types + 宏类型规则——with-gate 的理论谱系

**核心一手资料**：
ALW94（[DOI 10.1145/174675.177847](https://doi.org/10.1145/174675.177847)；
**原文未能获取**——Stanford 主页/ACM DL/archive.org 均不可达；以下基于
[Jensen POPL'98 §6 逐字技术描述](http://web.cs.ucla.edu/~palsberg/tba/papers/jensen-popl98.pdf)
+ [Trifonov & Smith POPL'96 全文](https://flint.cs.yale.edu/trifonov/papers/subcon.pdf)）；
[Typed Clojure 博士论文全文](https://thesis.ambrosebs.com/)
（Part III Ch16–19 精读）；
[Advanced Macrology SFP'07 全文](https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf)。

## 3.1 Conditional types：`(T→S | C)` 的确切语义

ALW94 的类型语言：函数类型、构造子类型、**并/交/递归类型** + 条件类型构造子 `?`。
推断结果为 `∀ᾱ. τ where C`（C = 限制类型变量实例化的子类型约束集）。

条件类型 `τ?φ` 的语义（Jensen §6 逐字描述）：**只有当 guard φ 非空（可满足）时，
结果才具有类型 τ；若 φ 归约为空类型，整个条件类型消去**。Jensen 称之为
"conjunction across types"（跨类型合取）。

完整 worked example（Jensen 引述）：`λy. case y of true→zero, false→succ(zero)`
推断为：

```
∀α. α → (zero?(α∩true)) ∪ (succ(zero)?(α∩false))  where {α ⊆ true∪false}
```

α 实例化为 `true` 时 `α∩false` 归约为空 ⟹ 该分支从类型中删除 ⟹ 整体归约为
`true→zero`。**guard 不可满足 ⟹ 分支消去**——这就是 DSL 中 `with-equal?` 门失败
时静默回退、不产生替换的理论原型（30 年先例）。

约束与求解：推断归结为**类型包含约束的可解性**（Aiken & Wimmers FPCA'93 的扩展，
为处理 union types）。Trifonov & Smith《Subtyping Constrained Types》系统化
`∀ᾱ.τ\C` 的子类型化，三层次：操作语义 ≤∀obs / 语义 ≤∀sem / 可判定近似 ≤∀dec；
要点：(SUB) 规则是唯一允许把约束集纳入考虑的规则；约束集可满足性是分支存亡的
判定核心。2024 年《Ill-Typed Programs Don't Evaluate》明确师承 ALW94 的单侧系统，
但指出 ALW 约束"far more expressive"——**高表达力以求解复杂度为代价**。

与 intersection types 的关系：交/并是原生成分；条件类型不是交类型的特例，而是其
**跨类型推广**（guard 与被守卫类型可属不同类型）。

## 3.2 谱系：谁继承了条件类型

软类型文献是**两条独立谱系**（[Soft Contract Verification §5](https://arxiv.org/abs/1307.6239)）：

1. **HM 推广路线**：Cartwright & Fagan PLDI'91 → Wright & Cartwright TOPLAS'97 →
   **Aiken et al. POPL'94**（条件类型）；
2. **set-based 抽象解释路线**：Flanagan PLDI'96 → Flanagan & Felleisen
   （MrSpidey）——**类型语言中没有条件类型构造子**。

条件类型思想的真正"复兴"在 occurrence typing：ICFP'10 把"谓词测试为真/假时
参数获得什么类型"形式化为函数类型上的**潜在命题**——`(T→S|C)` 的现代可组合
版本，被 Typed Clojure 原样继承。TR 现役还有 `#:pre` 前置条件特性
（错误信息如 "unable to prove precondition: `(<=2 (vector-length a))`"）。

**对 DSL 的直接映射**：`(with ((a b c)) …)` ≈ `∀ᾱ. τ where C` 的约束头；
`(with-equal? inner:list? a b)` ≈ `τ?φ` 的 guard（类型变量间的相等/可满足性
约束）。guard 作为类型规则一等成分有 30 年先例。Trifonov & Smith 的教训：
**guard 语言保持小且可判定**（DSL 现在只有类型相等 + 类型谓词，恰好是对的）。

## 3.3 宏的自定义类型规则：两条工业路线

**路线 a（Typed Racket 2007，SFP'07 全文）**：类型检查发生在**全展开后**的 core
form 上（原话："generally impossible to derive type rules for arbitrary
macros"）；用户**不能**注册规则。跨展开通信靠 **syntax property 协议**：typed
绑定宏展开时把类型塞进标识符的 `'type-label` property，检查器 `get-id-type`
读回；`require/typed` 的 contract 包装打 `'TRUST` property。宏与检查器通信三
机制：static bindings（syntax-local-value）、副作用、syntax properties。

**路线 b（Typed Clojure 2019，博士论文 Part III 全文）**：**交错展开**——自研
analyzer 提供 `unanalyzed` 与 `analyze-outer`（只展开一层）；检查器主循环对
未分析节点按**解析后的 head symbol 分发**——命中已注册规则（如
`clojure.core/cond → check-special-cond`）就对**未展开的 surface form** 直接
检查；否则再展开一层。另有 `do-special-form` 语法协议承载宏→检查器的常量信息
（选 `do` 位置因为元数据在展开中会丢、常量信息可被编译消去）、求值期副作用
通信（AOT 下展开期副作用会丢）、wrapper 宏（对展开结果太复杂的宏复制展开逻辑
+ 插入注解）。Ch 19 如实讨论展开顺序/副作用/重复展开的正确性前提。

**与 scheme-langserver 的对应**：generator/router 按 head 分发 + type 侧
expansion-driven 推断（beta 归约 + 虚拟文档）**已经是路线 b 的同构实现**；
Typed Clojure Ch19 对展开顺序/副作用/重复展开的讨论对本项目"负缓存 + 展开预算"
的工程决策是**直接先例**。缺的只是"规则可注册"的接口设计与 soundness 讨论
框架。

学术进阶设计（若哪天想摆脱每族宏手写模板）：
[Turnstile](https://docs.racket-lang.org/turnstile/)（规则 = 展开 + 检查合一，
一致性自动）、SoundX（先检查后去糖）、Ziggurat（宏的静态/动态语义分离定义，
"类型规则塔"）、[Type Tailoring ECOOP'24](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2024.44)
（宿主宏细化类型）、SweetT（规则自动推断）。

## 3.4 结论

DSL 的 with-gate **处于 30 年谱系上**，不是野路子；形式化时借鉴 `τ?φ` 的
"guard 满足性驱动分支存亡"语义与 latent proposition 的对象-路径表示（λTC 的
π(x)）。guard 语言保持小且可判定是已被验证的设计决策。

---

# §4 Semantic Subtyping——重写 `type:→?` 的算法蓝本

**核心一手资料**：
[FCB LICS'02 全文](https://xml.coverpages.org/Frisch-lics02.pdf)（JACM'08 前身，
理论相同；JACM 正式版被反爬未获全文）、
[Castagna primer arXiv:1809.01427 §4.2](https://arxiv.org/html/1809.01427v8)、
[Frisch 博士论文 Ch.7/§11.3/§11.5](https://www.cduce.org/papers/frisch_phd.pdf)、
[Kent 教程全文](https://pnwamk.github.io/sst-tutorial/)（Redex 模型，**Scheme 方言
写的，是最合适的对照蓝本**）、
[jclark/semtype 源码](https://github.com/jclark/semtype)（≈2600 行 Ballerina，
core.bal/function.bal/bdd.bal 等已精读）、
[ICFP'16 tallying](https://www.irif.fr/~gc/papers/icfp16.pdf)。

## 4.1 理论核心

类型 = 值集合，子类型 = 集合包含（Hosoya/Pierce 在 XDuce 首创正则树语言版本；
FCB 推广到高阶函数 + 任意布尔组合 + 递归类型）。论域 D 划分 D_basic / D_prod /
D_fun 三个**互不相交**分片；∨∧¬ 解释为并/交/补。

**bootstrap 循环**：函数是值，判断函数值是否属于 `t→s` 需要对函数体做类型检查，
而类型检查又依赖子类型——定义循环。解法：先取非计算性 bootstrap 模型定义子类型
与类型系统，再定义操作语义，最后证明**良类型值集合 V 本身构成等价模型**
（LICS'02 Theorem 4.1）。

**核心判别定理（LICS'02 Theorem 2.17，箭头判定的完备规则）**：

```
∩ᵢ(tᵢ→sᵢ) ⊆ ∪ⱼ(t'ⱼ→s'ⱼ)
⟺ ∃j. [ t'ⱼ ≤ ∨ᵢ tᵢ  ∧  ∀I′⊆I. ( t'ⱼ ≤ ∨_{i∈I′} tᵢ ) ∨ ( ∧_{i∈I∖I′} sᵢ ≤ s'ⱼ ) ]
```

即对正箭头集合的所有子集组合做分裂——**语义子类型相对树匹配多出来的主要复杂度
来源**。实用推论：`(t₁∨t₂)→s ≃ (t₁→s)∧(t₂→s)` 等分配律在语义模型中自动成立
——ad-hoc 树匹配必须手工枚举这类等式且容易漏。

**判定算法（primer §4.2 四步）**：

1. `S <: T` 归约为空集判定：`isEmpty(diff(S,T))`——**差与交不是可选增强，是
   算法入口**；
2. 布尔表达式化 DNF（每子句 = 正原子合取 ∧ 负原子合取）——DNF 不是中间产物
   而是类型的规范表示；
3. 按种类分片（乘积只跟乘积比，箭头只跟箭头比）；
4. 分片内按构造器分解：积逐分量协变 + 负原子子集枚举；箭头用 Thm 2.17；递归
   类型用模拟关系的余归纳不动点。

**递归类型**：类型代数定义为递归类型代数；**正则性**（类型的"基"传递闭包有限）
是终止性关键。equirecursive（CDuce：hash-consing DAG + 带回溯/免回溯的归纳谓词
计算 + cache）与 isorecursive 均可。

## 4.2 开源实现

**jclark/semtype**（Ballerina 实现，源码精读，≈2600 行）：

- 值域划分 22 个 "uniform types"（可选 readonly/read-write 拆分）；SemType =
  32 位位集或 `ComplexSemType{all, some, subtypeDataList}`；
- 每个 uniform type 一种 SubtypeData：简单类型用布尔；int/string/boolean 用
  专门区间/集合；**list/mapping/function 用 BDD**（三叉决策图 `Node{atom,left,
  middle,right}`——二叉版本 union 会指数复制，三叉版本 union 线性；即 Frisch
  论文 §11.3 的"arbres de décision ternaires"）；
- 每构造器五个运算：`union/intersect/diff/complement/isEmpty`；顶层
  `isSubtype = isEmpty ∘ diff`；
- 函数空集判定 = Thm 2.17 直接实现（`functionBddIsEmpty` 沿 BDD 累积正箭头域
  并集，`functionTheta` 做子集分裂）；
- 递归类型：`Env` 持 defs 数组，先插 dummy 再回填（指针式 equirecursive）；memo
  表按 BDD 节点缓存；多参数函数去糖成单参数 tuple → 单返回值。

**CDuce**（OCaml，论文级）：0.2 版约 20000 行；三叉决策树 + hash-consing；算法 =
 归纳谓词 Φ 计算 + cache，免回溯变体是论文贡献之一。**Kent 教程**（Redex 模型 +
  伪代码 + QuickCheck 式测试策略）：类型 = `(Base, Prod-BDD, Fun-BDD)` 分片三元组；
  **lazy BDD**（union 不合并子树、节点挂 union 列表）；subtyping/isEmpty、积/箭头
  inhabitation、函数应用全套伪代码。**这是 Scheme 重写最合适的对照蓝本**。

**tallying 与 OCaml**：OCaml 本体**没有** tallying（grep ctype.ml 确认）——
polymorphic variants 用 kinding constraints 模拟子类型；tallying 是集合论文献的
约束求解算法（`tally(D)` 产生有限主解完备集），每个分支判定以 ground 语义子类型
为 oracle。**对无多态、无类型变量的轻量推断完全用不上**——只在引入 ∀-多态时才
需要。

## 4.3 与我们系统的硬性差距（源码级对照后得出）

语义子类型判定要求**布尔代数 + 每构造器五运算**。对照现状：

| 必需 | 现状 | 结论 |
|---|---|---|
| 显式并 ∪ | 分散在替换候选层 | 需提升为类型节点内的 first-class 表示 |
| 空 ⊥ 与顶 ⊤ | `inner:void?`/`something?` 近似有 | `something?` 是"未确定"语义，与 ⊤（全部值的集合）不同，需精确化 |
| 补/差 ¬∖ | **完全没有** | **硬缺口**——无它无法归约到空集判定 |
| 交 ∩ | **完全没有** | 箭头/积分解都要求 |
| 每构造器 isEmpty | 无 | pair/list/vector/lambda 各写一个 |
| 值域不相交分片 | `inner:pair?` 与 `inner:list?` **语义重叠**（AGENTS.md 记载的老陷阱） | **重写中最大的语义设计工作**——集合语义不能容忍重叠，必须先重写为不相交原子（proper-list / improper-pair / empty-list 三分） |

**只取并 ∪ + ⊤ + 函数类型的子集可行吗？不可行**——判定 `S<:T` 的第一步就是
构造 `S∧¬T` 判空；没有否定/差，只能回到不完备的语法规则。

**折中**：交集可以只在内部表示里存在，不出现在用户可见语法——Kent 教程/semtype
正是此路线（表面只有并，内部自由产生交/补；与"替换候选列表 = DNF 子句"恰好
同构）。**放弃函数类型的完备判定**（箭头先用单条近似规则：dom 反变 + cod 协变 +
dom 非空，即 Flow/TS 的选择），换可靠但不完备的判定——对 LSP 提示/诊断场景是
最优性价比起点。

## 4.4 重写路线建议

1. 谓词语义细化为**互不相交的值域分片**（Scheme 值域：null / pair / vector /
   char / string / number / symbol / procedure / record / void / eof …；
   `inner:pair?` vs `inner:list?` 重叠必须解决）；
2. 内部表示改为 `(basic-位集, pair-BDD, lambda-BDD, vector-BDD, …)` 或 lazy BDD；
   基本类型用位集（Scheme 基本类型天然有限枚举，比 int 区间简单）；
3. 每构造器五运算 + isEmpty；`sem:subtype? = isEmpty ∘ diff`，memo 化
   （注意 AGENTS.md 线程安全教训：memo 表 per-thread 或加锁）；
4. 表层继续输出现有谓词符号树（打印语法），**只重写内部相容性判定**——对 LSP
   协议输出零侵入；
5. 测试策略抄 Kent 教程：随机生成类型 + 集合公理 property-based 测试
   （交换/结合/分配/德摩根、S∖T=∅ ⟺ S≤T）——CDuce/semtype 都验证了这套方法
   能抓出单测抓不到的 bug。

## 4.5 性能证据

- CDuce 实测：450 行程序（~3500 内部类型节点）编译 <0.2s，其中约一半在子类型
  算法；子类型判定被调用 19,956 次，共 70,999 次内部迭代（Frisch 论文 §11.5，
  "理论最坏指数、实践毫秒级"论点的原始出处）；
- 下界：树自动机包含 EXPTIME-complete；指数行为主要由箭头判定的子集分裂与
  否定引起；
- semtype 无正式基准；worst-case 内部循环 O(2^|N|)/O(2^|P|)，memo 表使重复
  子问题多项式化——LSP 规模的调用量（≤10⁵ 次）完全无压力；
- **真正的风险不是性能，是 pair/list 重叠的语义重设计**。

---

## 4.6 内部表示升级实施方案

> 本节把 §4 的调研结论落成可执行计划。核心原则：**只换判定层，不动上层**。

### 分层影响面

```
rnrs-meta-rules（数据）→ generator（生成 substitution）→ type:→?（相容性判定，ad-hoc 树匹配）
                                                              ↑ 只有这一层被替换

升级后：
rnrs-meta-rules（不动）→ generator（不动）→ type:→? 改为：谓词树 → SemType → isEmpty(diff)
```

substitution 生成、candy 匹配、LSP 输出全部不动。`type:→?`（interpreter.sls:70-113）
里现有的 `inner:?->pair` + `candy:match-left` 结构比较，换成"转换 + 集合判定"，
判完 SemType 即弃（或缓存）。`inner:?->pair`（含其 30 年模式变量 quirk）随之退役。

### 第一步：pair/list 重叠的解法——换 primitive

集合论语义要求值域不相交分片，而 `inner:pair?` 与 `inner:list?` 语义重叠
（非空 proper list 的脊柱就是 pair）。解法不是强行拆分，而是换 primitive：

```
pair    = 原始积构造器 (T1 × T2)            ← 唯一的 pair 表示
null    = 基类型 ('())
list(T) =def μα. (pair T α) ∪ null         ← 派生递归类型，表面语法保留
```

`inner:list?` 只是 `(inner:pair? T ...)` 链 + `null` 的语法糖，规则库一行不改。
数值塔同样展开为叶谓词的并（fixnum/bignum/flonum/ratnum/cflonum 各占一个 bit；
`integer? = fixnum ∪ bignum`，`real? = integer ∪ flonum ∪ ratnum`，依此类推）。

### 内部表示（Kent 教程路线）

```scheme
;; SemType = 分片记录（各分片互不相交；Scheme 基类型是有限枚举，
;; 比 Ballerina 的 int 区间简单，直接用位集）
(define-record-type sem:type
  (fields base-bits    ; fixnum 位集
          pair         ; BDD 节点或 #f（car/cdr 各为一个 SemType）
          arrow        ; BDD 节点或 #f（域/值域 SemType）
          vector))     ; BDD 节点或 #f（元素 SemType）

;; BDD = 三叉决策图 #(atom left middle right)，按 atom 严格降序保证共享
;; （二叉版本 union 指数爆炸，三叉线性——semtype 的 bdd.bal 与
;; Frisch 论文 §11.3 "arbres de décision ternaires" 同款）
```

每个构造器实现五运算：`union / intersect / diff / complement / isEmpty`。
判定入口一句话：`sem:subtype?(S,T) = isEmpty(diff(S,T))`（primer 四步算法的 Step 1）。

### 分期落地（每期独立验证）

**Phase 0：差异测试先行（~1 天）**。抄 Kent 教程 §6 的测试策略：随机生成谓词树
类型 + 集合公理（`S∖T=∅ ⟺ S≤T`、交换/结合/分配/德摩根），先对**现有 `type:→?`**
跑一遍，记录所有违背公理的案例——这是重写的验收基线，也是语义设计错误的探测仪。

**Phase 1：纯基类型（~2 天，~150 行）**。SemType 退化为 fixnum 位集：

```scheme
(define (sem:subtype? s t)
  (fxzero? (fxand (sem:type-base-bits s) (fxnot (sem:type-base-bits t)))))
```

覆盖所有非容器比较（number?/string?/谓词 reify 后的 identifier-reference——
按标识符名查基类型表）。这一期就能替换 `type:→?` 的大部分调用点。

**Phase 2：pair + null + vector（~1 周）**。引入 BDD；`inner:list?` 糖展开；
`inner:pair?` 进积构造器分解（primer Step 4：逐分量协变 + 负原子子集枚举）。
pair 的 isEmpty 就是"两个分量都非空"。

**Phase 3：箭头（~1 周）**。lambda `(ret <- params)` 进 arrow 分片。**先用近似单
规则**（dom 反变 + cod 协变 + dom 非空——Flow/TS 的选择），不实现 Thm 2.17 的
指数子集分裂；留升级口。

### 代码落点

新子目录 `analysis/type/semantic-subtyping/`（遵守分层规则，只被 analysis/type 引）：

```
semtype.sls    ; SemType record + 位集运算 + 五运算分发 + memo
bdd.sls        ; 三叉决策图
convert.sls    ; 谓词树 → SemType（带缓存；identifier-reference 按名查基类型表）
api.sls        ; sem:subtype? / sem:equivalent? / sem:restrict / sem:remove
```

interpreter.sls 的 `type:→?` 改调 `sem:subtype?`。**方向②的 restrict/remove 在此
直接变成 `sem:intersect` / `sem:diff`**——"②为④铺路"的具体含义：做②时接口就按
这个签名留。

### 风险清单（按真实程度排）

1. **memo 表线程安全**——AGENTS.md 记载过全局弱 eq 哈希表多线程下崩过的教训，
   per-thread（reference.sls 的 make-thread-parameter 先例）或加锁；
2. **`something?` 语义**——是"未确定"而非 ⊤；转换层区分：未确定 ⟹ 不做判定直接
   通过（保持现有宽松行为）；
3. **`inner:void?`**——"无返回值"标记而非底类型，作特殊 base bit，不参与 complement；
4. **模板段修饰符 `...`/`**1`**——不是类型；到达 SemType 的只有解释完毕的类型，
   但要用断言钉死这个假设；
5. 性能无忧（CDuce 实测 ~2 万次判定 <0.1s，本项目量级更小）——**风险全在语义
   设计**，所以 Phase 0 差异测试不是可选项。

---

# §5 综合建议

| 方向 | 用户可见收益 | 工程量 | 风险 | 建议 |
|---|---|---|---|---|
| ② occurrence typing 简化版 | **大**（根治 something? 泛滥） | 小（restrict/remove 几十行 + if 规则分支环境） | 低（启发式定位，元理论无负担） | **第一个做** |
| ① success typing 判据 post-pass | 中（新增一类零误报诊断） | 小（~200 行交集检查） | 低 | 第二个做 |
| ③ 形式化 + 规则注册接口 | 小（理论背书 + 可扩展性） | 中 | 低 | 随文档/重构做 |
| ④ 内部表示升级 | 大（长期） | **大**（值域不相交化是语义重设计） | 中高 | 单独立项，按 §4.6 分期实施（Phase 0 差异测试先行） |

收敛信号：四个方向在文献中互相引用、最终都汇到 Castagna 的集合论框架
（POPL'22 把 occurrence typing 归约到 union elimination；success typing 的类型格
是 semantic subtyping 的特例）。长期看 ②的 restrict/remove 与 ④的 meet/diff
是同一套原语——做②时把 restrict/remove 实现为"候选集操作 + 留交集接口"，就是
在为④铺路。

---

# 附录 A：阅读完整性声明

**§1 success typing**：✅ 全文精读：PPDP'06 核心论文、TypEr、contracts、
PEPM'13 错误切片、Bugs'05 主体、LYSE、etylizer、OTP man page/User's Guide；
⚠️ 部分：Jakob & Thiemann arXiv 扩展版（引言/形式化/λC/深度截断示例/结论）；
❌ 仅摘要：LPAR'18 多态 success types、Erlang'21 十五周年报告、FLOPS'16。

**§2 occurrence typing**：✅ 全文：POPL'08、ICFP'10（图 1–13 为图片不可读、
§7.4 末尾一小段截断）；其余读摘要 + 关键章节转述。2026 元理论再检视论文
（arXiv:2609.16299）为转述。

**§3 conditional types + 宏规则**：✅ 全文：Advanced Macrology SFP'07、
Typed Clojure 博士论文（Part III Ch16–19、Ch25 精读）、Jensen POPL'98、
Trifonov & Smith POPL'96、TR ts-guide；❌ 未获全文：ALW94 POPL'94 原文
（Stanford 主页/ACM DL/archive.org 不可达，已用 Jensen §6 逐字描述 +
Trifonov & Smith 补全）、Flanagan/Felleisen SBA 原文（镜像 404，谱系结论依据
Soft Contract Verification §5）。

**§4 semantic subtyping**：✅ 全文/源码：FCB LICS'02 前身全文、Castagna primer
§4.2、Frisch 论文 Ch.7/§11.3/§11.5、Kent 教程全文、semtype README/NOTES/TODO/
core.bal（635 行）/function.bal/mapping.bal 前 200 行/list.bal 前 60 行/
bdd.bal 前 100 行、ICFP'16 主要章节；❌ 仅摘要/元数据：FCB08 JACM 正式版
（HAL 被反爬）、Castagna 2022 教科书（仅参考文献页）、CDuce OCaml 源码本身。

# 附录 B：全部来源 URL

## §1 success typing
- 核心论文：https://user.it.uu.se/~kostis/Papers/succ_types.pdf
- TypEr：https://user.it.uu.se/~kostis/Papers/typer.pdf
- Contracts：https://user.it.uu.se/~kostis/Papers/contracts.pdf
- Bugs'05：https://user.it.uu.se/~kostis/Papers/bugs05.pdf
- Falsification：https://arxiv.org/abs/1502.01278
- 错误切片 PEPM'13：https://riunet.upv.es/bitstreams/bf4bc430-0b7a-4c17-bb85-0ba44cf23347/download
- 多态 success types LPAR'18：https://easychair.org/publications/paper/FzsM/open
- 官方文档：https://www.erlang.org/doc/apps/dialyzer/dialyzer.html
- etylizer：https://arxiv.org/html/2302.12783v2
- OTP 源码：https://github.com/erlang/otp/tree/master/lib/dialyzer/src
- LYSE：https://learnyousomeerlang.com/dialyzer
- ElixirLS：https://github.com/elixir-lsp/elixir-ls

## §2 occurrence typing
- POPL'08：https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf
- ICFP'10：https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf
- Castagna POPL'22：https://www.irif.fr/~gc/papers/popl22.pdf
- ifT-benchmark：https://github.com/utahplt/ifT-benchmark
- TS narrowing：https://www.typescriptlang.org/docs/handbook/2-narrowing.html
- Flow refinements：https://arxiv.org/abs/1907.05590
- OTMT PLDI'16：https://dl.acm.org/citation.cfm?id=2908091 （arXiv:1511.07033）
- 元理论再检视 2026：arXiv:2609.16299
- TR 模型：https://github.com/samth/popl08-model
- TR 性能：https://janvitek.org/pubs/jfp18.pdf
- SNAPL'17：https://www2.ccs.neu.edu/racket/pubs/typedracket.pdf

## §3 conditional types + 宏规则
- ALW94：https://doi.org/10.1145/174675.177847
- Jensen POPL'98：http://web.cs.ucla.edu/~palsberg/tba/papers/jensen-popl98.pdf
- Trifonov & Smith：https://flint.cs.yale.edu/trifonov/papers/subcon.pdf
- 软类型两谱系：https://arxiv.org/abs/1307.6239
- Typed Clojure 论文：https://thesis.ambrosebs.com/ 、
  https://ambrosebs.com/files/ambrosebs-phd-thesis.pdf
- Advanced Macrology：https://www2.ccs.neu.edu/racket/pubs/scheme2007-ctf.pdf
- TR ts-guide：https://docs.racket-lang.org/ts-guide/occurrence-typing.html
- TR ts-reference：https://download.racket-lang.org/releases/8.17/pdf-doc/ts-reference.pdf
- Turnstile：https://docs.racket-lang.org/turnstile/
- Type Tailoring：https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2024.44

## §4 semantic subtyping
- FCB LICS'02 全文：https://xml.coverpages.org/Frisch-lics02.pdf
- FCB08 JACM 元数据：https://hal.science/hal-00336120 （DOI 10.1145/1391289.1391293）
- Castagna primer：https://arxiv.org/abs/1809.01427
- Frisch 博士论文：https://www.cduce.org/papers/frisch_phd.pdf
- Kent 教程：https://pnwamk.github.io/sst-tutorial/ （Redex 模型
  https://github.com/pnwamk/sst-tutorial/）
- semtype：https://github.com/jclark/semtype
- Gentle Intro：https://www.cduce.org/papers/gentle.pdf
- ICFP'16 tallying：https://www.irif.fr/~gc/papers/icfp16.pdf
- OCaml ctype.ml（确认无 tally）：https://github.com/ocaml/ocaml/blob/trunk/typing/ctype.ml
- Ballerina semtype 生产移植：https://github.com/ballerina-platform/ballerina-lang/issues/42288
