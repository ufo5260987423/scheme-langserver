# Scheme 宏分析的理论框架纪要

> 目的：在"改进或重做宏分析"之前，把 scheme-langserver 现有的宏分析机制放进 Scheme 宏理论文献的坐标系中。本文回答三个问题：
> 1. 宏分析要解决的问题在理论上是什么？文献给出了哪些定理与不可判定性事实？
> 2. 现有两条机制路线（手写规则 / 通用自动展开）分别对应理论上的什么位置？
> 3. 重做时，开放问题应当以什么理论形式表述？
>
> 文中 `[实现: path:line]` 标注现有代码位置，`[文献: n]` 对应文末书目编号。整理日期：2026-10-02。

---

## 1. 问题的理论陈述

### 1.1 形式化

设源程序 `P` 为未展开的 Scheme 语法对象，`Expand` 为语言的宏展开器，`⟦P⟧ = Expand(P)` 为完全展开产物。记 `B(P)` 为 `P` 中每个标识符出现的**绑定目标**（定义位置/库）的函数——LSP 的 go-to-definition、find-references、hover、补全、unused 检查全部消费 `B(P)` 在源码位置上的投影。

这段话用三个词表解释：

- **"标识符出现"**：源码里同一个名字可能出现多次。`(define (f x) (+ x 1))` 与 `(f 2)` 中，`f` 出现 2 次（1 次定义、1 次调用），`x` 出现 2 次，`+` 出现 1 次；每一次出现都算一个独立的"标识符出现"，各自带有位置坐标。
- **"绑定目标"**：每个出现都要回答"这个名字在这里指的是谁？"，答案有三种典型情况：(1) 指向某处定义——`(f 2)` 里的 `f` 指上面 `(define (f x) ...)` 那个定义；(2) 指向某个库——`(+ x 1)` 里的 `+` 指 `(scheme base)` 库导出的 `+`；(3) 没有答案（未绑定/拼写错误）。
- **"函数"**：`B(P)` 是一个查表函数——输入"程序 `P` + 某个标识符出现的位置"，输出"它的绑定目标"。强调它是 `P` 的函数，是因为绑定关系是整个程序的属性而非单个标识符的属性：同一个名字 `x`，在不同程序里可以指向局部变量也可以指向库导入，单看名字判断不了。

LSP 各项功能与 `B(P)` 的对应：go-to-definition = 查 `B(P)` 并跳到输出的定义位置；find-references = 反查哪些出现的绑定目标是同一个定义；hover = 查 `B(P)` 再取目标处的类型/文档；unused 检查 = 查某定义是否没有任何出现指向它；补全 = 枚举当前位置可见的绑定目标。

**"纯语法函数"是什么意思**：指只看你写的源码文本结构（语法形状）、不看其他任何信息，就能算出结果的函数。对比两个例子——"数一下程序里有几个 `define`"是纯语法函数：只需扫一遍词法和括号结构，不需要运行程序、不需要知道 `define` 的含义；"`(f 2)` 里的 `f` 指的是谁"不是：单看这一行答案不唯一（可能是本文件定义、可能是库导入、可能是拼错），必须借助文本之外的信息（整个文件、import 列表等）。所以"`B(P)` 不是 `P` 的纯语法函数"的意思是：**绑定关系不能从源码文本本身直接读出来，它依赖文本之外的信息**。

宏分析的核心困难：**`B(P)` 不是 `P` 的纯语法函数**，而且宏让这种"文本之外的信息"达到了极致。以 `(try body (except c handler))` 为例：`c` 的绑定目标从源码语法看不出来——`try` 是宏，只有把它展开为 `(let ([condition c]) ...)` 之后，才能看出 `c` 绑定到展开产物中某个 `let` 的左部。也就是说 `B(P)` 的一部分信息只存在于展开之后的程序里，不在用户写的源码里。因此绑定结构的部分信息只存在于展开过程之中。这也是本项目两条路线的必然性所在：要么"真的去展开"（路线二，从展开过程取回信息），要么"额外给分析器一份关于宏的知识"（路线一的手写规则），没有"光读源码就全知道"的第三条路。

### 1.2 文献中的事实（这是整个框架的地基）

**事实 A（否定面）——绑定解析是展开过程的组成部分，不存在独立的源码级判定过程。** R6RS §10 规定"Information about identifier bindings is maintained **during expansion**"；Flatt 的 scope-set 形式化中 `resolve` 被定义为 expander 内部元函数 [文献: 14]。Culpepper 给出了操作性断言：

> "The expander marks introduced identifiers indiscriminately, since it **cannot predict which identifiers eventually become variable bindings**." —— Culpepper 博士论文 (2010), §2 [文献: 12]

对任意过程宏（编译期图灵完备），"展开后某标识符是否处于绑定位置"不可静态预测。**注意：文献中没有发表过严格的不可判定性定理**，以上为领域共识级断言；本项目的 doc 中"undecidable time-consuming" [实现: doc/analysis/identifier.md:110] 是同一意思的工程表述。

**事实 B（肯定面）——在模式化宏的可判定子语言内，绑定可以静态提取且有正确性定理。** Pombrio, Krishnamurthi & Wand 证明：给定核心语言的 scoping 规则与**模式化、组合式**的去糖规则，存在算法 `inferScope` 静态算出表面程序的绑定结构，且**定理 5.4**：推断结果与"先去糖再按核心规则定界"完全一致 [文献: 22]。Fisher & Shivers 的 Ziggurat 从另一方向主张：宏定义应**声明**其静态语义（输入/输出语法上的绑定效应），使分析器无需展开即可组合各层静态语义 [文献: 25]。

**事实 C（知识是渐进的）——绑定知识在展开过程中按 tentative → definite 两级偏序积累。** 展开器每发现一个原始绑定形式得到一条 tentative 箭头（指向所有可能受绑定的同名标识符）；解析到具体引用时转为 definite [文献: 11, 12]。任何在展开中途截断的分析，得到的都是 tentative 级知识。

**事实 D（卫生即绑定历史）——标识符的绑定由其展开历史决定。** Kohlbecker 染色算法（1986）[文献: 1]、marks+substitutions wrap（1992）[文献: 4]、scope-set（2016）[文献: 14] 三代模型都表达同一命题：两个同名标识符是否共享绑定，取决于它们的展开标记/scope 集合，而非词面名字。这解释了为什么"按名字匹配把展开结果映射回源码"必然失败（本项目的实测：Chez `(expand stx)` 输出把局部标识符改名成 gensym）。

### 1.3 框架的第一块：设计空间的两个极点

由事实 A 与事实 B，一切宏分析系统必然落在两极连线上：

```
极点 1：静态推断                    极点 2：运行展开器
（受限宏子语言的绑定规则提取）      （展开时绑定发现 + 结果回传）
ICFP 2017 Thm 5.4 保证 sound        Culpepper contentful expansion
Fisher-Shivers 声明式静态语义       tentative/definite 知识分级
零展开开销，仅覆盖子语言            覆盖任意宏，开销与级联爆炸
```

**scheme-langserver 的两条路线恰好各据一极**，这不是巧合而是问题结构的必然。框架的价值在于：它告诉我们改进的方向不是"选一边"，而是 (a) 扩大静态极的可判定子语言覆盖面，(b) 降低运行极的回传损耗，(c) 在两者之间建立有 soundness 保证的桥。

---

## 2. 形式模型（借用的理论语言）

### 2.1 语法对象与 wrap（Hieb–Dybvig–Bruggeman 1992）[文献: 4]

先用一句话解释**语法对象（syntax object）**：语法对象 = 一段程序文本 + 它的"出身信息"的打包。Scheme 的 reader 有两个档位：`(read port)` 产出普通 s-表达式（裸 datum，只有结构，名字就是名字）；`(read-syntax port)` 产出语法对象，每个标识符额外携带两类信息：(1) **词法上下文**（内部叫 wrap / marks+substitutions / scopes）——记录"这个标识符在哪一次展开、哪个绑定作用域里被读进来"，两个写法相同的名字是否同一个东西由它决定而非由拼写决定；(2) **源位置**——哪个文件第几行第几列，宏展开不丢它，所以展开产物能映射回源码。类比：普通 s-表达式像纯文本（复制后所有副本等价），语法对象像带超链接的 AST 节点——每个节点知道自己的源位置和绑定归属。

语法对象在宏分析中的作用：(1) **绑定判定只能在它上面做**——判断调用点的 `c` 与模板里的 `c` 是否同一绑定，比的是 wrap（`free-identifier=?` 即比较两个语法对象是否解析到同一 label），宏分析的本质工作（求 `B(P)`）天然以它为输入；(2) **卫生展开靠它免费获得**——展开时对输入输出同时打标，两次打标相消，宏引入的语法保留标记，模板临时变量永不捕获用户变量；(3) **源位置使"展开结果映射回编辑器"成为可能**——Racket Check Syntax 只分析完全展开产物却能在原始源码上画箭头，靠的就是全程携带源位置。

**本项目的关键定位**：Chez 的 `expand` 输出丢弃这层信息（annotation 丢失、局部标识符被 gensym 改名），项目无法直接消费原生语法对象，于是用 `index-node` 手工重建其等价物——`index-node` ≈ 去掉了 wrap 的语法对象，wrap 承担的"绑定判定"职能由 `identifier-reference` 网络取代（见 §4、§6-Q5）。

形式定义：语法对象 = `(expression, wrap)`；`wrap = (marks, substitutions)`，substitution 记 `(id ↦ label, mark*)`。绑定解析为两级映射：

```
wrap : id  → label        （标识符经 wrap 解析为 label）
env  : label → binding    （label 在展开环境中查到绑定）
```

宏展开对输入与输出同时打标（两次打标相消，新引入语法保留标记）——卫生由此免费获得。展开环境 `Env = Sym → Transformer | Variable | Pattern-Variable | Special`。

【论文级形式化】Hieb–Dybvig-Bruggeman (1992) 的原始记法（转述，符号经统一简化）：

```
语法对象与 wrap 的语法：
  sobj  ::= atom | (sobj . sobj) | (wrap e w)
  w     ::= (m* . s*)                  ; marks 与 substitutions 的有序序列
  s     ::= (name label m*) | shift    ; shift：内部定义边界的代数标记
  label ::= 每次绑定生成的新鲜名字

标识符解析 resolve（标识符经 wrap 映到 label）：
  resolve(x, (m* . s*)) = 自最新向最旧扫描 s*，
    取第一个 name = x 且其标记与当前 m* 相容的条目的 label；
    扫描落空 → 按名字查全局/词法环境（顶层情形），视为 global binding

展开器 expand 的两条关键规则（伪代码级）：
  expand((lambda (x₁ ... xₙ) body), r) =
      令 l₁..lₙ 为新鲜 label，r' = r[xᵢ ↦ binding(lexical, lᵢ)]
      (lambda ((wrap xᵢ ((lᵢ)))) (expand(body, r')))
  expand((m e ...), r) =                    ; m 在 r 中绑定为 transformer
      令 γ 为新鲜 mark，t = M(add-mark(γ, (e ...)))   ; M = transformer 过程
      expand(add-mark(γ, t), r)               ; 输出同样打标，两次打标相消即 flip

两个标识符相等性谓词（即 free-identifier=? / bound-identifier=? 的语义）：
  x ≡free y  ⟺ resolve(x) = resolve(y)
  x ≡bound y ⟺ x、y 词面同名且携带相同 marks
```

**对本项目的意义**：`identifier-reference` 的 `(identifier, library-identifier, type)` 三元组就是 `binding` 记录的手工重建；本项目的 `available-identifiers`（随 `step` 传递的引用表）是 `env` 词法部分的近似——区别在于论文的 `env` 由 expander 维护且随展开逐级扩展，本项目的由抽象解释器在 `index-node` 树上自顶向下维护。

### 2.2 scope-set 视角（Flatt 2016）[文献: 14]

先用一句话解释**scope-set**：给每个标识符挂一个"它见过的所有作用域"的集合，用集合之间的子集关系判定绑定——它是 Racket 现行 expander 的卫生模型，是 §2.1 marks+substitutions 的现代替代品。

核心机制三步：(1) 每个引入作用域的构造发一个唯一的 scope（相当于不可伪造的印章）——每个宏调用一个、每个绑定形式（`let`/`lambda` 参数表）一个、每个模块一个；(2) 标识符记录自己携带的全部 scope，如 `(let ([x 1]) (+ x x))` 里的 `x` 只带 let 发的那一个 scope `s1`，即 `x + {s1}`；(3) 绑定解析 = biggest-subset 规则：维护全局表 `Σ : 名字 → (scope 集合 → 目标)`，引用查表时在同名且其 scope 集为引用 scope 集**子集**的候选中取**最大者**——集合适配意味着"该引用见过产生该绑定的作用域"。

宏展开 = 往输入标识符上加宏调用自己的 scope 再做对称差：模板引入的标识符保留标记（永不等于用户标识符），用户传入的标识符标记相消还原。卫生由此获得，且对比 1992 模型的 marks+替换链双结构，scope-set 只有一个集合；它还是**纯数据结构算法**（集合加子集判定，不依赖 expander 内部过程），语义已被证明与 marks/wrap 模型等价（ICFP 2017 §3.4），Go 的 wile、Lean 4 均有独立实现先例。

形式定义：绑定存储 `Σ : name → (scope-set → name)`；解析规则 = 在名字相同且 scope 集为引用 scope 集**子集**的候选中取**最大者**（biggest-subset）。scope 集在包含序下构成 join-semilattice。

【论文级形式化】Flatt (POPL 2016) 的 Redex 风格定义（转述，符号经统一简化）：

```
scope   ::= 不透明原子值（每处绑定形式、每次宏调用各发一个）
x@scs   ::= 携带 scope 集合 scs 的标识符 x
Σ       : name → (scope-set ⇀ name)          ; 绑定表（部分函数）

解析元函数（biggest-subset）：
  resolve(x@scs, Σ) = y
    其中 Σ(x)(scs') = y，scs' ⊆ scs，
    且对一切满足同名且 scs'' ⊆ scs 的候选有 scs'' ⊆ scs'（取最大子集）

展开的两条核心规则：
  (lambda (x@scs ...) e) ⟹ (lambda (x@scs∪{ς} ...) e∪{ς})
      ς 为 fresh scope（绑定形式发 scope，体内所有标识符继承）
  (macro-app M e) ⟹ flip(M(flip(e, ς)), ς)
      ς 为本次宏调用的 fresh scope
      flip(x@scs, ς) = x@(scs △ {ς})          ; 对称差：有则删、无则加

绑定表扩展（展开遇绑定形式时）：
  见 (lambda (x@scs) ...) 时 Σ' = Σ[x ↦ (scs ↦ fresh-name)]
```

**对本项目的意义**：项目目前用"文档级"近似（expander-doc fallback，§4.3）做跨文档绑定解析，因为拿不到 Chez 内部的 label 环境；而 scope-set 说明绑定解析可以用纯数据结构从头实现，不必依赖宿主 expander 暴露内部 API。若重做时需要一个语义精确的自有绑定解析器，scope-set 是唯一既有完整形式规范、又有多个独立实现先例的路线（详见 §6-Q5）。

### 2.3 两级绑定知识（Culpepper 2010）[文献: 11, 12]

先用一句话解释**两级绑定知识**：展开器是"边展开边发现绑定"的，发现的先后不同导致绑定知识天然有两种置信度。

以 `(my-let ([x 1]) (+ x x))` 为例：展开器把它展开成 `(let ([x 1]) (+ x x))` 后，**看到 `let` 的那一刻**就知道"`x` 有一个绑定"——但还不知道 `body` 里两个 `x` 是否归它（可能有遮蔽、可能有 hygiene 改名）；要等展开推进到引用处、把引用解析到具体绑定时才算数。由此分两级：**tentative（推测）** = 绑定已见、引用候选未消歧（DrRacket 画紫色带问号的箭头）；**definite（确定）** = 引用已解析到具体绑定（蓝色箭头）。两个细节：用 `datum->syntax` 等卫生破坏原语制造的引用，展开器永远无法预知其来源，只能永远停在 tentative 级；箭头两端的判定硬约束是必须 `free-identifier=?` 等价（共享 binding），不能按词面名字。

对静态分析的含义：任何不跑完整个展开过程的分析工具，拿到的都只是 tentative 级知识；工具应当向用户区分"确定"与"推测"，而不是把推测当真。

形式表述：

```
definite arrow: 引用已解析到具体绑定        → 蓝色（确定）
tentative arrow: 绑定已见、引用候选未消歧   → 紫色带问号（推测）
```

判定硬约束：箭头两端必须 `free-identifier=?` 等价（共享 binding），不能按词面名字。

【论文级形式化】Culpepper (2010) 对绑定箭头的定义（转述）：

```
展开轨迹 T = 展开器处理的（源式, 展开结果）有序序列

definite(b, r)  ⟺  b 是绑定出现 ∧ r 是引用出现 ∧ expander 对 r 的解析结果 = b
tentative(b, r) ⟺  b 是 T 中已发现的绑定出现 ∧ r 是源码中与 b 同名的引用出现
                     ∧ 展开尚未推进到 r 的解析

单调性不变式：definite(b, r) ⟹ tentative(b, r)
  （知识只增不减：tentative 随展开推进或转为 definite，或随候选被排除而撤销）
推论：datum->syntax 等卫生破坏原语产生的引用，来源不可预知，
      只能永远停留在 tentative 级
```

**对本项目的意义**：现有自动路线把 tentative 级知识直接当作 definite 使用（见 §4.2、§5），这是已知的精度隐患而非 soundness 违规——但项目目前没有区分两级，重做时应把"置信度"作为引用的一等属性。

### 2.4 相位与库（Ghuloum–Dybvig 2007；Flatt 2002）[文献: 8, 9]

先用一句话解释**相位（phase）**：Scheme 的代码不止在一个"时间"运行——程序本身在运行期跑，而宏 transformer（`(define-syntax m (lambda (e) ...))` 里的 lambda 体）在**宏展开期**跑；如果 transformer 自己还用了宏，那就有展开期的展开期（meta 层）。同一个名字在不同相位可以是完全不同的东西：运行期的 `helper` 是个普通函数，展开期叫 `helper` 的可能是另一个函数，两者互不相干。库的 import/export 因此都要携带"我在哪些相位可见"的信息。

**隐式相位**（Ghuloum–Dybvig 对 R6RS 库系统的形式化）：程序员不需要手写 `for expand` 之类的标注——编译器从库依赖图出发，归纳推导出每个库需要在哪些相位被实例化、求值。

**部分展开程序**（Flatt 2002）：一个模块可以先展开到"半完成"状态（核心形式 + 少量延迟的宏调用 + 已知的编译期绑定）并保存/传输，与后续模块组合时不必从源码重新展开——这是增量编译与缓存能成立的理论前提。

形式表述：R6RS 库的 import/export 携带相位集合；相位推导算法由库依赖图归纳传播每个库的实例化相位。部分展开程序可序列化、与后续模块组合而无需重新展开——**增量分析与 workspace 缓存的理论前提**。

【论文级形式化】Ghuloum–Dybvig (ICFP 2007) 的相位推导（转述，符号经统一简化）：

```
库形式：(library (name) (export e ...) (import i ...) body)
import 说明：i ::= (lib-name level-shift ...)
                 ; (for expand) ⇔ shift = 1，(for run) ⇔ shift = 0

判定规则（简记）：
  (import (A) shift p) ∈ L ∧ L 在相位 q 使用 A 的绑定 x
      ⟹ x 在 A 内部于相位 q − p 处定义/求值
  L 的 transformer（相位 1 的代码）引用 B 的绑定
      ⟹ B 必须在相应移位后的相位被实例化（约束沿依赖图传递）
  A 在相位 p 被实例化 ⟹ A 依赖的库按各自 shift 累计传播实例化需求

求值：所需实例化相位集合 = 上述约束的最小不动点；
论文证明隐式推导完备：不加显式 for-expand 标注也不会丢失必要实例化。
```

**对本项目的意义**：`file-linkage` 矩阵 + `library-node` 树是库依赖图；`expanded+callee-list` 中的 `expander-doc` 槽是"跨文档/跨相位绑定解析"的 ad-hoc 实现（见 §4.3）。

### 2.5 源–目标相关（Hieb et al. §4.5；resugaring）[文献: 4, 20, 21]

先用一句话解释**源–目标相关**：展开前的代码与展开后的代码之间的位置对应关系——展开产物里的每个东西知道自己来自源码哪里。

正向的形式规则（Hieb–Dybvig-Bruggeman §4.5 "source-object correlation"）：

```
expand(source(e, a), r) = source(expand(e, r), a)
```

读作"展开应保持源位置"：报错能定位到宏调用点而不是展开后的内部代码，靠的就是这条性质在实现里成立（实现方式 = 给语法对象加一个字段携带位置；Guile 的 psyntax 2021 年按此改造成功，有完整 diff 可查）。

**Resugaring（反糖化）**研究的是**反向**：把核心语言的求值序列/分析结果提升回表面语法。它给出两条正确性准则——**Emulation**（表面层的每一项，去糖后与对应的核心项同构）与 **Abstraction**（去糖引入的代码永不在表面层泄露）。这两条正是评价"展开结果映射回源码"的一切方法的准绳。

Resugaring 准则的定理形式（Pombrio–Krishnamurthi, PLDI 2014，转述）：

```
设 D 为去糖函数，S = s₁ ⇒ s₂ ⇒ ... 为表面求值序列，C = c₁ ⇒ c₂ ⇒ ... 为核心求值序列
  Emulation：   ∀i. ∃j. D(sᵢ) = cⱼ
               （每个表面步去糖后同构于某个核心步）
  Abstraction： ∀i. sᵢ 中出现的语法均来自原始源码
               （去糖引入的代码永不在表面序列泄露）

Source-object correlation 的分配形式（Hieb et al. 1992 §4.5）：
  expand(source(e, a), r) = source(expand(e, r), a)
  且对一切核心构造子 f：
  f(source(e₁, a), ..., source(eₙ, a)) = source(f(e₁, ..., eₙ), a)
```

**对本项目的意义**：Chez 的 `expand` 输出丢弃全部 annotation、局部标识符被 gensym 改名——源–目标相关在 Chez 上是**断的**。项目被迫自建对应（pretty-print 展开结果 → 重新 parse → 节点配对），这正是 resugaring 问题的工程形态，应以其准则评价自建对应的质量（见 §5、§6-Q3）。

### 2.6 模式宏的绑定静态提取（Pombrio et al. 2017）[文献: 22]

先用一句话解释**绑定静态提取**：对一大类"模式匹配-模板替换"式宏（syntax-rules 的主体），绑定结构**不需要展开就能算出来**，而且有定理保证算得和"先展开再定界"一模一样。

思路：把每条糖规则（pattern → template）翻译成关于 scope 的约束（模板里哪个位置绑定、哪个位置引用、哪个位置导入/导出名字），在最小不动点下求解这些约束，得到一套表面语言的 scoping 规则，再用 `checkScope` 验证一致性。论文**定理 5.4**保证：用推断出的表面规则定界，与"先去糖、再按核心语言规则定界"的结果完全一致。论文的应用目标写得很直白——把推断出的规则装进编辑器，画出 Check-Syntax 式的绑定箭头。

**对本项目的意义（原评估）**：本项目的手写规则（`match-process` 等）本质上就是人工编写的 scope 声明——该论文提供了把它们**机械化生成 + 验证**的路径：对任意 syntax-rules 宏自动生成声明、验证通过则用（零展开开销的精确解），不通过再回退到展开路线。这就是 §6-Q1 的理论支柱。

**适用范围勘定（2026-10-07，经全文核实）**：该论文的场景是**语言规范定义的糖**——语言设计者给出核心语言的 scoping 规则 + 模式化去糖规则，算法把核心规则提升到表面；对标物是 DrRacket 给**内建形式**画 Check-Syntax 箭头的功能，不是宏系统。论文确有 hygiene 章节（§7，防糖引入捕获），但两个理由使其不能作为本项目的宏线杠杆：① 输入形式化（设计者保证的规范糖）与用户宏世界不匹配——我们的分析对象是任意 syntax-rules 宏的**展开产物**，拿不到这一前提；② 论文自己的案例研究即暴露算法边界：Scheme `do` 的绑定列表（绑定在 step 可见、在 init 不可见）**推断失败**（§6.4 原文 "we cannot infer scope for this macro"），named-let 情形还允许 let 绑定遮蔽函数名——这恰是真实宏家族的主导形状，与本项目 P5 实测（CPS/级联宏绑定在终点物化、match 家族静态提取覆盖 0%）收敛。**结论：对 scheme-langserver，[22] 的对应物是非宏功能**——30+ 个手写真特殊形规则（let/lambda/cond/do……）正是"语言规范糖"的角色，理论上可用其框架机械化生成/验证；宏线（Q1/P5）的机械化期望随之下调。

与项目代码的具体对应关系：

| 论文（ICFP 2017） | syntax-rules | 本项目 |
|---|---|---|
| 去糖规则 `lhs → rhs`（带洞） | clause：`pattern → template` | 一个宏的一条子句 |
| 洞（hole） | pattern variable | compound-list 中的替换位 |
| `bind(π)`：位置 π 声明新名字 | 模板中绑定形式出现的 pattern 变量 | `match-process` 里"裸 symbol → 构造 identifier-reference" |
| `import(π, n)` / `export(π, n)`：名字从第 n 个洞流入/流出 | 参数透传（如 `try` 的 handler 体里的名字来自调用点） | `pattern+index-node->pair-list` 干的事 |
| 核心语言规则 Γ_core（**输入**） | — | `analysis/identifier/rules/` 下的 let/lambda/define 等核心形式规则 |
| 推断出的表面规则 Γ_sugar（**输出**） | — | 本应由 `self-defined-rules/` 手写编码的东西 |

注意：`analysis/identifier/rules/` 下的规则分两类——核心形式规则（let、lambda、define……）在论文框架里是 Γ_core，是算法的**输入**（已知、不用推）；`self-defined-rules/`（match、try……）才是算法**输出**的对应物。即 `inferScope(ufo-match 的全部子句 + Γ_core) ⇒ 一套等价于 match-process 语义的声明`。

使用该算法的四个前提（否则会误用）：

1. **只覆盖"模板实例化即全部语义"的宏**。syntax-case 的 guard（fender）是任意代码、过程式 transformer、`datum->syntax` 玩花活的宏，超出论文的基础片段（其 §6.3 给了可判定性限制清单）；对这些只能回退到"真展开"路线或继续手写。
2. **级联宏要把整条链的规则都给它**。`match → match-one → match-two → …` 每一环都是一条去糖规则，全部作为输入；任何一环 `checkScope` 失败，整个宏回退。这正好给项目现在"嵌套宏拒绝"的 no-op 机制一个理论化判据：拒绝与否从"模板里有没有 define-syntax"变成"checkScope 过不过"。
3. **输出粒度是 scope 结构，不是 identifier-reference**。算法只告诉你"位置 π 绑定名字、引用解析到谁"，不直接产出带 document/index-node/type 的引用记录——需要一层机械翻译（类似现在 pattern.sls + shallow-copy 的角色），但这层是确定性的、无需启发式。
4. **它解决 soundness，不解决调度**。推出来的规则使用时是零展开的（极点 1），§6-Q2 的级联/缓存问题对它不存在；但"哪些宏走推断、哪些走展开"是调度问题，该论文不提供答案。

一句话：**手写规则 ≈ inferScope 的手工实例化；Q1 就是把 `self-defined-rules/` 从"人写"升级成"机器推 + 定理保证 + 验证不过再回退"。**

形式表述：对模式化去糖规则，`inferScope` 由 RHS 已知 scope 生成约束、最小不动点求解、`checkScope` 验证；定理 5.4 保证与"先去糖再定界"一致。其 scope 规则声明语言（bind/import/export 谓词按产生式给出）可直接充当"手写规则"的理论形态。

【论文级形式化】Pombrio–Krishnamurthi–Wand (ICFP 2017)（转述，符号经统一简化）：

```
scope 结构 = 变量出现上的 preorder ≼
  d ≼ u 读作"声明 d 的效力到达使用 u"
  遮蔽（shadowing）= 同一名字的两个声明在 ≼ 下可比较：靠内者优先

糖规则：lhs → rhs，位置带谓词标注
  bind(π)      ：产生式 π 位置声明新名字
  import(π, n) ：π 位置的名字由第 n 个洞导入
  export(π, n) ：π 位置向第 n 个洞导出名字

inferScope 算法：
  1. 约束生成：对每条规则，rhs（核心侧，scope 已知）上洞与洞之间的
     可达路径，要求 lhs 对应洞之间保持同路径（否则必然 unbound/capture）
  2. 候选规则 = 约束系统的最小不动点
  3. checkScope：验证 ≼ 为 preorder（自反、传递）且每个名字的声明全序

定理 5.4（一致性）：设 D 为去糖、Γ_core 为核心 scope 规则、Γ_sugar 为推断规则，
  对任意表面程序 p：Γ_sugar ⊢ p : ok  ⟺  Γ_core ⊢ D(p) : ok
  （直接在表面检查绑定 等价于 展开后按核心规则检查）
```

---

## 3. 分析框架：宏分析的四要素

综合以上文献，任何宏分析系统可分解为四个正交要素。这既是评价现有实现的检查表，也是重做时的模块边界建议：

| 要素 | 理论对应 | 问题 |
|---|---|---|
| **E1 绑定知识来源** | 静态推断 [22, 25] vs 展开时发现 [11, 12] | 知识从哪来？ |
| **E2 展开能力表示** | transformer / generator（contentful）[12] | 宏定义被分析器看成什么？ |
| **E3 中间表示与遍历** | 语法对象+wrap [4]、scope-set [14]、nanopass IL [7] | 分析在哪个 IR 上进行？ |
| **E4 源映射与回传** | source correlation [4]、resugaring [20, 21] | 展开产物如何投影回源码？ |

现有实现的全景映射见下节。

---

## 4. 现有机制 → 理论框架的映射

### 4.1 路线一（生产在用）：手写规则 = 极点 1 的实例

`analysis/identifier/self-defined-rules/` 下手写 processor（`match-process`、`try-process`、`let1-process` 等）**不展开宏，直接按模式的绑定语义声明引用**。以 `match-process` [实现: analysis/identifier/self-defined-rules/ufo-match/match.sls:11-45] 为例：pattern 中的裸 symbol 直接在 pattern 节点上构造 `identifier-reference`（type `'variable`）并三处附着——这是对 `ufo-match` 模式语言**手工编写的 bind 谓词语义**。

| 框架要素 | 手写规则的形态 | 理论判词 |
|---|---|---|
| E1 知识来源 | 人工静态声明（不展开） | 对应 inferScope 的手写实例 [22]；soundness 依赖作者对宏语义的正确理解 |
| E2 展开能力 | 无需表示（规则即语义） | 对应 Fisher-Shivers 的"声明式静态语义" [25] |
| E3 中间表示 | 原始 `index-node` AST | 分析直接在表面语法上进行 |
| E4 源映射 | 恒等（引用直接附着源码节点） | 完美的 Emulation/Abstraction——这是该路线最大的理论优势 |

**结论**：手写路线在理论上是**精确解**（无近似、无回传损耗），其代价是 each-宏族的劳动，且 soundness 无机械保证（取决于规则作者）。

### 4.2 路线二（实验性，已停用）：通用自动展开 = 极点 2 的实例

数据流：`define-syntax:attach-generator`（E2 定义期）→ `router.sls` 调度（被注释）→ `expansion-generator->rule`（E1+E3 调用期）→ `private:shallow-copy`（E4 回传）。

**E2 展开能力表示 = contentful expansion 的雏形。** `private:find-generator` 从 `(syntax-rules ...)`、`(syntax-case ...)`、`(lambda (x) (syntax-case x ...))` 三种形态提取 generator 闭包，挂到宏关键字的每个 `identifier-reference-syntax-expander` 上 [实现: analysis/identifier/rules/define-syntax.sls:19-61]。这正是 Culpepper "contentful expansion" 的核心思想——**不修改语言，把宏 transformer 换成记录展开轨迹的载体** [文献: 12]。差别：Culpepper 记录的是展开轨迹（匹配到的子句、模式变量绑定），本项目记录的是可重放的 generator 闭包。

**E1 知识来源 = 展开时绑定发现。** `make-generator-for-clauses` [实现: analysis/identifier/expanders/syntax-rules.sls:108-146] 的算法：

1. `private:confirm-clause`：构造 `(syntax-case ',input ,literals ...)` 交给**宿主 Chez 的 syntax-case** 匹配选子句 [实现: syntax-rules.sls:216-233]——即借 Hieb-Dybvig expander 的 `$syntax-dispatch` 做 clause 选择（Clinger-Rees 匹配的一端 [文献: 3]），前加 `private:signature-match?` 保守签名过滤（性能优化，保证"不错杀"语义）；
2. `make-pattern`/`gather-context`/`pattern+index-node->pair-list`：按 R6RS 11.19 把 pattern 解析为十类节点（`underscore/pattern-variable/literal-identifier/list-form/pair-form/ellipse-list-form/ellipse-pair-form/vector-form/ellipse-vector-form/equal?-datum`）[实现: pattern.sls:33-97]，建立 pattern ↔ 调用点 index-node 的 pairs——这是**输入侧**的 source correlation；
3. `generate-binding` + 协程 iterator：pattern 变量 → 调用点节点的绑定映射（ellipsis 层级由 `dive-into-an-ellipsed-form` 的 level 整数标记，对应 Clinger-Rees 的 ellipsis depth [文献: 3]）；
4. `expand->index-node-compound-list`：模板替换，pattern 变量代换为调用点节点（compound-list 可含 index-node/list/pair/vector/symbol）；
5. pretty-print 后经 `source-file->annotations` 重新 parse 成 `expansion-index-node`——**输出侧的 source correlation 重建**；
6. `private:expansion+index-node->pairs`：展开树 ↔ compound-list 配对（`private:take` 截断容错）。

**第一层展开的语义正确性已被回归测试锚定**：与 Chez `(expand ...)` 的第一步一致 [实现: tests/analysis/identifier/test-match-expansion-compare.sps:88-90]——这等价于宣称 route 2 的展开符合 Hieb-Dybvig 语义 [文献: 4]。

**E3 中间表示与递归。** `expansion-generator->rule` [实现: analysis/identifier/expanders/expansion-wrap.sls:18-44] 把 generator 包装成 `step` 规则：

1. 调用点表达式树含 `...` → 放弃展开（`private:tree-has?`）；
2. 调 generator 得 `pairs+expansion`；
3. 构造新条目 `(expansion-index-node callee-node expander-doc pairs new-all-pairs)` 推入 `expanded+callee-list`（5 元素新格式）；
4. **递归守卫**：`(and (not (contain? memory expression)) (< (length memory) 15))`——memory cap 15，防级联宏家族（match→match-next→...）展开风暴；
5. 对展开 AST 递归 `step`——**分析在"半展开程序"上进行**，这正是 Flatt 部分展开概念的分析侧用途 [文献: 9]。

**E4 回传 = 箭头提取。** `private:shallow-copy` [实现: expansion-wrap.sls:139-206]：

1. `build-reverse-map`（eq-hashtable）：展开节点 → 原文/compound 节点；
2. `private:recursive-collect` 收集展开树的 export/import 引用；
3. 按 pairs/all-pairs 定位每个展开节点的对应原文节点，**新建** `identifier-reference` 附着回去（注意：是新建引用而非移动——展开树与原文的引用网络并行存在）；
4. `private:sync-to-parent-expansion` 向父层展开树反向同步，支持级联（cascade）。

理论上这就是 **Culpepper 箭头提取算法 + resugaring 的复合** [文献: 12, 20]；已知限制（§5）正是两处文献准则未被满足的地方。

### 4.3 跨文档解析：expander-doc fallback = 隐式相位的 ad-hoc 实现

`private:find-available-references-for` [实现: abstract-interpreter.sls:273-312] 与 `private:find-expander-doc-for-node` [:263-271]：当节点位于某次宏展开内部时，符号解析 fallback 到 **expander 所在文档**（宏定义文件）的引用表。类型推断侧有一份同构复制 [实现: type/substitutions/generator.sls:164-192]，且类型替换的规则分派显式排除 `'parameter`/`'syntax-parameter` 引用 [实现: generator.sls:156-162]（pattern 变量不进类型替换）。

理论上这对应"宏定义体的标识符在定义点词法环境中解析"——但实现是**文档级**而非**展开期环境级**的近似（真正的 `env: label → binding` 是瞬时的，见框架要素 E1 的注释；这正是 Racket `identifier-binding` 与 Chez 的差距）。

### 4.4 两路线对比的理论重述

doc/analysis/identifier.md:478-485 的对比表可翻译为：

| | 手写规则（极点 1） | 自动展开（极点 2） |
|---|---|---|
| E1 知识 | 人工声明，static，sound 靠人 | 展开发现，运行期，第一层有测试锚定 |
| E4 源映射 | 恒等（完美） | shallow-copy 近似（单层，见 §5） |
| 复杂度 | O(AST) 单遍 | 展开成本 × 级联深度（cap 15） |
| 覆盖 | 子语言（已写规则的宏族） | 任意挂 generator 的 syntax-rules/syntax-case |
| 知识置信度 | definite | tentative 直接当 definite 用 |

---

## 5. 与理论对照：现有实现的性质账本

**已成立的性质：**

- 第一层展开的语义正确性：与 Chez syntax-case 一致（回归测试锚定）。
- 手写规则在其覆盖面上：精确的 definite 级绑定，完美源映射。
- 自动路线的引用回传：不会把展开树引用移动到原文（新建引用），原文 AST 不被污染。

**已知的近似与洞（理论归因）：**

1. **shallow-copy 单层限制** [实现: expansion-wrap.sls:139-206; doc: identifier.md:534-569]——最深层的 pattern 变量绑定回不到原始宏调用节点。理论表述：回传只满足**单层的** Emulation，多层 resugaring 需要 Pombrio-Krishnamurthi 的 DAG 表示与重复子项处理 [文献: 21]。
2. **ellipsis 节点对应不完备**——`expansion+index-node->pairs` 靠 `private:take` 截断容错，不理解 ellipsis 语义；`expansion-generator->rule` 对含 `...` 的调用点直接放弃。理论表述：ellipsis depth 拼接（Clinger-Rees [文献: 3]）未在输出侧实现，对应关系退化为启发式。
3. **嵌套宏拒绝**——`private:template-has-nested-macro?` 对模板含 `define-syntax` 的宏装 no-op generator（match-two 等因此断链）。理论表述：宏生成宏（macro-generating macros）超出当前 E2 表示的表达能力，是事实 A 的成本显形。
4. **memory cap = ad-hoc 终止参数**——15 是魔法数（doc 记 10，代码已改 15）。理论表述：级联展开的终止性无文献指导；Bove-Arbilla 的合流定理 [文献: 17] 保证展开次序不影响结果，但不给深度界。
5. **tentative 当 definite**——回传引用与手写引用无置信度区分（LSP 服务本身带推测性，此处仅作理论记录）。
6. **展开结果重新 parse 的 fidelity 损耗**——pretty-print → `source-file->annotations` 假设展开输出可被忠实打印/重读；宏输出中含非可打印对象或读写不对称语法时该假设破裂（`expansion->printable-object` 是补丁）。

---

## 6. 开放问题的理论化表述（供重做时参考）

**Q1 可判定子语言的边界划在哪？**
手写规则的实际覆盖面 = "模式守卫组合可直读的宏"。ICFP 2017 的 `inferScope` 算法 [文献: 22] 表明这一覆盖面可以**机械化扩展**：对任意 syntax-rules 宏自动生成 bind/import/export 声明并验证（checkScope），不满足约束时回退到展开路线。这把手写规则的"soundness 靠人"变成"soundness 靠定理 + 验证器"。**边界（2026-10-07 勘定，见 §2.6）**：[22] 的算法对 do 式绑定列表（绑定对部分子表达式可见）自认失败（论文 §6.4），而真实宏家族正由此类形状主导（P5 实测 match 家族 0% 覆盖）——Q1 的机械化扩展应视为"仅覆盖可直读宏的语法部分"；[22] 对本项目真正成立的落点是**非宏特殊形规则**（let/lambda/cond/do……）的机械化生成与验证，即把我们手写的 language-spec 糖规则换成"核心规则 + 去糖规约 + 推断器"。

**Q2 级联展开的控制能否有 principled 方法？**
现状 = 全量 eager 展开 + memory cap。文献无直接答案，但三个理论资源：(a) 宏隐藏（macro hiding，Racket macro-debugger）——已知宏家族按白名单 opaque 化，等价于"极点 1 规则优先、极点 2 兜底"的分层调度；(b) 按需展开（lazy/partial expansion, [文献: 9]）——只在符号解析失败处触发；(c) 展开结果缓存 keyed by (宏定义, 调用点形状)——部分展开程序的序列化 [文献: 9] 是其理论形态。

**Q3 回传的 soundness 准则。**
应为 scheme-langserver 定义自己的两条准则（仿 [文献: 20]）：

- **Emulation**：回传到原文节点的每个引用，其在展开树中的绑定来源可追踪（当前：经由 pairs 链部分满足）；
- **Abstraction**：展开树节点绝不出现在用户可见的 LSP 结果中（当前满足——回传一律新建引用附着原文节点）。

shallow-copy 单层限制的突破 = 需要 multi-layer resugaring；Pombrio 2015 的 DAG 路线 [文献: 21] 与项目已有的 `shared-reference` 去环机制 [实现: virtual-file-system/index-node.sls:90-110] 在结构上同源。

**Q4 ellipsis 的源对应能否完备化？**
输入侧（`pattern+index-node->pair-list`）已按 R6RS 模式对齐；输出侧缺 ellipsis 感知的配对算法。Clinger-Rees 的 depth 拼接 [文献: 3] 与 Guile psyntax 的 source 透传 diff（2021-02 commits）提供了两个可抄的实现蓝本。

**Q5 跨文档绑定解析的语义精度。**
expander-doc fallback 是文档级近似。若要语义精确，需要 scope-set 模型 [文献: 14] 级别的环境表示（label 级而非文档级）；ICFP 2017 §3.4 已证明 scope-set 语义与 marks/wrap 等价，故可在不依赖 Chez expander 的前提下实现（wile、Lean 4 先例）。

**Q6 类型推断与宏分析的耦合。**
类型替换生成复用宏展开的 expander-doc 映射，并以 `'syntax-parameter` 类型排除 pattern 变量。理论视角：多阶段计算 [文献: 19] 把宏展开看作编译期阶段，类型分析是运行期阶段，"绑定时间分析"（binding-time analysis）是调度两者的形式工具；Pombrio 2018 [文献: 23] 进一步表明类型规则本身也能从糖规则回推——scheme-langserver 的 rnrs-meta-rules 手写类型规则正是其手工实例。

**Q7 工程现实约束。**
（本轮调研记录，非理论问题）Chez `(expand stx)` 输出丢 annotation、局部标识符被 gensym 改名、无 `identifier-binding` 等价 API——源–目标相关必须自建（§2.5）。在此约束解除前，任何"直接消费原生展开产物"的设计都不可行；自建对应的质量应以 Q3 准则验收。

---

## 7. 结论

1. 现有两条路线不是历史包袱，而是问题结构（事实 A vs 事实 B）的必然显形；重做时应保留"两极并存、显式调度"的骨架。
2. 理论文献给了三件现有实现尚未用上的武器：**可机械化验证的 scope 推断**（Q1）、**resugaring 正确性准则**（Q3）、**部分展开的调度理论**（Q2）。
3. 改进的优先级建议（按 soundness 收益/工作量比）：Q1（自动验证的手写规则生成）> Q2c（展开缓存）> Q3 准则落地（把单层限制从"已知 bug 列表"变成"可度量的性质"）> Q4/Q5（完备化）。

---

## 8. 文献索引

### 卫生宏与展开算法

1. Kohlbecker, Friedman, Felleisen, Duba. *Hygienic Macro Expansion*. LFP 1986. <https://doi.org/10.1145/319838.319859> — 原始染色算法；绑定 = 展开历史的函数。
2. Bawden, Rees. *Syntactic Closures*. LFP 1988. <https://doi.org/10.1145/62678.62687> — 标识符 = 符号 + 词法环境。
3. Clinger, Rees. *Macros That Work*. POPL 1991. <https://doi.org/10.1145/99583.99607> — 模式宏、ellipsis depth、线性时间"边展开边重命名"算法。
4. Hieb, Dybvig, Bruggeman. *Syntactic Abstraction in Scheme*. Lisp and Symbolic Computation 5(4), 1992. <https://doi.org/10.1007/BF01806308> — syntax-case 形式化：wrap/marks/substitutions、expand 伪代码、§4.5 source-object correlation。
5. Dybvig. *Writing Hygienic Macros in Scheme with Syntax-Case*. Indiana TR 356, 1992. — `free-identifier=?`/`bound-identifier=?`/`datum->syntax` 的原语级语义。
6. Dybvig. *The Scheme Programming Language*, 4th ed., Ch. 9, 2009. <https://www.scheme.com/tspl4/syntax.html> — syntax-case 可操作规范。
7. Sarkar, Waddell, Dybvig. *A Nanopass Infrastructure for Compiler Education*. ICFP 2004. <https://doi.org/10.1145/1016848.1016878>；Keep. *A Nanopass Framework for Commercial Compiler Development*. PhD diss., 2012. <https://andykeep.com/pubs/dissertation.pdf> — IL 形式规格方法论。
8. Ghuloum, Dybvig. *Implicit Phasing for R6RS Libraries*. ICFP 2007. <https://doi.org/10.1145/1291151.1291197> — 相位推导。
9. Flatt. *Composable and Compilable Macros: You Want it When?* ICFP 2002. <https://doi.org/10.1145/581478.581486> — 部分展开程序、相位分离。

### 绑定知识的提取与工具化

10. Flatt, Culpepper, Darais, Findler. *Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts*. JFP 22(2), 2012. <https://doi.org/10.1017/S0956796812000093> — 定义上下文、内部定义的增量环境扩展。
11. Culpepper, Felleisen. *Debugging Macros*. GPCE 2007. <https://doi.org/10.1145/1289971.1289994>（前身：*A Stepper for Scheme Macros*, Scheme Workshop 2006；扩版：*Debugging Hygienic Macros*, SCP 75(7), 2010）— tentative/definite 绑定箭头。
12. Culpepper. *Refining Syntactic Sugar: Tools for Supporting Macro Development*. PhD diss., Northeastern, 2010. <https://repository.library.northeastern.edu/files/neu:897/fulltext.pdf> — contentful expansion、展开轨迹、箭头提取正确性；"cannot predict" 断言出处。
13. Culpepper, Felleisen. *Fortifying Macros*. ICFP 2010. <https://doi.org/10.1145/1863543.1863577> — syntax-parse：受限模式语言 ⟹ 绑定形状静态可知。

### 卫生与绑定解析的现代形式化

14. Flatt. *Binding as Sets of Scopes*. POPL 2016. <https://doi.org/10.1145/2837614.2837620> — Σ: name→(scope-set→name)、biggest-subset 解析；纯数据结构可实现。
15. Herman, Wand. *A Theory of Hygienic Macros*. ESOP 2008. <https://www.khoury.northeastern.edu/home/wand/papers/herman-wand-07.pdf> — 卫生性的 α-等价不变量。
16. Adams. *Towards the Essence of Hygiene*. POPL 2015. <https://doi.org/10.1145/2676726.2677013> — 卫生改名的构造性刻画。
17. Bove, Arbilla. *A Confluent Calculus of Macro Expansion and Evaluation*. LFP 1992. <https://doi.org/10.1145/141471.141562> — 展开/求值交错的合流性。
18. Clinger, Wand. *Hygienic Macro Technology*. PACMPL 4(HOPL), 2020. <https://doi.org/10.1145/3386330> — 全家族算法综述（110 页）。

### 多阶段视角与反糖化

19. Ganz, Sabry, Taha. *Macros as Multi-Stage Computations*. ICFP 2001. <https://doi.org/10.1145/507635.507646>；Taha. *Multistage Programming*, 1999 — 宏 = 编译期阶段；绑定时间分析。
20. Pombrio, Krishnamurthi. *Resugaring: Lifting Evaluation Sequences through Syntactic Sugar*. PLDI 2014. <https://doi.org/10.1145/2594291.2594319> — Emulation/Abstraction 准则。
21. Pombrio, Krishnamurthi. *Hygienic Resugaring of Compositional Desugaring*. ICFP 2015. <https://doi.org/10.1145/2784731.2784755> — DAG 表示、共享子项。
22. Pombrio, Krishnamurthi, Wand. *Inferring Scope through Syntactic Sugar*. PACMPL 1(ICFP), 2017. <https://dl.acm.org/doi/pdf/10.1145/3110288> — 绑定静态推断 + 定理 5.4。~~★最直接相关~~ **评级下调（2026-10-07 全文核实）**：场景是语言规范糖（设计者写核心规则 + 去糖规则）与 DrRacket 内建形式箭头，非宏系统；算法对 do 式绑定列表自认失败（§6.4）。本项目落点 = 非宏特殊形规则的机械化生成（§2.6 勘定、§6-Q1）。
23. Pombrio, Krishnamurthi. *Inferring Type Rules for Syntactic Sugar*. PLDI 2018. <https://doi.org/10.1145/3192366.3192398>；Pombrio. PhD diss., Brown, 2018. — 类型规则回推。
24. Guan, Cao, Yu, Wang, Wang, Hu. *Semantics Lifting for Syntactic Sugar*. OOPSLA 2024 — 静态语义提升（resugaring 的对偶路线）。

### 直接先行工作

25. Fisher, Shivers. *Static Analysis for Syntax Objects*. ICFP 2006. <https://doi.org/10.1145/1159803.1159817>；扩版 *Building Language Towers with Ziggurat*, JFP 18(5-6), 2008 — 标题即"语法对象的静态分析"：分析宏定义本身、组合静态语义。
26. Culpepper, Owens, Flatt. *Syntactic Abstraction in Component Interfaces*. GPCE 2005. — 跨组件边界的绑定可见性。
27. Barrett, Christiansen, Gélineau. *Predictable Macros for Hindley-Milner* (Klister). TyDe 2020 extended abstract. — 宏展开与类型检查交错的现代实例（Klister 中宏可查询当前展开任务的类型目标）。

### 规范性文献

28. R6RS §10 (Libraries) / §4.3 (Expansion): "Information about identifier bindings is maintained **during expansion** to enforce lexical scoping." <https://standards.scheme.org/unofficial/errata-corrected-r6rs.pdf>

---

以上 28 条文献的本地副本已归档至 [`macro-papers/`](./macro-papers/README.md)（28/28 已下载，含逐篇中文归纳与版本备注；#24 为手动从 ACM 下载，自动下载被 Cloudflare 拦截）。
