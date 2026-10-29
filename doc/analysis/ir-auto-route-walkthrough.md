# IR 自动路由全机制代码走读（match 调用点逐环节）

> 调研日期 2026-10-28（合并走读版：整合原 `ir-auto-route-code-walkthrough-cn.md` 与本文档）。基于 `kimi` 分支当前代码。 相关文档：`doc/analysis/chez-sc-expand-records-switch.md`（`(sc-expand x env #t)` 开关的机制与发现历程）、 `doc/design/route2-retirement-proposal.md`（route 2 退役决策记录）、 `doc/analysis/macro-analysis-framework.md`（宏分析总框架）。 **route 2（expanders/ 自研展开管线）已于 2026-10-21 彻底退役**（commit 0ac9f75c / 6067b65b），本文描述的 IR 路径是唯一通路：失败 = 调用点静默得不到展开派生引用，无回退路径。 示例贯穿：`(define (f xs) (match xs [(a b) (g a b)]))`，以及 fixture `tests/resources/workspace-fixtures/sc-expand-differential/consumer.scm.txt` 中 bias 131 的调用点 `(match input [(a b) (+ a b)] [('sym) 0] [(? number? n) n])`。 机制自 2026-10-21 起**宏无关**：除 ufo-match 六宏族外，`match-index-node`（库 `(scheme-langserver virtual-file-system index-node)`）也走同一通路——头符号与其库从调用点推导，env 按"该库的副本"引导。 文中金丝雀/命中率数字均为**历次测量快照**（不同 commit、不同 fixture 下口径不同，仅作量级参考，不作当前基线）。

---

## 0. 全景调用链

```
step (abstract-interpreter.sls)
  → 对文档里每个"不是普通值/过程"的 head identifier 构造 rules
  → construct-rules 内建规则表（let/lambda/syntax-case/...）未命中时
  → route&add (analysis/identifier/self-defined-rules/router.sls)
  → 命中 ufo-match 族或 match-index-node → rules 里挂上 (ir-auto-route:try-expand)
  → step 走到 (match xs [(a b) ...]) 调用点时执行该 rule
  → ir-auto-route:try-expand
      （门控 + 特性自检 chez-ir-available? → private:try）
  ├─► private:env ────────────── env 引导（源优先；interaction 兜底）
  │     ├─ bootstrap-source     三级源定位 → load → fresh env（按源缓存）
  │     │     ① 该调用点 head 的 import 解析目标（精确）
  │     │     ② workspace 库树 walk-library
  │     │     ③ CHEZSCHEMELIBDIRS 扫描
  │     └─ bootstrap-interaction 双向探测 interaction env（最后兜底）
  ├─► private:forms ──────────── document-text → 带注解顶层 form（按文本缓存）
  ├─► chez-ir-find-define-containing ── span 下行定位包围 define
  ├─〔fresh〕private:expand-fresh ─ 注解伪造的 (let () form #f) 包装
  │     └─ 双类阻塞标识符参数增长重试（≤64 次）：
  │        "unbound identifier X" / "misplaced aux keyword (X ...)"
  └─〔interaction〕chez-ir-expand ─ (sc-expand form env #t) 直接展开
  │
  ├─► chez-ir-call-site-facts ── IR 单次遍历 → refs + bindings 两类事实
  │     （旧接口 call-site-refs / call-site-bindings 保留为其投影）
  └─► private:synthesize! ────── 合成 identifier-reference
        归组 (name . binding-bias) + per-clause 作用域（use 祖先校验+回退）
        + 绑定驱动 use-less 合成 + 防重护栏 + speculative 标记
        ※ 类型侧不在此路径 —— type router 泛化宏臂 → macro-expansion.sls
          （Chez 展开 → 重建注解树 → 虚拟文档 → generator 处理 → 回传）

之后：step 照常步进 → 叶子解析 → find-available-references-for
      → usage-count++ → definition / hover / references / 检查器

失败：任意环节 guard 捕获 → (ir-auto-route:stats) 记账 miss
      → 调用点静默无展开派生引用（无回退路径）
```

关键认知：`route&add` 本身**不做展开**，只做模式匹配式分发。对 `(match ...)`：

```scheme
[(and (equal? library-identifiers '((ufo-match)))
    (find (lambda (e) (equal? expressions (list e)))
      '(match match-next ...)))          ; head 解析到 (ufo-match) 库的 match
  (add-rule-procedure rules `((,(ir-auto-route:try-expand)) . ,target-identifier))]
```

`ir-auto-route:try-expand` 返回一个零参数调用的 rule 工厂 thunk `(lambda (root-file-node root-library-node document index-node) ...)`， 符合 rule 调用约定；这个 rule 对**每一个宏调用点**执行一次。 路由器里两条臂：ufo-match 六宏族（库 `(ufo-match)`：match / match-next / match-one / match-check-ellipsis / match-two / match-check-identifier）， 以及 match-index-node（库 `(scheme-langserver virtual-file-system index-node)`， 2026-10-21 泛化加入）。历史对照：route 2 时代需要 langserver 侧 expander（`private:find-expander` 已删）且路由臂会包裹 route-2 规则； IR 路径无任何 opt-out 门控，机制内部完全宏无关。

## 1. ir-auto-route.sls —— 编排层（688 行）

`private:try` 的流程，以 `(define (f xs) (match xs [(a b) (g a b)]))` 为例：

### ① 确定 head 的库（macro-agnostic 的信息源）

从 call-node 第一个子节点取 head 符号：

```scheme
(let* ([head-symbol (index-node-expression (car (index-node-children call-node)))]
    [head-refs (find-available-references-for document call-node head-symbol)]
    [library-identifier ...])
```

用 `find-available-references-for` 查到 head 解析到哪个库（这里是 `(ufo-match)`）。 这个机制不写死 match——`match-index-node` 走的完全是同一个入口。

### ② forms 缓存

`private:forms` 用文档 **text**（不是磁盘）重新 `get-datum/annotations` 解析出带注解的顶层 form 列表，按 text 哈希缓存（上限 64 份，didChange 时 text 变即自然失效）。为什么按 text：编辑中的脏缓冲区也要能走这条路， 否则整个编辑会话全回退。（sfd 直接建在字符串上——磁盘一致性校验的 v1 设计已废弃。）

### ③ 定位待展开的 form

`chez-ir-find-define-containing` 找包含调用点 bias 的**最小 `(define ...)` form**。
- 为什么不是整个 library form：对 library 顶层 `sc-expand` 会把定义推迟到 编译期部分，返回的运行时 IR 是空的（实测只得到一个 `Lsrc:quote`）；
- span 通过子节点合并计算并 memoize；逐 child 检查 span 包含性， dotted template 的不规范尾部也当节点递归（修复过的 "not a pair" 崩溃）。

### ④ env 引导（`private:env`，SOURCE-FIRST 顺序）

返回 `(env . mode)`，`mode ∈ (fresh | interaction)`；缓存按源路径分键的哈希表。

1. `private:import-source`：通过调用点自己的 import 解析找到 head 所在宏库的 **具体源文件**（优先；一个 workspace 可能有同一库的多个副本——本仓库的 测试 fixture 就装着旧版 Chicken 风格变体——消费方的 import 解析知道哪个 是对的）；
2. 退回 `walk-library` 在 library-node 树里找；
3. 再退回 `CHEZSCHEMELIBDIRS` 扫 `ufo-match.sls` 等；
4. 都找不到才退回 interaction env（容错但非确定性）。

找到源后：`(load source)` 注册到 Chez 全局 library manager（script 和 编译二进制行为一致，compile-chez-program 实测），然后 `private:build-env` 构造**精确编译期语义的 fresh env**： `(chezscheme) + head 库 + 文档自身的 import sets`（workspace root 临时 prepend 到 `library-directories`），构造失败退回最小二元 env `(chezscheme) + head 库`。

包含文档 imports 的意义：**辅助关键字模式获得精确语义**——用户手写的 `?`/`:_`/`**1` 与宏定义处的 aux 同绑定，不再被重试机制不透明化 （历次快照：诊断数 1913→1907，GT 中 `(? …)` 自锚定伪条目消失）。 env 缓存键 = `源 × 库 × import 子句`——一个 server 进程分析多个工作区、 各有不同宏副本/不同 import 面时互不干扰。

**为什么源优先于 interaction**（2026-10-21 实测定论）：fresh env 只含 `(chezscheme) + <head 库>`，含义完全确定；而 interaction env 的内容随 **host 进程的 import 面**变化——曾发生某 host 的传递依赖意外遮蔽了 Chez 基环境的 `except` 辅助关键字，使同一份用户代码"有时能展开有时不能" （注释里有 ufo-try 的 `except` 事故记录）。展开语义绝不能依赖这种偶然性。

### ⑤ 双向 probe 验证

interaction/fresh env 都很宽松，naive probe 会假阳性（unbound `match` 展开成 `($top-level-value 'match)` 不报错）。所以 `private:probe-in` 同时：

- **negative**：IR 里不得出现 `($top-level-value '<head>)`（真展开会消费关键字）；
- **positive**（仅 match 本尊）：failure thunk 带 `(quote match)`， 用 `chez-ir-contains-quoted-symbol?` 确认。

两个判定谓词的实现形状见 §2（`chez-ir-global-lookup?` / `chez-ir-contains-quoted-symbol?`）。探针用 `(match 1 [x x])` 而非 `[_ 1]`：ufo-match 对 `_` 通配符在**所有模式**下报 "misplaced aux keyword" （与 env 无关的既有 quirk），用 `_` 探针曾把编译二进制误判为不可用。

### ⑥ fresh 模式的展开包装

fresh env 不可变、对自由标识符严格，直接 sc-expand 用户 define 会炸 （兄弟 define 互相不可见）。`private:expand-fresh` 把 form 包成注解版的 `(let () form #f)`（内部 define 被接受，尾部 `#f` 满足"body 至少一个 表达式"；wrapper 伪造为 annotation 使内层位置不丢），再按需包 `(lambda (free...) ...)`。被包进 wrapper 的标识符对展开**不透明**—— 我们只关心宏调用的事实，外围 define 里的其他形式不需要被真正展开。

自由变量列表**从错误消息里生长**：捕获 `unbound identifier X` / `misplaced aux keyword (X ...)`（Chez 自己把 `except` 声明为 aux 关键字） 两种错误，把 X 加进参数列表重新展开，上限 64 次。第二类是 2026-10-21 的 实测发现：`except` 在 chezscheme 基环境中就是辅助关键字，**任何**含 chezscheme 的环境都拒绝对 `(except c …)` 形式的展开——route 2 时代某些 文件能过纯属特定 host import 面对 `except` 的偶然遮蔽。token 必须取 **短语之后**的第一个——消息尾部跟位置信息，取最后一个会拿到文件名。 发现过的参数按 `(form 身份 × env 身份)` memo（实测快照 2026-10-28： 225 次 fresh 展开省掉 196 次重试，~60% 的 fresh sc-expand 时间曾花在 重复发现同一批阻塞标识符上）。

### ⑦ 展开 + 取事实 + 合成

```scheme
[ir      (sc-expand form env #t)]                ; records switch，见 §2
[refs    (let-values ([(r b) (chez-ir-call-site-facts ir path)])
           (private:merge-use-less-bindings r b))]
                                                 ; 单次遍历取 refs+bindings 两类事实
                                                 ; （旧接口 call-site-refs / -bindings 为其投影），
                                                 ; 绑定项补成 (name #f binding-bias) 后与 refs 合并
[inserted (private:synthesize! refs document call-node)]
```

### ⑧ private:synthesize!

把 IR 事实物化成 langserver 的 identifier-reference：

- 按 `(name . binding-bias)` 分组，每组**一个** reference （route 2 时代共享 call-node scope 会产生重复候选，归组后消除）；
- 只接受 binding-bias 落在本调用点 span 内的条目（嵌套调用点各归各； 宏派生判定：绑定在调用点跨度内才算宏产物——`input` 的绑定（lambda 形参）在跨度外 → 跳过，普通规则已注册，天然排除）；
- **anchor** = binding-bias 处的 index-node（跳转定义的目标）；**scope** = call-node 的能同时包住 binding 与全部 uses 的直接子节点——即 per-clause 作用域：`(match xs [(a b) ...] [(a b c) ...])` 里第二个 clause 的 `c` 不会泄漏进第一个 clause（兄弟子句互不可见，scrutinee 天然排除，嵌套 match 外层变量在内层仍可见）；
- 绑定驱动补充：merge 进来的 `(name #f binding-bias)` entry（从未被使用的 pattern var）走同管线合成——`(match input [(a b) a])` 的 body 也能补出 b；
- anchor 若已导出同名标识符（clause body 里真有 lambda 参数注册了）则 跳过，避免重复；
- reference 标 `speculative? #t`，写入 anchor 的 `references-export-to-other-node` 和 scope 的 `references-import-in-this-node`（排序插入）。

### ⑨ 命中之后：引用"上班"

规则返回 `#t`，step 照常步进子节点。到 body 叶子（fixture 例中 bias 160）：

```scheme
;; abstract-interpreter.sls 叶子分支
(find-available-references-for document body-a-node 'a)
;; 沿父链收集 imports → 命中调用节点上的合成 a-绑定（锚 152）→ usage-count++
```

从此 definition（返回 152）、hover、references、unused-imports / unused-local-variables 全部正常工作，usage-count 干净。

### ⑩ 失败语义与可观测性

`private:try` 整体包在 `guard` 中：任何失败 → `private:bump! (cons #f ms)` → 返回 `#f` → **调用点静默得不到展开派生引用**。回退构成（历次快照 37 例实证）几乎全是 `form-not-found`——**宏模板内的伪调用点**（Chez 展开 宏定义时模板是数据不是代码，IR 路径正确地拒绝分析；route 2 时代曾白分析 它们，即历史 profile 中 +231ms 急切成本的组成）。

可观测性三件套：

| 入口 | 内容 |
|---|---|
| `(ir-auto-route:stats)` | `(命中 回退 总毫秒)`，互斥锁保护（threaded 安全） |
| `(ir-auto-route:env-source)` | `'interaction \| (loaded . path) \| 'unavailable` |
| `REPLAY_TRACE=1` | 每次 miss 的完整条件文本（含文档 URI 与 bias）+ bootstrap 源信息 |

## 2. chez-ir.sls —— IR 工具层（899 行）

全项目**唯一** `#!chezscheme` 文件（`#!chezscheme` 必须是文件首行、紧邻 `(library ...)`、前无注释行），把所有未公开 API（`sc-expand` 第三参、 `#%$record-type-descriptor`、nanopass 的 `Lsrc:*` record）隔离在这一处—— Chez 升级只会坏这一个文件。注意顶部警告：公开 `record-type-descriptor` 碰到 kernel record 会抛出**不可捕获**的硬错误，所以处处用 `#%$record-type-descriptor` 且绝不裸碰 annotation record。

| 件 | 干什么 |
|---|---|
| `chez-ir-available?` | 特性自检（3 参 sc-expand 可调用 + kernel record 可读），失败则整条 IR 路优雅降级 |
| `chez-ir-read-annotated-file` / 内存版 `forms-from-text` | `get-datum/annotations` 的 bfp 协议循环，逐 form 读出带注解 datum |
| `chez-ir-find-define-containing` | §1③ 用的 span 定位（span memoized 下行） |
| `chez-ir-expand` | 就是 `(sc-expand datum env #t)`——**records?=#t 开关**：跳过 `$uncprep` 反解析，返回保留全部源码位置的 Lsrc IR（开关本身见 `chez-sc-expand-records-switch.md`） |
| `chez-ir-call-site-facts` | **单次遍历** IR，同时产出两类事实：① 源码文件 == 本文档的 `Lsrc:ref`（`#(maybe-src x)` 字段），输出 `(name ref-bias binding-bias)`（binding-bias 来自 prelex 的 source，宏引入的绑定为 `#f`）；② 所有 prelex **绑定**（letrec 的 x*、case-lambda clause 形参），源码在本文档的按 `(name binding-bias)` 去重收集——补上"从未被使用的 pattern var"这种 use 驱动收集器永远看不到的绑定。2026-10-28 合并，旧接口 `chez-ir-call-site-refs` / `chez-ir-call-site-bindings` 保留为其投影 |
| `chez-ir-global-lookup?` / `chez-ir-contains-quoted-symbol?` | probe 的双侧判定（识别 `Lsrc:call` 套 `primref $top-level-value` 套 `Lsrc:quote` 的特定形状 / 任意 quote record）；二者共用 `private:record-walk`（pair + record 双走，seen 去环，annotation 下潜） |
| `chez-ir->annotated-sexp` | `$uncprep` 的逆操作（**类型侧**用）：把 IR record 重新组装成带注解的 sexp 树——每个节点包 annotation（位置来自 preinfo.src / ref maybe-src / prelex.source；未知位置共享 dummy sfd，因为 index-node 构建器要求每个节点都带 source 且会丢非注解子节点）；primref 还原成名字、prelex 还原成用户标识符；**case-lambda 应用按 arity 选 clause β-归约成 let**（类型生成器只认 let 形式的值流，不认 case-lambda 应用；包括经 let/letrec 绑定名的间接应用——Chez 会把 clause lambda 提升穿越这些 let，所以维护了一个 prelex→case-lambda 的 env 表）；`chez-ir-rebuild-budget` 边建边数节点，超限立即 raise |
| `chez-ir-rebuild-budget` | parameter 形式的节点预算，重建时**边建边数**，超限立即 raise——防那个展开成 350 万节点的 match-index-node 调用点把堆撑爆（事后数节点已经涨到 GB 级） |
| `chez-ir-call-site-type-flows` | 差分测试 oracle：从 letrec 绑定和 case-lambda 应用中提取 `((binding-bias . flow))`，flow 是 `(ref . bias)` / `(chain op . flow)` 的组合，cxr 访问器链被分解（cadr → car∘cdr）。已不在生产路径 |

工具层的性能备忘（2026-10-28）：`private:accessors-of` 与 `private:rebuild-tag` 两处 memo（record 访问器表 / IR 重建标签）用 mutex 保护的 eq-hashtable（各自一把 mutex）；`private:rtd-name-prefix?` （kernel record 名字前缀判定）为无 memo 单份调用，每次做 `#%$record-type-name` 字符串比较。

## 3. 类型侧的同源入口（expansion-driven，2026-10-27 第三轮最终架构）

类型侧不走 identifier router 的 ufo-match 臂，而是 `analysis/type/substitutions/self-defined-rules/router.sls` 的 **generic macro arm**：head 的 root-ancestor 类型为 `syntax-variable` 的调用点一律挂上 `(macro-infer root-library-node document index-node)`，即 `type:expand-and-infer`（`analysis/type/substitutions/macro-expansion.sls`）， 内部调 `ir-auto-route:expand-call-site`：同一份 forms/env/展开，但 §1⑦ 换成 `chez-ir->annotated-sexp` 重建树，让真实生成器在虚拟文档上跑， 结果按源位置映射回调用点。

`expand-call-site` 的负缓存顺序有讲究：**先查负缓存 → head refs → find-define（最便宜的确定性拒绝）→ env 引导 → 展开**，因为绝大多数 调用点根本扩不了（vendored 库里 define-syntax 内部的几千个宏调用）， 把便宜的拒绝放前面。负缓存按文档 + bias 键，以 text 一致性失效、 成功驱逐（success-eviction）。

逐步走读：

1. **展开**：`ir-auto-route:call-site-flows` 同款 env 引导 + `sc-expand`；
2. **重建**：`chez-ir->annotated-sexp` 把 IR 记录重建为**全量注解 s 表达式树**（默认 $uncprep 输出剥光注解，实测——位置必须从 preinfo.src/ref maybe-src/prelex.source 恢复；未知形状降级 (void)）； **单子句 case-lambda 应用（含经 let/letrec 绑定的名字应用——Chez 会提升 子句 lambda）按 arity 选子句 β 归约为 let**，因为该类型系统对形参只在 返回点解释、永不落到形参节点，let 规则的"绑定↔初值"是唯一的值流通道；
3. **虚拟文档**：`private:build-index-tree` 用 `make-index-node` 自建树 （**不用 init-index-node**——那是为读取器产物设计的，过滤/合成/去重 三环节对重建数据静默丢子树，实测）。每个节点带真实或 dummy 源 （构建器要求每个节点有源对象，实测）；**引用上下文直接继承 callee 文档的 ordered-reference-list**（展开式里每个名字按构造在调用方编译 环境可解析，调用方 identifier pass 已把带真实元数据——含 type-expressions——的引用填进该表，find-available 沿父链走到天花板 自然落入），取代早期手造种子层（每符号一枚引用、9 参构造器错位、 自引用守卫等八个阻抗类 bug 全是那一层引入的，实测教训档案见代码注释）；
4. **局部绑定注册**：`private:register-bindings!` 在跑 generator 前为虚拟树 的 let/let*/letrec/letrec* 绑定符与 case-lambda 形参各建一枚引用 （type='variable'/'parameter'，index-node 与 initialization-index-node 都指 binder 节点本身）挂到绑定形式的 import 列表——与标识符侧 lambda/let 规则在真实文档里做的事一致。没有这一步，use 位置（如 `(car w)` 里的 w）沿父链找不到 binder，值流链死在第一环（实测）； 注册后 use 经 trivial-process 的 local 分支挂上 binder 指针，let 规则的 绑定↔初值双向链接随之贯通；
5. **generator 处理**：注入式调用 construct-substitutions-for（解环 generator→router→本模块）；**子节点逐个隔离 try**（一个子节点 raise condition 曾把后续兄弟的整棵子树分析静默清零，只留一条诊断—— 生产级 bug，已修）；
6. **回传**：虚拟树里源位置属于调用点跨度的节点，其替换项按 bias 拷回 真实文档节点；虚拟文档按真实文档键保留（替换项里的虚拟节点指针 惰性解释需要）。

**实测**：`(match (string->list "ab") [(a b) (+ a b)] ...)` 的 a/b/n 各 获得值流替换项（虚拟树节点指针）；back-prop 20+ 条目/调用点； **`type:recursive-interpret-result-list` 下 a 完整解析到 char?**—— 值流链 a ← (car w) ← w ← (cdr v) ← v ← (string->list "ab") ← (inner:list? char? ...) 全环贯通（2026-10-27 实测，这是"最后一环"的 闭环证据）。

**历史弯路（三轮架构的教训档案）**：① IR flow 语言 （chez-ir-call-site-type-flows + 翻译层）——能力真子集复刻 generator， 已拆，提取器转任差分 oracle；② 手写宏族规则（源语法 pattern 走查， 含 type 侧 match 的 `(? pred id)` 近似）——能工作但违背宏无关初衷， 已被本方案取代（删除）；③ 本轮的 vfs 构建器阻抗类（全量注解/头符号 包装/dummy 源/9 参构造器/自引用守卫/去重种子/兄弟清零）全部写进代码 注释。

**已知边界（诚实清单）**：a 已到 char? 级；b 经变参尾链（cadr 由 car/cdr 模板复合）只断言到替换项级——复合模板的跨节点套用是 DSL 自己的 事，不再属于本机制；`(? pred)` 的谓词精化不在本机制范围（守卫语义属 源语法可见信息，按接缝判据归规则层）。

测试：`tests/analysis/type/substitutions/test-ir-type-flow.sps`（机制级断言 + **递归解释到 char? 的闭环断言**）+ `test-macro-rules-vs-ir.sps` （oracle 差分）。实现细节另见 `doc/analysis/type/type.md` 的 macro-expansion 章节；本文只保留走读视角。

## 4. 一眼例子

```scheme
(define (f xs) (match xs [(a b) (g a b)]))
```

1. `route&add` 认出 head `match` ∈ `(ufo-match)` → 挂上 try-expand rule；
2. step 到该调用点 → `private:try`；
3. `import-source` 找到 workspace 里 ufo-match 的 `.sls` → `load` + `build-env` `(chezscheme) (ufo-match) <本文档 imports>` → probe 通过；
4. `find-define-containing` 定位到 `(define (f xs) ...)`；
5. `expand-fresh`：第一次报 unbound `g` → 包 `(lambda (g) (let () form #f))` → 成功；
6. IR 里 `a`、`b` 的 prelex 绑定 source 指向 pattern 位置（在本文档 path 内），uses `(g a b)` 的 ref source 也在——绑定落在**模式位置** 是 Chez 原生归属，goto-definition 跳得准的根源（差分测试以它为地面 真值，15/15）；
7. `synthesize!`：anchor = pattern 位置 index-node，scope = clause 子节点 → `a`、`b` 成为 speculative 引用，跳转定义、补全、unused 检查从此可见。

## 5. 实测陷阱汇编（本机制的全部"坑"清单，二十一条）

按发现顺序，全部写入代码注释与 commit message：

1. `expand` 输出无位置——丢在出口 `$uncprep`，`(sc-expand x env #t)` 可跳过（开关本身见 `chez-sc-expand-records-switch.md`）。
2. 读取 IR 要 `#%$record-type-descriptor`；公开的 `record-type-descriptor` 对内核记录报**不可捕获**的 "unrecognized record"。
3. 文件必须 `#!chezscheme` 首行开 `#%$` 原语，且指令必须紧邻 `(library …)`、前无注释行。
4. annotation 裹住 IR 子树与位置：遍历要下潜；pair 记录混存，下降必须双走。
5. `prelex.source` 是注解标识符（再剥一层），`ref` 的 maybe-src 是裸 source object。
6. `get-datum/annotations` 给每个节点（含原子）包注解，形状比较要深剥。
7. **必须展开包围 define，不能是 library 表单**（library 运行时 IR 为空）。
8. 模板点对语法 `(pat . body)` 的非正规尾会让 span 下行崩溃。
9. **interaction env 宽松**：未绑定宏静默变 `($top-level-value …)`，朴素探测全假阳性——探针必须双向验证。
10. **`_` 通配符**被 ufo-match 全模式拒绝——永不用于探针。
11. fresh `(environment …)` 不可变：拒顶层 define（let 包装解决）、对自由标识符严格（参数增长解决）。
12. let 包装要尾 `#f`（"no expressions in body"）；包装用 `make-annotation` 伪造保位置。
13. **Chez 基环境自带辅助关键字**（`except` 实测）——用户函数名碰撞时用 "misplaced aux keyword" 重试中和；消息 token 取短语后第一个（尾部是位置信息）。
14. 错误重试上限要 ≥64（真实 define 引入几十个 sibling）。
15. 源选择要精确到**调用点的 import 解析目标**——workspace 里可能有多个同名宏副本（fixture 旧版变体）。
16. env 缓存按源分键且用 `equal-hash`（`string-hash` 表塞 `'interaction` 符号键每次抛异常，被 step 的 try/except 吞成诊断噪音）。
17. miss 追踪禁用 `condition-irritants`（对无 &irritants 条件抛异常）。
18. host 进程**无需** import `(ufo-match)`（`run.ss` 已移除）——load 路径自给自足。
19. **use 驱动提取的盲区**（2026-10-26）：从未被使用的模式变量（`(match input [(a b) a])` 的 b）不产生 Lsrc:ref，use 驱动收集永远看不见它——补全候选缺失。实测未使用 prelex 在 IR 里存活且 source 指向 pattern 位置，故用绑定驱动收集做补充（merge 为 ref-bias=#f 的 entry）。
20. **跨子句泄漏与双份候选**（2026-10-26）：统一插调用节点作用域导致兄弟子句互见、同名变量双份（子句1 body 补全出 `(a a)`）。修复 = 归组 + 插入点下沉到 binding-bias 所属直接子句 + use 祖先校验回退。教训：候选列表出现同名双份 = 同层 import 列表混入了两个绑定。
21. **测试 bias 计算的陷阱**：模式 b 的 " b) " 与 body `(+ a b)]` 的 " b)" 只差一个尾部空格，nth 搜索会抓错（调试时 b 的锚点 199 被指为 155，实为测试辅助函数抓到了 body b）。fixture 搜索串要带足够上下文。

## 6. 测试锚定矩阵

| 测试 | 锚定的环节 |
|---|---|
| `tests/analysis/identifier/test-sc-expand-differential.sps` | §1⑦+⑧：Chez IR 地面真值 vs 分析结果（15/15） |
| `tests/analysis/identifier/test-ir-auto-route.sps` | §1 全链（真实 workspace init） |
| `tests/analysis/identifier/test-ir-auto-route-load.sps` | §1④+⑥：load 源定位、fresh env、let 包装、**双类重试**、单锚点（10 断言） |
| `tests/analysis/identifier/test-sc-expand-route3-spike.sps` | §1⑦+⑧ 纯机制 + 性能门（<50 ms，实测 ~11 ms） |
| `tests/analysis/identifier/test-ir-auto-route-clause-scope.sps` | per-clause 作用域 + use-less 绑定（跨子句泄漏/双份候选/未使用绑定补全，8 断言） |
| `tests/robustness/robustness-ir-route-threaded.sps` | 全链并发安全（6 次 threaded init，25/25） |
| `tests/analysis/identifier/test-ir-auto-route-match-index-node.sps` | 宏无关泛化：match-index-node 走完整 IR 链路（真实仓库 init，GT 11 项全中） |
| `bin/ir-route-canary.ss` | 全仓端到端（命中/回退/引用数/诊断数；自分析本仓库） |
| `tests/protocol/apis/test-{definition,references,document-symbol}.sps` | §1⑨：IDE API 冒烟 |

（route 2 时代的 14 个测试随管线一并删除；`rules/syntax-rules.sls` / `rules/syntax-case.sls` 及其测试**保留**——clause-process 的子句符号注册 属于普通 define 分析，不是宏展开。）

## 7. 已识别的优化候选（未实施项）

1. ~~`chez-ir-call-site-refs` / `call-site-bindings` / `type-flows` 各自 独立 walk 一遍 IR，三次遍历可合并~~（已做 2026-10-28：refs+bindings 合并为单次遍历的 `chez-ir-call-site-facts`，旧接口保留为投影； type-flows 为测试 oracle 不动）；
2. ~~`private:find-source` 里手工实现的字符串函数~~（已清理 2026-10-28： 死代码 `private:string-contains?` / `private:token-after` 删除， `private:find-substring` 换 srfi :13 `string-contains`， `private:split-on` 内部改用 srfi `string-index`）；
3. `private:synthesize!` 里 `pick-index-node-from` 对 `document-index-node-list` 线性扫描，调用点密集时累计成本可观；
4. `private:try` 等处的 `(car (index-node-children call-node))` 裸结构 访问是否可用 `match-index-node` 模式匹配优化——待调研。

## 8. 一句话总结

**router 认出六宏族并转交 `try-expand`；ir-auto-route 以"源优先"引导出确定性的 Chez 环境（工作区自己的宏副本，load 注册 + fresh env）、用注解伪造的 let 包装 + 双类阻塞标识符参数化让 Chez 展开（位置零损耗）、把 IR 里的 (use, definition) 事实经单次遍历归组、以 per-clause 作用域（use 祖先校验 + 回退）+ 绑定驱动 use-less 补充合成 identifier-reference 贴回文档；chez-ir 承担与 Chez 未文档化 IR 对话的全部脏活（含探针真伪鉴定）；类型侧经同一 env 引导展开后用 chez-ir->annotated-sexp 重建虚拟文档、由真实 generator 跑值流；失败即静默不分析——route 2 的自研展开管线已成为历史。**
