# Chez `sc-expand` 的 `records?` 开关：拿到带完整源位置的展开 IR

> 范围：Chez Scheme expander 的未文档化开关 `(sc-expand x env #t)`——跳过出口反解析，直接返回编译器内部 nanopass IR（parsed Lsrc），其中表达式/引用/绑定三层都携带源位置。含完整使用协议、可运行代码、实测结果与稳定性评估。
> 环境：Chez Scheme 10.4.1，源码核实于 cisco/ChezScheme@main（2026-10-20）；宏为 `.akku/lib/ufo-match.chezscheme.sls`。
> 日期：2026-10-20。
> 姊妹篇：`macro-analysis-framework.md`（路线二理论）、`macro-resolution-profile.md`（路线二实测开销）。本文是"宿主 Chez 有没有自带带位置的宏展开"这一问题的答案——**有，且一直在内核里，只是默认不输出**。

---

## 1. 结论

1. Chez 展开器**从始至终保留完整的位置信息**：读取注解 → 展开（`chi-*` 把 annotated expression 传给输出构造器）→ 位置嵌入 IR（`preinfo.src` / `ref` 的 `maybe-src` / `prelex.source`）。
2. 位置唯一的丢失点是 **sc-expand 出口处的 `$uncprep` 反解析**（IR → 普通 s 表达式），而这一步有一个布尔开关可以跳过。
3. `(sc-expand x env #t)` 返回带位置三分归属（调用点 / 宏定义文件 / 无位置）的 IR，与 Racket `expand` 的 syntax object 信息等价，且绑定级位置（`prelex.source`）比 Racket 更细。
4. 全部可用**纯用户态代码**实现（不修改 Chez、不 fork），但需要两个未文档化入口：`sc-expand` 第三参数与 `#%$record-type-descriptor`。

## 2. 开关本身

`sc-expand` 实际有 5 个参数（CSUG 只记载前两个）：

```scheme
(sc-expand x env records? compiling-a-file outfn)
;;  x                要展开的表达式（可带 annotation）
;;  env              展开环境（默认 interaction-environment）
;;  records?         #f（默认）→ 返回普通 s 表达式，位置丢失
;;                   #t         → 返回编译器内部 IR，位置全保留
;;  compiling-a-file compile-file 路径内部使用
;;  outfn            输出文件名（影响错误消息），内部使用
```

定义（`s/syntax.ss:6783-6801`）：

```scheme
(set! sc-expand
  (rec sc-expand
    (case-lambda
      ((x) (sc-expand x ... #f #f))
      ((x env) (sc-expand x env #f #f #f))
      ((x env records?) (sc-expand x env records? #f #f))
      ((x env records? compiling-a-file) (sc-expand x env records? compiling-a-file #f))
      ((x env records? compiling-a-file outfn)
       ...
       (let ([x (at-top
                  (parameterize ([meta-level 0])
                    (chi-top* x (env-wrap env) ctem rtem (env-top-ribcage env) outfn)))])
         (if records? x ($uncprep x)))))))
```

即：`chi-top*` 产出带完整源信息的 IR，`records?` 只决定要不要做 `$uncprep`。

## 3. 默认路径丢失什么（`$uncprep`，`s/cprep.ss:53`）

| IR 中的内容 | 拍平后 | 丢失 |
|---|---|---|
| `preinfo` 记录（挂每个 `case-lambda`/`call`） | 整个丢弃（仅作 sexpr 缓存） | **每个表达式的源位置** |
| `(ref maybe-src x)` | 只留符号 `x` | **每个引用的源位置** |
| `prelex` 记录 | `(prelex-name x)`（gensym 符号） | **绑定位置**与变量身份的精确性 |
| `primref` 记录 | `($primitive n name)` | level 变成数字——`#2%`/`#3%` 前缀的来历 |

因此 `expand` 输出 = 同一 IR 的低配视图。位置不是展开过程丢的，是**出口反解析丢的**。

## 4. 开关打开后的产物：parsed Lsrc

`(sc-expand ann env #t)` 返回 Lsrc 语言的 nanopass 记录树。文法（`s/base-lang.ss:219-265`）很小，Expr 共 20 种形态；日常宏展开分析只会遇到：

```
(case-lambda preinfo cl ...)     一切函数的泛化形态（lambda 也编译成它）
(call preinfo e0 e1 ...)         函数调用
(ref maybe-src x)                变量引用     ← 位置在表单第 1 字段
(set! maybe-src x e)             赋值         ← 同上
(quote d)                        字面量
(if e0 e1 e2) / (seq e0 e1)
(letrec ([x* e*] ...) body)
(pr pr)                          primref（字段：level name flags arity）
```

**位置的三层存储**（比 Racket syntax object 更细）：

| 层面 | 位置 | 记录/字段 |
|---|---|---|
| 表达式 | `preinfo.src` | `preinfo`（`preinfo-lambda`/`preinfo-call` 的父类型；字段 `(src sexpr)`） |
| 引用 | `ref`/`set!` 表单第一字段 | `maybe-src`（source object 或 #f） |
| 绑定 | `prelex.source` | `prelex`（字段 `(name flags source operand $uname)`，`s/base-lang.ss:84`） |

三分归属的来源：展开时每个输出构造器（`build-lambda`/`build-call`/`build-ref`…，`s/syntax.ss:499-536`）收到一个 `ae`（annotated expression），`ae->src`（`s/syntax.ss:461`）从中取 source object：

- **调用点拷贝**的片段：ae 来自调用点文件 → src = 调用点位置；
- **宏模板引入**的片段：ae 来自宏定义文件（若宏编译时保留了注解）→ src = 宏库源码位置；
- 宏运行时现场构造（`datum->syntax` 等）：ae 无注解 → src = #f。

`s/syntax.ss:155-161` 的头部注释明确写了这个设计意图：

> If annotations are supplied, the proper annotated expression is passed to the various output constructors, allowing implementations to accurately correlate source and expanded code.

## 5. 实测结果（ufo-match，2026-10-20）

测试源（`/tmp/match-src2.ss`）：

```scheme
(match (f x)
  [(a b) (+ a b)]
  [('sym) 0]
  [_ 'other])
```

IR 遍历结果（节选，完整复现见 §7）：

```
表达式[preinfo-call] src=match-src2.ss:1:1          ← match 调用点本体
  绑定 v  src=无                                     ← 宏生成临时量
  引用 v  src=lib/ufo-match.chezscheme.sls:142:20    ← 模板引入 → 宏定义位置
    绑定 a  src=无
      表达式[preinfo-call] src=match-src2.ss:2:10   ← body (+ a b) 调用点位置
      引用 a  src=match-src2.ss:2:13                ← body 引用 a → 调用点位置
      引用 b  src=match-src2.ss:2:15
    引用 x  src=lib/ufo-match.chezscheme.sls:270:34  ← 模板里的解构临时量
```

三分归属与 Racket `expand` + `syntax-original?` 实验结果一一对应：

| 展开中的节点 | 归属 | 位置 |
|---|---|---|
| scrutinee、body、body 中对模式变量的引用 | 调用点 | `match-src2.ss:*` |
| 临时变量（v/w/failure/x）的引用 | 宏定义 | `ufo-match.chezscheme.sls:*` |
| 宏生成的 let 绑定处（prelex.source） | 纯生成 | #f |

合成 Racket 的 `syntax-original?` 等价判定：src 的 sfd 属于调用点文件 ⇒ 原始节点。

## 6. 使用协议

### 6.1 输入端：注解必须带 debug 标志

`ae->src` 检查（`s/syntax.ss:461-465`）：

```scheme
(and (annotation? ae)
     (fxlogtest (annotation-flags ae) (constant annotation-debug))
     (annotation-source ae))
```

文件读入的注解默认带此标志，标准读法即可：

```scheme
(define-values (annotated efp)
  (get-datum/annotations textual-port
    (make-source-file-descriptor path (open-file-input-port path))  ; 注意：sfd 要二进制端口
    bfp))                            ; 首调 0；后续调用传上一个 efp（多字节字符文件必须遵守）
```

### 6.2 读取端：`#%$record-type-descriptor` 是必需的

公开的 `record-type-descriptor` 对 IR 记录（以及 annotation 等内核记录）报 **"unrecognized record" 硬错，且该错误无法被 `guard`/`with-exception-handler` 捕获**（实测：异常绕过处理器直达默认处理程序）。必须改用内核原语：

```scheme
(#%$record-type-descriptor obj)    ; 对任何记录返回 rtd
(record-type-name rtd)             ; 如 Lsrc:ref:Expr.17、preinfo-lambda、prelex
(record-type-parent rtd)           ; 沿父链查找（preinfo-lambda → preinfo）
(record-type-field-names rtd)
(record-accessor rtd i)            ; 按字段序号访问
```

配套注意事项：

- **类型名带版本后缀**：nanopass 语言节点计数器产生 `Lsrc:ref:Expr.17` 中的 `.17`，跨版本会变。匹配用**前缀**（如 `"Lsrc:ref"`）或父链 + 字段名，不要硬编码全名。
- 字段序号依据（`s/base-lang.ss`）：`preinfo` = `(src sexpr)` → src 在 0；`prelex` = `(name flags source operand $uname)` → source 在 2；`ref` = `(maybe-src x)` → src 在 0、x 在 1。
- source object → 行列：`(locate-source (source-object-sfd src) (source-object-bfp src))` 返回 3 个值 `(path line col)`。`source-object-line/column` 访问器返回 #f（行列按需计算）。
- IR 中可能出现 annotation 记录（输入拷贝片段的残留包装），遍历时跳过（它们是"不可识别记录"，同样只能用 `#%$record-type-descriptor` 看，没必要处理）。

### 6.3 环境

`env` 用 `(interaction-environment)`（脚本内已 import 宏所在库）或显式 `(environment ...)`。宏必须是 Chez 原生 syntax-case 宏才能解析——ufo-match 家族正好是。

## 7. 完整可运行代码

在 scheme-langserver 仓库根目录（`source .akku/bin/activate` 后）：

```scheme
(import (chezscheme) (ufo-match))

;; ---- 1. 带注解读取 ----
(define PATH "/tmp/match-src2.ss")
(define p (open-input-file PATH))
(define sfd (make-source-file-descriptor PATH (open-file-input-port PATH)))
(define-values (annotated efp) (get-datum/annotations p sfd 0))
(close-port p)

;; ---- 2. 开关：#t = 拿带位置的 IR ----
(define ir (sc-expand annotated (interaction-environment) #t))

;; ---- 3. 遍历工具 ----
(define (rtd-of o) (#%$record-type-descriptor o))
(define (ancestor-rtd rtd name)
  (let loop ([rtd rtd])
    (cond [(not rtd) #f]
          [(eq? (record-type-name rtd) name) rtd]
          [else (loop (record-type-parent rtd))])))
(define (name-prefix? rtd prefix)
  (let* ([n (symbol->string (record-type-name rtd))]
         [len (string-length n)] [plen (string-length prefix)])
    (and (>= len plen) (string=? (substring n 0 plen) prefix))))
(define (src->string src)
  (if (and src (source-object? src))
      (let-values ([(path line col) (locate-source (source-object-sfd src) (source-object-bfp src))])
        (format "~a:~a:~a" path line col))
      "无"))

(define seen (make-eq-hashtable))
(define events '())
(define (emit! fmt . args) (set! events (cons (apply format fmt args) events)))

(define (field-refs o rtd)           ; 记录所有字段值（守卫个别字段不可读）
  (let* ([n (vector-length (record-type-field-names rtd))])
    (let loop ([i 0] [acc '()])
      (if (fx= i n)
          (reverse acc)
          (loop (fx+ i 1)
                (cons (guard (e [#t #f]) ((record-accessor rtd i) o)) acc))))))

(define (walk o depth)
  (when (record? o)
    (unless (eq-hashtable-ref seen o #f)
      (eq-hashtable-set! seen o #t)
      (let* ([rtd (rtd-of o)]
             [preinfo-rtd (ancestor-rtd rtd 'preinfo)]
             [prelex-rtd (ancestor-rtd rtd 'prelex)])
        (cond
          [preinfo-rtd
           (emit! "表达式[~a] src=~a" (record-type-name rtd)
                  (src->string ((record-accessor preinfo-rtd 0) o)))
           (for-each (lambda (v) (walk-any v (fx+ depth 1))) (field-refs o rtd))]
          [(name-prefix? rtd "Lsrc:ref")
           (let ([vals (field-refs o rtd)])
             (emit! "引用 ~a  src=~a"
                    (guard (e [#t '?])
                      (let ([x (cadr vals)])
                        (if (record? x)
                            ((record-accessor (rtd-of x) 0) x)   ; prelex.name
                            x)))
                    (src->string (car vals)))
             (for-each (lambda (v) (walk-any v (fx+ depth 1))) vals))]
          [prelex-rtd
           (emit! "绑定 ~a  src=~a"
                  ((record-accessor prelex-rtd 0) o)
                  (src->string ((record-accessor prelex-rtd 2) o)))]
          [else
           (for-each (lambda (v) (walk-any v (fx+ depth 1))) (field-refs o rtd))])))))
(define (walk-any o depth)
  (cond [(pair? o) (walk-any (car o) depth) (walk-any (cdr o) depth)]
        [(and (record? o) (not (annotation? o))) (walk o depth)]
        [else (void)]))

(walk-any ir 0)
(for-each (lambda (s) (display s) (newline)) (reverse events))
```

要点：遍历时**绝不能对 annotation 记录调用公开 `record-type-descriptor`**（硬错不可捕获），遇到 `(annotation? o)` 直接跳过；其余记录一律走 `#%$record-type-descriptor`。

## 8. 边界与稳定性

| 事项 | 状态 |
|---|---|
| 文档化程度 | 全部未文档化：3+ 参数、`#%$record-type-descriptor`、IR 文法。缓解：相关记录均 nongenerative（uid 写死），语义稳定；类型名后缀（`.17`）会变，用前缀/父链匹配 |
| 适用宏观类型 | Chez 原生 syntax-case 宏（ufo-match 家族是）；与 route 2 能力边界一致 |
| 顶层结构 | 表达式/脚本顶层最直接；整文件（compile-file 路径）多一层 `Lexpand` 包装（`group`/`visit-only`/`revisit-only`/`recompile-info`，见 `s/cprep.ss:40-53`），需先剥层 |
| R6RS 库形式 | `(library ...)` 顶层是一组 visit/revisit 分组，分析库体需处理 `eval-when` 结构 |
| 性能 | 一次 `sc-expand` = 编译器一次完整展开；比"展开 + pretty-print + reparse"便宜（省打印与重解析），信息无损 |
| 变量身份 | `prelex` 记录对象本身是精确身份（无 gensym 同名歧义）；拍平后的 gensym 名字仅用于展示 |

## 9. 对 scheme-langserver 的应用方向与落地记录

1. **route 2 对照验证通道**（✅ 2026-10-20 已落地）：`analysis/identifier/chez-ir.sls` + `tests/analysis/identifier/test-sc-expand-differential.sps`（fixture 在 `tests/resources/workspace-fixtures/sc-expand-differential/`）。库内封装了全部未文档化访问（含特性自检 `chez-ir-available?`），该文件以 `#!chezscheme` 首行指令开 `#%$` 原语（项目唯一，指令必须紧邻 `(library ...)` 且前无注释行）。测试把 IR 的 `(name ref-bias binding-bias)` 三元组与 route 2 的 `find-available-references-for` 解析结果按偏移比对。
   **首个实测发现与修复**（同日）：Chez 原生展开把模式变量绑定归属到**模式位置**（如 `a` → bias 152），route 2 当时回传到**宏调用节点**（bias 131）且产生重复 speculative 引用。定位链路：protocol 包装的构造器陷阱确认正确引用存在于 pattern 节点的 export 列表但从未进 ordered list；SLSCOPY_TRACE 证实 a/b 引用确实经 shallow-copy 的 import walk 回传（此前判断有误），失败根源是**绑定挂在内部作用域节点（生成的 let）上而该节点无 pair**——pvar 链接只存在于叶子 pair。修复（`expansion-wrap.sls` import 回退路径）：扫描**所有 hop 的 pair compounds**找同标识符的调用点叶子恢复真锚点；并把**作用域可见性**（插入宏调用节点，所有 use 的祖先）与**定义位置**（引用的 index-node = 恢复的模式位置）分离。修后差分全绿（含 scrutinee `input` → lambda 形参的透明路径）。已知残余：重复 speculative 引用（解析计数 2 vs Chez 的 1，TODO-15 P1/P2 多重性问题）。
2. **受控并行期**（✅ 2026-10-20 落地，`analysis/identifier/ir-auto-route.sls` + `tests/analysis/identifier/test-ir-auto-route.sps`）：router 的 ufo-match 臂挂"IR 优先、静默回退 route 2"的组合规则。**门控默认开启**（2026-10-20 翻转：`SLSCHEME_IR_ROUTE=0` 逐进程 opt-out；翻转依据 = 金丝雀 dominance + 14 条误报消除 + 多线程浸泡 6/6 干净 + IR 默认下 match 家族全回归一致）（fixture 实测 hits=1、IR 耗时 0.78 ms、15/15 满足地面真值；差分测试在两种模式下都作金丝雀 15/15）。实现要点（v2，2026-10-20 当日修订）：env 两级引导——① interaction-environment 快路径（host 已 import ufo-match 时；可变、宽容，直接展开 define）；② **load 回退**：经 library-node 树（walk-library）或 CHEZSCHEMELIBDIRS 定位**工作区自己的** ufo-match 源 → `(load …)` 注册进 Chez 全局库管理器 → 用 fresh `(environment '(chezscheme) '(ufo-match))` 展开——编译二进制同样可用（实测），且消除版本偏移（展开用的是用户工作区的宏副本）。fresh env 不可变（拒顶层 define）且对自由变量严格，故展开走 **let 包装**（内层 define 被接受，注解伪造的包装保证内部位置零损耗，尾缀 #f 满足 body 约束）+ **自由变量参数增长重试**（捕获 "unbound identifier" 错误逐次加 lambda 形参，上限 8 次）。两个实测陷阱：interaction env **宽松**——未绑定的 match 会静默展开成 `($top-level-value 'match)` 而**不报错**，朴素探测全部假阳性（`chez-ir-global-lookup?` + `chez-ir-contains-quoted-symbol?` 双向验证真展开）；错误消息里 "unbound identifier X" 的 X 后面还跟位置信息，取 token 必须取短语后的第一个而非最后一个。v1 的 env 用 interaction-environment（fresh `(environment …)` 不可变、拒绝 define 且对自由变量严格——实测；（fresh `(environment ...)` 不可变、拒绝 define 且对自由变量严格——实测；host 需 import `(ufo-match)`，探测失败则永久回退）；**必须展开包围 define 而非 library 表单**（library 顶层展开把定义推迟到编译期部件，运行时 IR 为空——实测只得到一个 `Lsrc:quote`）；annotated forms 按**文档内存文本**解析并缓存（sfd 直接建在字符串上——对未保存缓冲区天然正确，原磁盘一致性校验设计已废弃）；缓存按文本键、上限 64 份防编辑会话膨胀；miss 原因 `REPLAY_TRACE=1` 可见；命中统计 `(ir-auto-route:stats)` = (命中数 回退数 总毫秒)。ufo-match 库自身模板内的调用点（约 56 个）无 define 包围，按设计回退 route 2。
   **全仓金丝雀**（`bin/ir-route-canary.ss`，对本仓库 835 文档自分析，双模式 diff）：IR 模式命中 45 个真实用户调用点（256 个内部/模板调用点按设计回退），**诊断差异单向优于 route 2**——消除 14 条"Unused local variable"误报（实证：route 2 的重复引用损坏 usage-count，如 syntax-case.sls 的 clause-expression 明明被使用却被报未用；IR 模式无此问题），零新增诊断；引用总数 -371（0.03%，重复清理）；墙钟时间持平或更好（两模式 440-770 ms 区间波动）。**部署形态（修正 2026-10-20，同日初版结论被推翻）**：`scheme --script` 与 `compile-chez-program` 编译的二进制**都可用**——`sc-expand` 在编译程序里依然能解析编进二进制的 ufo-match 宏（探针实测 `(match 1 [x x])`、`(match '(1 2) [(a b) (+ a b)] …)` 均展开成功）。初版"编译二进制不可用"的结论是**探测探针自身的 bug**：探针用了 `[_ 1]`，而 ufo-match 对 `_` 通配符一律报 "misplaced aux keyword"（两种模式都有的既有 quirk，与 env 无关）——探针在编译二进制里误报 unavailable，IR 路径被静默关闭。`run.ss` 的 `(ufo-match)` import 仍是前提（没有它宏根本不会被编进二进制）。残留限制：版本偏移（二进制内嵌的 ufo-match 与用户工作区的副本语义相同——syntax-rules 宏族算法固定——但非该宏族的用户宏不在覆盖范围，与 route 2 一致）。翻转默认后的补充（同日）：多线程浸泡 6/6 干净（`tests/robustness/robustness-ir-route-threaded.sps`）；回退分类——金丝雀 256 个回退**全部是宏模板内伪调用点**（Chez 语义下非调用点，IR 正确地拒绝分析；route 2 的急切展开白分析它们，即 profile 中 +231 ms 的组成）；修复 `chez-ir-find-define-containing` 对点对的非正规尾（模板 `(pat . body)` 语法）崩溃。退役路线见 `doc/design/route2-retirement-proposal.md`。
3. **路线三 spike**（✅ 2026-10-20 通过，`tests/analysis/identifier/test-sc-expand-route3-spike.sps`）：单调用点端到端验证"用 Chez IR 替代 route 2 生成器管线"。流程：`(sc-expand define-form env #t)` → `chez-ir-call-site-refs` 提取 `(name ref-bias binding-bias)` → 按位置合成 identifier-reference（锚定 = 绑定位置；插入宏调用节点 imports 保证可见性；只合成宏派生绑定——调用点跨度内的，lambda 形参等普通绑定天然排除）。**结果：15/15 断言全绿；管线 11.1 ms（含进程内首次编译开销）vs route 2 冷调用 228 ms（≈20x），稳态 sc-expand 仅 ~0.5 ms**。宏内部临时量（v/w/failure）被调用点过滤天然排除——它们归属宏定义文件，本就不该出现在用户代码的引用层；route 3 恰好只产出 IDE 面向的引用层，等于同时绕开了 route 2 对整个展开树跑 step 的成本（ufo-match 规则体 ~8.3 s 嵌套 step 的最大单项）。
   **全面替换前待解的未知数**（spike 未覆盖）：每个工作区库的 env 构造（经 file-linkage 的 import specs 转换；自由变量在 fresh env 下严格、interaction env 宽松——脚本顶层调用点需要包装或回退）；usage-count 记账与 unused-imports/unused-local-variables 的接合；多调用点批处理与增量更新（文档变更→重展开该文档，IR 便宜故可行）；线程模式；非 ufo-match 的工作区自有宏仍走 route 2/1，router 变三路分发。
4. **expander-doc fallback 的替代**（中期）：fallback 路径现用 pretty-print + reparse（保真有损）。IR 路径下可用 `(path, line, col)` 把每个 `ref`/绑定**按位置直接匹配**回 index-node——比 gensym pretty-name 匹配可靠，天然免疫同名歧义。
4. 工程要求（已执行）：隔离封装（`chez-ir.sls` 单点），启动自检（`chez-ir-available?` 探测 3-arity `sc-expand` 与 `#%$record-type-descriptor`），Chez 升级时单点适配。

## 10. 与 Racket 机制对照

| | Racket `expand` | Chez `sc-expand ... #t` |
|---|---|---|
| 输出 IR | syntax object | nanopass Lsrc 记录树 |
| 表达式位置 | `syntax` 结构体 srcloc 字段 | `preinfo.src` |
| 引用位置 | 语法对象逐节点 | `ref` 的 `maybe-src`（逐引用） |
| 绑定位置 | 依赖宏作者 `syntax/loc` | `prelex.source`（固有） |
| 原始性判定 | `syntax-original?`（reader 标记 + macro scope 检查） | 自行合成：src 属于调用点文件 ⇒ 原始 |
| 宏引入归属 | macro scope / 模板位置 | 宏定义文件位置 / #f |
| 文档化 | 完全文档化 | 完全未文档化（本文是项目内记录） |
