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

> **2026-10-29 压缩**：本节原为 route-2 时代的四段落地日志（route 2 对照验证通道、 受控并行期、路线三 spike、expander-doc 替代设想），细节已随 route 2 退役失去时效。 完整脉络（含逐阶段实测数字、部署形态修正史、二十一条陷阱汇编、测试矩阵）见 [ir-auto-route-walkthrough.md](./ir-auto-route-walkthrough.md)；退役决策与证据汇总见 [route2-retirement-proposal.md](../design/route2-retirement-proposal.md)。 保留一句话结论：本文的机制探索（§1-§8、§10）已落地为 `analysis/identifier/expander/chez-ir.sls` + `ir-auto-route.sls`—— 未文档化访问全部单点封装、启动自检（`chez-ir-available?`）、Chez 升级时只改这一个文件。
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
