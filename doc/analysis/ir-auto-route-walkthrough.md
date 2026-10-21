# match 宏自动解析全机制：源码逐环节走读

> 范围：一个 ufo-match `match` 调用点从"被 step 遇到"到"IDE 功能可用"的完整生命周期，逐环节对应到具体模块与函数。**route 2（expanders/ 自研展开管线）已于 2026-10-21 彻底退役**（commit 0ac9f75c / 6067b65b，决策记录见 `doc/design/route2-retirement-proposal.md`），本文描述的 IR 路径是唯一通路：失败 = 调用点静默得不到展开派生引用。
> 前置阅读：`chez-sc-expand-records-switch.md`（`(sc-expand x env #t)` 开关的机制与发现历程）。
> 示例贯穿：fixture `tests/resources/workspace-fixtures/sc-expand-differential/consumer.scm.txt` 中 bias 131 的调用点 `(match input [(a b) (+ a b)] [('sym) 0] [(? number? n) n])`；金丝雀数字来自本仓库自分析（816 文档，110 命中 / 40 回退；回退几乎全是宏模板内的伪调用点）。
> 机制自 2026-10-21 起**宏无关**：除 ufo-match 六宏族外，`match-index-node`（库 `(scheme-langserver virtual-file-system index-node)`）也走同一通路——头符号与其库从调用点推导，env 按"该库的副本"引导。
> 最后更新：2026-10-21（route 2 退役 + match-index-node 泛化后全面重写）。

---

## 0. 总览

```
用户代码 (match …)
  │
  ▼
router.sls ─────────────────────────────── 分发：ufo-match 六宏族臂挂上 IR-only 规则
  │
  ▼
ir-auto-route:try-expand ───────────────── 门控 + 特性自检 → private:try
  │
  ├─► private:env ──────────────────────── env 引导（源优先；interaction 兜底）
  │     ├─ bootstrap-source                三级源定位 → load → fresh env（按源缓存）
  │     │     ① 该调用点 match 头的 import 解析目标（精确）
  │     │     ② workspace 库树 walk-library '(ufo-match)
  │     │     ③ CHEZSCHEMELIBDIRS 扫描
  │     └─ bootstrap-interaction           双向探测 interaction env（最后兜底）
  │
  ├─► private:forms ────────────────────── document-text → 带注解表单（按文本缓存）
  ├─► chez-ir-find-define-containing ───── span 下行定位包围 define
  │
  ├─〔fresh 模式〕private:expand-fresh ──── 注解伪造的 (let () form #f) 包装
  │     └─ 双类阻塞标识符参数增长重试（≤64 次）：
  │          "unbound identifier X" / "misplaced aux keyword (X …)"
  └─〔interaction 模式〕chez-ir-expand ──── (sc-expand form env #t) 直接展开
  │
  ├─► chez-ir-call-site-refs ───────────── IR 树遍历 → (name ref-bias binding-bias)
  │     （三分归属：调用点 / 宏定义文件 / 无；探针真伪鉴定谓词同此层）
  │
  └─► private:synthesize! ──────────────── 合成 identifier-reference
          宏派生过滤 + 去重 + 可见性/定义位置双角色分置 + speculative 标记

之后：step 照常步进 → 叶子解析 → find-available-references-for
      → usage-count → definition / hover / references / 检查器

失败：任意环节 guard 捕获 → (ir-auto-route:stats) 记账 miss
      → 调用点静默无展开派生引用（无回退路径）
```

## 1. 触发与分发 — `self-defined-rules/router.sls`

step（抽象解释器）步进文档树，遇复合节点时按头符号解析规则。头 `match` 解析到 `(ufo-match)` 库的引用 → 命中六宏族臂（match / match-next / match-one / match-check-ellipsis / match-two / match-check-identifier）：

```scheme
[(and (equal? library-identifiers '((ufo-match)))
    (find (lambda (e) (equal? expressions (list e)))
      '(match match-next match-one match-check-ellipsis match-two match-check-identifier)))
  (add-rule-procedure rules `((,(ir-auto-route:try-expand)) . ,target-identifier))]
```

两条臂：

1. **ufo-match 六宏族**（库 `(ufo-match)`）：match / match-next / match-one / match-check-ellipsis / match-two / match-check-identifier；
2. **match-index-node**（库 `(scheme-langserver virtual-file-system index-node)`，2026-10-21 泛化加入）——langserver 自己的规则文件大量使用，加入后金丝雀命中从 25 涨到 110。

与退役前的区别：

- **不再需要 langserver 侧的 expander**（`private:find-expander` 已删）——IR 路径靠 Chez 自己的 expander 解析头符号，臂只负责"哪些标识符挂 IR 规则"；机制内部完全宏无关：`private:try` 从调用点推导头符号与库标识，`private:env` 按**该库**的副本引导（fresh env = `(chezscheme) + <头符号的库>`，缓存键 = 源路径 × 库）；
- **不再包裹 route-2 规则**。`SLSCHEME_IR_ROUTE=0` 的语义变为"不做自动解析"（旧的 route-2 逃生门随管线一起消失）。

## 2. 规则入口 — `ir-auto-route:try-expand`

```scheme
(define (ir-auto-route:try-expand)
  (lambda (root-file-node root-library-node document index-node)
    (and private:gate? (chez-ir-available?)
      (private:try root-library-node document index-node))))
```

- `private:gate?`：默认开，`SLSCHEME_IR_ROUTE=0/off` 逐进程关闭（二分排障用）；
- `chez-ir-available?`（`chez-ir.sls`）：进程级自检——3-arity `sc-expand` 可用 + `#%$record-type-descriptor` 正常。**Chez 升级破坏未文档化 API 时在此降级（该家族不自动解析），不是崩溃。**

## 3. env 引导 — `private:env`（源优先）

返回 `(env . mode)`，`mode ∈ (fresh | interaction)`；缓存为按源路径分键的哈希表（`equal-hash`——键是路径字符串或 `'interaction` 符号；`string-hash` 表塞符号键会每次抛异常，已踩过）。

### 3a. 源优先 `private:bootstrap-source`（主路径，确定性）

`private:find-source` **三级源定位**：

```scheme
(or (private:import-source document call-node)     ; ① 该调用点 match 头实际解析到的文档
  (and (library-node? root-library-node)          ; ② workspace 库树 (walk-library '(ufo-match))
    ...)
  (private:libdirs-source))                        ; ③ CHEZSCHEMELIBDIRS 扫描
```

① 是精确性的关键：一个 workspace 可能有**多个 (ufo-match) 副本**（本仓库的测试 fixture 就装着旧版 Chicken 风格变体），`walk-library` 取第一个会拿错版本；`find-available-references-for document call-node 'match` 返回的头引用所指向的 document，才是**这个文件的 import 真正解析到的副本**（金丝雀实证：fixture 副本与 .akku 副本并存时，① 总能选对）。

拿到源后 `(load source)`——显式 load 把库注册进 Chez 全局库管理器（脚本/编译二进制行为一致，compile-chez-program 探针实测），随后 `(environment '(chezscheme) '(ufo-match))` 构造 **fresh env** 并 `probe-in` 双向验证。**每个源路径一个缓存项**——一个 server 进程分析多个工作区、各有不同宏副本时互不干扰。

**为什么源优先于 interaction**（2026-10-21 实测定论，金丝雀 13 站点迁移案）：fresh env 只含 `(chezscheme) + (ufo-match)`，含义完全确定；而 interaction env 的内容随 **host 进程的 import 面**变化——曾发生某 host 的传递依赖意外遮蔽了 Chez 基环境的 `except` 辅助关键字，使同一份用户代码"有时能展开有时不能"。展开语义绝不能依赖这种偶然性。

### 3b. interaction 兜底 `private:bootstrap-interaction`

仅当找不到任何源时：探测 `(interaction-environment)`——**双向验证**（`private:probe-in`）：

```scheme
(let ([ir (sc-expand '(match 1 [x x]) env #t)])
  (and (record? ir)
    (not (chez-ir-global-lookup? ir "match"))        ; 负向：无宽松全局查找
    (chez-ir-contains-quoted-symbol? ir 'match)))    ; 正向：真展开带 (quote match)
```

双向验证的必要性（实测教训）：interaction env **宽松**——未绑定的 `match` 会静默展开成 `($top-level-value 'match)` 而**不报错**，朴素探测全部假阳性。两个谓词（`chez-ir.sls`）：

- `chez-ir-global-lookup?`：在 IR 中找 `($top-level-value '<名>)` 调用形状——call 记录字段为 `(preinfo operator arglist…)`（operator 是第二个字段，参数尾是单个列表字段），且需**下潜 annotation**（IR 子树和位置都裹在注解里）；
- `chez-ir-contains-quoted-symbol?`：确认 ufo-match 失败续延的 `(error 'match …)` 留下 `(quote match)`；
- 二者共用 `private:record-walk`（pair + record 双走，seen 去环，annotation 下潜）。

探针用 `(match 1 [x x])` 而非 `[_ 1]`：ufo-match 对 `_` 通配符在**所有模式**下报 "misplaced aux keyword"（与 env 无关的既有 quirk），用 `_` 探针曾把编译二进制误判为不可用。

## 4. 表单获取与 define 定位

```scheme
[forms (private:forms text path)]        ; 从 document-text 直接解析
[form  (chez-ir-find-define-containing forms (index-node-start call-node))]
```

- `private:forms`：`get-datum/annotations` 循环（sfd 直接建在字符串上）——**对未保存缓冲区天然正确**（磁盘一致性校验的 v1 设计已废弃）；按文本缓存，上限 64 份防编辑会话膨胀；
- `chez-ir-find-define-containing`（`chez-ir.sls`）：span memoized 下行，找含 bias 的最小 `(define …)`。**必须定位 define 而非 library 表单**——library 顶层展开把定义推迟到编译期部件，运行时 IR 为空（实测只得到一个 `Lsrc:quote`）。会下潜**非正规尾**（模板点对语法 `(pat . body)`——修复过的 "not a pair" 崩溃）。

## 5. 展开（两种模式）

### fresh 模式 `private:expand-fresh`（主路径）

fresh env **不可变**（拒顶层 define、对自由标识符严格），因此：

```scheme
(let loop ([params '()] [attempts 0])
  (guard (e [else
      (let ([blocked (private:expand-blocked-identifier-from-message …)])
        (if (and blocked (< attempts 64) (not (memq blocked params)))
          (loop (cons blocked params) (+ attempts 1))   ; 加形参重试
          (raise e)))])
    (sc-expand (private:wrap-form form params) env #t)))
```

**`private:wrap-form`**：`(let () <form> #f)`（有自由标识符时为 `(lambda (free …) (let () <form> #f))`）：

- 内层 define 在不可变环境也被接受（实测）；尾 `#f` 满足 "no expressions in body"（只有定义没有表达式会被拒）；
- **包装整体用 `make-annotation` 伪造**，内部注解原样穿透 → **IR 里每个 ref/binding 的源位置保持原文偏移**（实测：`(input 31 18)` 类三元组的 bias 与原文一致）；
- 被包进 wrapper 的标识符对展开**不透明**——我们只关心 match 调用的事实，外围 define 里的其他形式不需要被真正展开。

**双类阻塞标识符重试**（`private:expand-blocked-identifier-from-message`）——严格 env 的两种拒绝都被同一机制中和：

| 错误消息 | 含义 | 提取 |
|---|---|---|
| `attempt to reference unbound identifier X at char N …` | 自由变量（同库 sibling 函数等） | 短语后的第一个 token |
| `misplaced aux keyword (X …)` | **Chez 基环境自己声明的辅助关键字**（实测：`except`）被用户当函数名 | 短语后跳过 `(` 取 token |

第二类是 2026-10-21 的实测发现：`except` 在 chezscheme 基环境中就是辅助关键字，**任何**含 chezscheme 的环境都拒绝对 `(except c …)` 形式的展开——route 2 时代某些文件能过纯属特定 host import 面对 `except` 的偶然遮蔽。重试上限 64（`analysis/type/domain-specific-language/interpreter.sls` 曾超 8 次上限丢站点，实测）；token 必须取**短语之后**的第一个——消息尾部跟位置信息，取最后一个会拿到文件名。

### interaction 模式 `chez-ir-expand`

可变宽容环境直接 `(sc-expand form env #t)`，无需包装。Chez 完成整个级联（match → match-next → match-one → match-two → new-sym?），卫生、作用域、check 消除全是原生语义。

## 6. IR → 调用点事实 — `chez-ir-call-site-refs`

遍历 IR 记录树，收集 `Lsrc:ref` 中**源位置属于本文件**的引用：

```
(match input [(a b) (+ a b)] …)
  ⇒ ((a 160 152) (b 162 154) (n 204 201) (input 138 120))
     名字  引用bias  绑定bias
```

三分归属（Chez 卫生机制的天然产物）：

| IR 节点 | 源位置 | 处理 |
|---|---|---|
| scrutinee / body 拷贝 | 调用点文件 | **保留**（调用点事实） |
| 宏模板引入的引用 | 宏定义文件 | 被 path 过滤 |
| 纯生成临时量（v/w/failure） | 无 | 被过滤 |

绑定位置取自 `prelex.source`——其中是**注解标识符**（再剥一层 `annotation-source` 到 source object 取 bfp），`ref` 的 maybe-src 则是裸 source object。模式变量绑定因此落在**模式位置**（`a` → 152）——Chez 原生归属，goto-definition 跳得准的根源；差分测试以它为地面真值（15/15）。

## 7. 合成引用 — `private:synthesize!`

```scheme
(when (and macro-derived? (in-call-span? …))         ; 绑定位置落在调用点跨度内
  (unless (hashtable-ref seen (list name binding-bias) #f)
    (let* ([picked (pick-index-node-from … binding-bias)]
        [anchor (if (index-node? picked) picked call-node)]
        [ref (make-identifier-reference name document anchor call-node '() 'variable '() '())])
      (identifier-reference-speculative?-set! ref #t)
      (index-node-references-export-to-other-node-set! anchor (append … `(,ref)))        ; 定义位置
      (index-node-references-import-in-this-node-set! call-node (sort … (cons ref …)))))) ; 作用域可见性
```

- **宏派生判定**：绑定在调用点跨度内才算宏产物；`input` 的绑定（120，lambda 形参）在跨度外 → 跳过（普通规则已注册，天然排除）；
- **去重**：按 `(name . binding-bias)`；
- **双角色分置**：插入点在**宏调用节点**（所有 use 的祖先，保证可解析——插到锚点会让兄弟 use 解析归零，实测）；引用的 `index-node` = 恢复出的绑定位置（goto-def 目标）；同时挂到锚点的 exports（既有 IDE 功能与测试探的附着面）；
- 与 route 2 时代的本质区别：**零重复引用**（route 2 的 pair 回传会产生重复 speculative 引用，曾损坏 usage-count 造成 14 条 "Unused local variable" 误报；IR 路径单锚点，金丝雀诊断数历史最低）。

## 8. 命中之后：引用"上班"

规则返回 `#t`，step 照常步进子节点。到 body 叶子（bias 160）：

```scheme
;; abstract-interpreter.sls 叶子分支
(find-available-references-for document body-a-node 'a)
;; 沿父链收集 imports → 命中调用节点上的合成 a-绑定（锚 152）→ usage-count++
```

从此 definition（返回 152）、hover、references、unused-imports / unused-local-variables 全部正常工作，usage-count 干净。

## 9. 失败语义与可观测性

`private:try` 整体包在 `guard` 中：任何失败 → `private:bump! (cons #f ms)` → 返回 `#f` → **调用点静默得不到展开派生引用**。回退构成（金丝雀 37 例实证）几乎全是 `form-not-found`——**宏模板内的伪调用点**（Chez 展开宏定义时模板是数据不是代码，IR 路径正确地拒绝分析；route 2 时代曾白分析它们，即历史 profile 中 +231ms 急切成本的组成）。

可观测性三件套：

| 入口 | 内容 |
|---|---|
| `(ir-auto-route:stats)` | `(命中 回退 总毫秒)`，互斥锁保护（threaded 安全） |
| `(ir-auto-route:env-source)` | `'interaction \| (loaded . path) \| 'unavailable` |
| `REPLAY_TRACE=1` | 每次 miss 的完整条件文本（含文档 URI 与 bias）+ bootstrap 源信息 |

## 10. 测试锚定矩阵

| 测试 | 锚定的环节 |
|---|---|
| `tests/analysis/identifier/test-sc-expand-differential.sps` | 6+8：Chez IR 地面真值 vs 分析结果（15/15） |
| `tests/analysis/identifier/test-ir-auto-route.sps` | 1–8 全链（真实 workspace init） |
| `tests/analysis/identifier/test-ir-auto-route-load.sps` | 3a+5：load 源定位、fresh env、let 包装、**双类重试**、单锚点（10 断言） |
| `tests/analysis/identifier/test-sc-expand-route3-spike.sps` | 6+7 纯机制 + 性能门（<50 ms，实测 ~11 ms） |
| `tests/robustness/robustness-ir-route-threaded.sps` | 全链并发安全（6 次 threaded init，25/25） |
| `tests/analysis/identifier/test-ir-auto-route-match-index-node.sps` | 宏无关泛化：match-index-node 走完整 IR 链路（真实仓库 init，GT 11 项全中） |
| `bin/ir-route-canary.ss` | 全仓端到端（命中/回退/引用数/诊断数；自分析本仓库） |
| `tests/protocol/apis/test-{definition,references,document-symbol}.sps` | 8：IDE API 冒烟 |

（route 2 时代的 14 个测试随管线一并删除；`rules/syntax-rules.sls` / `rules/syntax-case.sls` 及其测试**保留**——clause-process 的子句符号注册属于普通 define 分析，不是宏展开。）

## 11. 实测陷阱汇编（本机制的全部"坑"清单）

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

## 12. 一句话总结

**router 认出六宏族并转交 `try-expand`；ir-auto-route 以"源优先"引导出确定性的 Chez 环境（工作区自己的宏副本，load 注册 + fresh env）、用注解伪造的 let 包装 + 双类阻塞标识符参数化让 Chez 展开（位置零损耗）、把 IR 里的 (use, definition) 事实按双角色合成 identifier-reference 贴回文档；chez-ir 承担与 Chez 未文档化 IR 对话的全部脏活（含探针真伪鉴定）；失败即静默不分析——route 2 的自研展开管线已成为历史。**
