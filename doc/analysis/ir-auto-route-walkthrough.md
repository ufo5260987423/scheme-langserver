# match 宏自动解析全机制：源码逐环节走读

> 范围：一个 ufo-match `match` 调用点从"被 step 遇到"到"IDE 功能可用"的完整生命周期，逐环节对应到具体模块与函数。route 2（expanders/ 自研管线）已于 2026-10-21 彻底退役，本文描述的 IR 路径是唯一通路；失败语义见 §9。
> 前置阅读：`chez-sc-expand-records-switch.md`（`(sc-expand x env #t)` 开关的机制与发现历程）、`macro-analysis-framework.md`（E1–E4 理论框架）、`route2-retirement-proposal.md`（路线二退役提案，含证据汇总）。
> 环境：Chez Scheme 10.4.1，2026-10-20 当日全部机制经测试与金丝雀实测。
> 示例贯穿：fixture `tests/resources/workspace-fixtures/sc-expand-differential/consumer.scm.txt` 中 bias 131 的调用点 `(match input [(a b) (+ a b)] [('sym) 0] [(? number? n) n])`。

---

## 0. 总览

```
用户代码 (match …)
  │
  ▼
router.sls ─────────────────────────────── 分发：ufo-match 臂挂上"组合规则"
  │
  ▼
ir-auto-route:wrap-rule ────────────────── "IR 先、任意失败静默回退 route 2"
  │
  ├─► private:env ──────────────────────── 三级 env 引导（快路径 / load / 放弃）
  │     ├─ bootstrap-interaction           双向验证地探测 interaction env
  │     └─ bootstrap-source                精确 import 源 → load → fresh env（按源缓存）
  │
  ├─► private:forms ────────────────────── document-text → 带注解表单（按文本缓存）
  ├─► chez-ir-find-define-containing ───── span 下行定位包围 define
  │
  ├─〔interaction 模式〕chez-ir-expand ──── (sc-expand form env #t)
  └─〔fresh 模式〕private:expand-fresh ──── 注解伪造的 (let () form #f) 包装
       │                                    + 自由变量参数增长重试（≤8 次）
  │
  ├─► chez-ir-call-site-refs ───────────── IR 树遍历 → (name ref-bias binding-bias)
  │     （三分归属：调用点 / 宏定义文件 / 无；真伪鉴定：global-lookup? / quoted?）
  │
  └─► private:synthesize! ──────────────── 合成 identifier-reference
          宏派生过滤 + 去重 + 可见性/定义位置双角色分置 + speculative 标记

之后：step 照常步进 → 叶子解析 → find-available-references-for
      → usage-count → definition / hover / references / 检查器

失败：任意环节 guard 捕获 → stats 记账 miss → 调用点无展开派生引用
      （route 2 管线已于 2026-10-21 退役；回退 = 静默不分析）
```

## 1. 触发与分发 — `self-defined-rules/router.sls`

step（抽象解释器）步进文档树，遇复合节点时按头符号解析规则。头 `match` 解析到 `(ufo-match)` 库的引用 → 命中 ufo-match 臂（六宏族白名单）：

```scheme
(add-rule-procedure rules
  `((,(ir-auto-route:wrap-rule
      (expansion-generator->rule expander step file-linkage expanded+callee-list memory target-identifier)))
    . ,target-identifier))
```

挂上去的是 **IR-only 规则**：`(ir-auto-route:try-expand)`——route 2 退役后不再有任何包裹/回退（退役记录见 doc/design/route2-retirement-proposal.md）。

## 2. 组合规则入口 — `ir-auto-route.sls` `ir-auto-route:wrap-rule`

```scheme
(lambda (root-file-node root-library-node document index-node)
  (if (and private:gate? (chez-ir-available?))
    (or (private:try root-library-node document index-node)              ; IR 路径
      (route2-rule root-file-node root-library-node document index-node)) ; 回退
    (route2-rule root-file-node root-library-node document index-node))))
```

- `private:gate?`：默认开，`SLSCHEME_IR_ROUTE=0/off` 逐进程关闭（二分排障用）；
- `chez-ir-available?`（`chez-ir.sls`）：两项进程级自检——3-arity `sc-expand` 可用 + `#%$record-type-descriptor` 正常。**Chez 升级破坏未文档化 API 时在这里降级为 route 2，不是崩溃。**

## 3. env 引导（v2 核心）— `private:env` / 两级 bootstrap

按源路径缓存（`private:env-cache-ht`，`equal-hash`；键为 `'interaction` 符号或源路径字符串——`string-hash` 表塞符号键会每次抛异常，已修）。

### 3a. 快路径 `private:bootstrap-interaction`

探测 `(interaction-environment)`，但**双向验证**（`private:probe-in`）：

```scheme
(let ([ir (sc-expand '(match 1 [x x]) env #t)])
  (and (record? ir)
    (not (chez-ir-global-lookup? ir "match"))        ; 负向：无宽松全局查找
    (chez-ir-contains-quoted-symbol? ir 'match)))    ; 正向：真展开带 (quote match)
```

**为什么必须双向**（实测教训）：interaction env **宽松**——未绑定的 `match` 会静默展开成 `($top-level-value 'match)` 而**不报错**，朴素探测全部假阳性。两个谓词（`chez-ir.sls`）：

- `chez-ir-global-lookup?`：在 IR 中找 `($top-level-value '<名>)` 调用形状——call 记录字段为 `(preinfo operator arglist…)`（operator 是第二个字段，参数尾是单个列表字段），且需**下潜 annotation**（IR 子树和位置都裹在注解里）；
- `chez-ir-contains-quoted-symbol?`：确认 ufo-match 失败续延的 `(error 'match …)` 留下 `(quote match)`；
- 二者共用 `private:record-walk`（pair + record 双走，seen 去环）。

### 3b. load 回退 `private:bootstrap-source`

`private:find-source` **三级源定位**：

```scheme
(or (private:import-source document call-node)     ; ① 该调用点 match 头实际解析到的文档
  (and (library-node? root-library-node)          ; ② workspace 库树 (walk-library '(ufo-match))
    ...)
  (private:libdirs-source))                        ; ③ CHEZSCHEMELIBDIRS 扫描
```

① 是精确性的关键：一个 workspace 可能有**多个 (ufo-match) 副本**（本仓库的测试 fixture 就装着旧版 Chicken 风格变体），`walk-library` 取第一个会拿错版本；`find-available-references-for document call-node 'match` 返回的头引用所指向的 document，才是**这个文件的 import 真正解析到的副本**。

拿到源后 `(load source)`——显式 load 把库注册进 Chez 全局库管理器（脚本/编译二进制行为一致，compile-chez-program 探针实测），随后 `(environment '(chezscheme) '(ufo-match))` 构造 fresh env 并 `probe-in` 验证。**每个源路径一个缓存项**——一个 server 进程分析多个工作区、各有不同宏副本时互不干扰。

## 4. 表单获取与 define 定位

```scheme
[forms (private:forms text path)]        ; 从 document-text 直接解析
[form  (chez-ir-find-define-containing forms (index-node-start call-node))]
```

- `private:forms`：`get-datum/annotations` 循环（sfd 直接建在字符串上）——**对未保存缓冲区天然正确**（v1 的磁盘校验设计已废弃）；按文本缓存，上限 64 份防编辑会话膨胀；
- `chez-ir-find-define-containing`（`chez-ir.sls`）：span memoized 下行，找含 bias 的最小 `(define …)`。**必须定位 define 而非 library 表单**——library 顶层展开把定义推迟到编译期部件，运行时 IR 为空（实测只得到一个 `Lsrc:quote`）。会下潜**非正规尾**（模板点对语法 `(pat . body)`，修复过的崩溃）。

## 5. 展开（两种模式）

**interaction 模式**：`chez-ir-expand form env` = `(sc-expand form env #t)`。Chez 在可变宽容环境里完成整个级联（match → match-next → match-one → match-two → new-sym?），产出带位置的 parsed Lsrc IR。级联中的宏引入、卫生改名、check 消除全是 Chez 原生语义。

**fresh 模式** `private:expand-fresh`（fresh env **不可变**：拒顶层 define、对自由变量严格）：

```scheme
(let loop ([params '()] [attempts 0])
  (guard (e [else
      (let ([unbound (private:unbound-identifier-from-message …)])
        (if (and unbound (< attempts 8) (not (memq unbound params)))
          (loop (cons unbound params) (+ attempts 1))   ; 加形参重试
          (raise e)))])
    (sc-expand (private:wrap-form form params) env #t)))
```

- `private:wrap-form`：`(let () <form> #f)`——内层 define 在不可变环境也被接受（实测）；尾 `#f` 满足 "no expressions in body"；**包装整体用 `make-annotation` 伪造**，内部注解原样穿透 → **IR 里每个 ref/binding 的源位置保持原文偏移**（实测：`(input 31 18)` 类三元组的 bias 与原文一致）；
- 自由变量重试：严格 env 报 `attempt to reference unbound identifier helper at char 33 …` → `private:unbound-identifier-from-message` 取**短语后的第一个 token**（消息后跟位置信息，取最后一个会拿到文件名）→ 包一层 `(lambda (helper …) (let () form #f))` 重试，上限 8 次。这是"调用同库 sibling 函数"的真实工作区形态能被展开的原因。

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

绑定位置取自 `prelex.source`——其中是**注解标识符**（再剥一层 `annotation-source` 到 source object 取 bfp），`ref` 的 maybe-src 则是裸 source object。模式变量绑定因此落在**模式位置**（`a` → 152）——这是 Chez 原生归属，也是 goto-definition 跳得准的根源，与 route 2 修复后的锚定一致（差分测试双模式 15/15 的共同基准）。

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
- **双角色分置**（Tier-1.5 修复确立的设计）：插入点在**宏调用节点**（所有 use 的祖先，保证可解析——插到锚点会让兄弟 use 解析归零，实测）；引用的 `index-node` = 恢复出的绑定位置（goto-def 目标）；同时挂到锚点的 exports（route-2 兼容附着面，既有 IDE 功能与测试探的是它）。

## 8. 命中之后：引用"上班"

规则返回 `#t`，step 照常步进子节点。到 body 叶子（bias 160）：

```scheme
;; abstract-interpreter.sls 叶子分支
(find-available-references-for document body-a-node 'a)
;; 沿父链收集 imports → 命中调用节点上的合成 a-绑定（锚 152）→ usage-count++
```

从此 definition（返回 152）、hover、references、unused-imports / unused-local-variables 全部正常工作，且 usage-count 是**干净的**——金丝雀实证 route 2 的重复引用会把它搞坏（14 条 "Unused local variable" 误报），IR 路径零误报、诊断单向更优。

## 9. 失败路径与可观测性

`private:try` 整体包在 `guard` 中：任何失败 → `private:bump! (cons #f ms)` → 返回 `#f` → **调用点静默得不到展开派生引用**（route 2 已于 2026-10-21 彻底退役，见 doc/design/route2-retirement-proposal.md）。这包括：`form-not-found`（宏模板内的伪调用点——Chez 语义下不是调用点，IR 正确地拒绝分析，route 2 时代曾过度分析它们）、env 不可用、sc-expand 报错等。

可观测性三件套：

| 入口 | 内容 |
|---|---|
| `(ir-auto-route:stats)` | `(命中 回退 总毫秒)`，互斥锁保护（threaded 安全） |
| `(ir-auto-route:env-source)` | `'interaction \| (loaded . path) \| 'unavailable` |
| `REPLAY_TRACE=1` | 每次 miss 的完整条件文本 + bootstrap 源信息 |

金丝雀（`bin/ir-route-canary.ss`）对本仓库自分析：IR 路径 43-45 命中、~256 回退——回退全部是宏模板内的伪调用点（Chez 语义下不是调用点，IR 正确地拒绝分析；route 2 曾白分析它们，即 profile 中 +231 ms 的组成）。

## 10. 测试锚定矩阵

| 测试 | 锚定的环节 |
|---|---|
| `tests/analysis/identifier/test-sc-expand-differential.sps` | 6+8：IR 地面真值 vs 分析结果（route 2 与 IR 模式**双模式** 15/15） |
| `tests/analysis/identifier/test-ir-auto-route.sps` | 1–8 全链（interaction 快路径，真实 workspace init） |
| `tests/analysis/identifier/test-ir-auto-route-load.sps` | 3b+5：load 源定位、fresh env、let 包装、**自由变量重试**、单锚点 |
| `tests/analysis/identifier/test-sc-expand-route3-spike.sps` | 6+7 纯机制 + 性能门（<50 ms，实测 ~11 ms） |
| `tests/robustness/robustness-ir-route-threaded.sps` | 全链并发安全（6 次 threaded init，25/25） |
| `bin/ir-route-canary.ss` | 全仓端到端（双模式 diff 诊断/引用数/耗时） |

## 11. 一句话总结

**router 认出 match 并转交；ir-auto-route 备好 Chez 环境（源优先 + interaction 兜底）、让 Chez 展开（fresh env 的注解伪造 let 包装 + 两类阻塞标识符的参数化）、把 IR 里的 (use, definition) 事实翻译成 identifier-reference 贴回文档（双角色合成）；chez-ir 承担与 Chez 未文档化 IR 对话的全部脏活（含探针真伪鉴定）；route 2 的自研展开管线已彻底退役，失败即静默不分析。**
