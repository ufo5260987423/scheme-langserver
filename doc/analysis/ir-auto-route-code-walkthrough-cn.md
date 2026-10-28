# IR 自动路由代码走读（标识符侧，以 match 为例）

> 调研日期 2026-10-28。基于 `kimi` 分支 47cb4620。
> 相关文档：`doc/analysis/ir-auto-route-walkthrough.md`（机制级 walkthrough）、
> `doc/analysis/chez-sc-expand-records-switch.md`（Chez records 开关）、
> `doc/design/route2-retirement-proposal.md`（route 2 退役决策记录）。

## 0. 全景调用链

```
step (abstract-interpreter.sls)
  → 对文档里每个"不是普通值/过程"的 head identifier 构造 rules
  → construct-rules 内建规则表（let/lambda/syntax-case/...）未命中时
  → route&add (analysis/identifier/self-defined-rules/router.sls)
  → 命中 ufo-match 族 → rules 里挂上 (ir-auto-route:try-expand) 这个 rule lambda
  → step 走到 (match xs [(a b) ...]) 调用点时执行该 rule
  → private:try (analysis/identifier/expander/ir-auto-route.sls)
  → sc-expand → chez-ir walkers → 合成引用
```

关键点：`route&add` 本身**不做展开**，只做模式匹配式分发。对 `(match ...)`：

```scheme
[(and (equal? library-identifiers '((ufo-match)))
    (find (lambda (e) (equal? expressions (list e)))
      '(match match-next ...)))          ; head 解析到 (ufo-match) 库的 match
  (add-rule-procedure rules `((,(ir-auto-route:try-expand)) . ,target-identifier))]
```

`ir-auto-route:try-expand` 返回一个零参数的 thunk 工厂：
`(lambda (root-file-node root-library-node document index-node) ...)`，
符合 rule 调用约定。这个 rule 对**每一个 match 调用点**执行一次。

## 1. ir-auto-route.sls —— 编排层（737 行）

`private:try` 的流程，以 `(define (f xs) (match xs [(a b) (g a b)]))` 为例：

### ① 确定 head 的库（macro-agnostic 的信息源）

从 call-node 第一个子节点取 head 符号：
```scheme
(let* ([head-symbol (index-node-expression (car (index-node-children call-node)))]
    [head-refs (find-available-references-for document call-node head-symbol)]
    [library-identifier ...])
```
用 `find-available-references-for` 查到 head 解析到哪个库（这里是 `(ufo-match)`）。
这个机制不写死 match——`match-index-node` 走的完全是同一个入口。

### ② forms 缓存

`private:forms` 用文档 **text**（不是磁盘）重新 `get-datum/annotations`
解析出带注解的顶层 form 列表，按 text 哈希缓存（上限 64 条，didChange 时
text 变即自然失效）。为什么按 text：编辑中的脏缓冲区也要能走这条路，
否则整个编辑会话全回退。

### ③ 定位待展开的 form

`chez-ir-find-define-containing` 找包含调用点 bias 的**最小 `(define ...)` form**。
- 为什么不是整个 library form：对 library 顶层 `sc-expand` 会把定义推迟到
  编译期部分，返回的运行时 IR 是空的（单个 quote）。
- span 通过子节点合并计算并 memoize；逐 child 检查 span 包含性，
  dotted template 的不规范尾部也当节点递归。

### ④ env 引导（`private:env`，SOURCE-FIRST 顺序）

1. `private:import-source`：通过调用点自己的 import 解析找到 head 所在宏库的
   **具体源文件**（优先；一个 workspace 可能有同一库的多个副本，消费方的
   import 解析知道哪个是对的）；
2. 退回 `walk-library` 在 library-node 树里找；
3. 再退回 `CHEZSCHEMELIBDIRS` 扫 `ufo-match.sls` 等；
4. 都找不到才退回 interaction env（容错但非确定性；注释里有 ufo-try 的
   影子 `except` 事故记录）。

找到源后：`(load source)` 注册到 Chez 全局 library manager（script 和
编译二进制都可用），然后 `private:build-env` 构造**精确编译期语义的 env**：
`(chezscheme) + head 库 + 文档自身的 import sets`（workspace root 临时
prepend 到 `library-directories`），构造失败退回最小二元 env。
env 按 `source × library × imports` 缓存。

### ⑤ 双向 probe 验证

interaction/fresh env 都很宽松，naive probe 会假阳性（unbound `match`
展开成 `($top-level-value 'match)` 不报错）。所以 `private:probe-in` 同时：

- **negative**：IR 里不得出现 `($top-level-value '<head>)`（真展开会消费关键字）；
- **positive**（仅 match 本尊）：failure thunk 带 `(quote match)`，
  用 `chez-ir-contains-quoted-symbol?` 确认。

### ⑥ fresh 模式的展开包装

fresh env 不可变、对自由标识符严格，直接 sc-expand 用户 define 会炸
（兄弟 define 互相不可见）。`private:expand-fresh` 把 form 包成注解版的
`(let () form #f)`（内部 define 被接受，尾部 `#f` 满足"body 至少一个
表达式"；wrapper 伪造为 annotation 使内层位置不丢），再按需包
`(lambda (free...) ...)`。自由变量列表**从错误消息里生长**：捕获
`unbound identifier X` / `misplaced aux keyword (X ...)`（Chez 自己把
`except` 声明为 aux 关键字）两种错误，把 X 加进参数列表重新展开，
上限 64 次。发现过的参数按 `(form 身份 × env 身份)` memo
（47cb4620 的优化，225 次展开省掉 196 次重试）。

### ⑦ 展开 + 取事实 + 合成

```scheme
[ir      (sc-expand form env #t)]               ; records switch，见下
[refs    (chez-ir-call-site-refs ir path)]      ; 使用驱动：源码在本文档的 Lsrc:ref
[bindings (chez-ir-call-site-bindings ir path)] ; 绑定驱动：补从未使用的 pattern var
[inserted (private:synthesize! (merge refs bindings) document call-node)]
```

### ⑧ private:synthesize!

把 IR 事实物化成 langserver 的 identifier-reference：

- 按 `(name . binding-bias)` 分组，每组**一个** reference
  （消除共享 call-node scope 时代的重复候选）；
- 只接受 binding-bias 落在本调用点 span 内的条目（嵌套调用点各归各）；
- **anchor** = binding-bias 处的 index-node（跳转定义的目标）；
  **scope** = call-node 的能同时包住 binding 与全部 uses 的直接子节点——
  即 per-clause 作用域：`(match xs [(a b) ...] [(a b c) ...])` 里第二个
  clause 的 `c` 不会泄漏进第一个 clause；
- anchor 若已导出同名标识符（clause body 里真有 lambda 参数注册了）则
  跳过，避免重复；
- reference 标 `speculative? #t`，写入 anchor 的
  `references-export-to-other-node` 和 scope 的
  `references-import-in-this-node`（排序插入）。

任何一步失败：静默返回 `#f`，调用点只是拿不到展开派生的引用，普通规则
照跑；原因记在 `ir-auto-route:stats` / `REPLAY_TRACE` 环境变量里。

## 2. chez-ir.sls —— IR 工具层（870 行）

全项目**唯一** `#!chezscheme` 文件，把所有未公开 API（`sc-expand` 第三参、
`#%$record-type-descriptor`、nanopass 的 `Lsrc:*` record）隔离在这一处——
Chez 升级只会坏这一个文件。注意顶部警告：公开 `record-type-descriptor`
碰到 kernel record 会抛出**不可捕获**的硬错误，所以处处用
`#%$record-type-descriptor` 且绝不裸碰 annotation record。

| 件 | 干什么 |
|---|---|
| `chez-ir-available?` | 特性自检（3 参 sc-expand 可调用 + kernel record 可读），失败则整条 IR 路优雅降级 |
| `chez-ir-read-annotated-file` / 内存版 `forms-from-text` | `get-datum/annotations` 的 bfp 协议循环，逐 form 读出带注解 datum |
| `chez-ir-find-define-containing` | 上面 ③ 用的 span 定位 |
| `chez-ir-expand` | 就是 `(sc-expand datum env #t)`——**records?=#t 开关**：跳过 `$uncprep` 反解析，返回保留全部源码位置的 Lsrc IR |
| `chez-ir-call-site-refs` | 遍历 IR 收集源码文件 == 本文档的 `Lsrc:ref`（`#(maybe-src x)` 字段），输出 `(name ref-bias binding-bias)`；binding-bias 来自 prelex 的 source，宏引入的绑定为 `#f` |
| `chez-ir-call-site-bindings` | 遍历所有 prelex **绑定**（letrec 的 x*、case-lambda clause 形参），源码在本文档的按 `(name binding-bias)` 去重收集。补上"从未被使用的 pattern var"这种 use 驱动收集器永远看不到的绑定 |
| `chez-ir-global-lookup?` / `chez-ir-contains-quoted-symbol?` | probe 的双侧判定（识别 `Lsrc:call` 套 `primref $top-level-value` 套 `Lsrc:quote` 的特定形状 / 任意 quote record） |
| `chez-ir->annotated-sexp` | `$uncprep` 的逆操作（**类型侧**用）：把 IR record 重新组装成带注解的 sexp 树——每个节点包 annotation（位置来自 preinfo.src / ref maybe-src / prelex.source；未知位置共享 dummy sfd，因为 index-node 构建器要求每个节点都带 source 且会丢非注解子节点）；primref 还原成名字、prelex 还原成用户标识符；**case-lambda 应用按 arity 选 clause β-归约成 let**（类型生成器只认 let 形式的值流，不认 case-lambda 应用；包括经 let/letrec 绑定名的间接应用——Chez 会把 clause lambda 提升穿越这些 let，所以维护了一个 prelex→case-lambda 的 env 表） |
| `chez-ir-rebuild-budget` | parameter 形式的节点预算，重建时**边建边数**，超限立即 raise——防那个展开成 350 万节点的 match-index-node 调用点把堆撑爆（事后数节点已经涨到 GB 级） |
| `chez-ir-call-site-type-flows` | 差分测试 oracle：从 letrec 绑定和 case-lambda 应用中提取 `((binding-bias . flow))`，flow 是 `(ref . bias)` / `(chain op . flow)` 的组合，cxr 访问器链被分解（cadr → car∘cdr）。已不在生产路径 |

## 3. 类型侧的同源入口

类型侧不走 identifier router 的 ufo-match 臂，而是
`analysis/type/substitutions/self-defined-rules/router.sls` 的 **generic
macro arm**：head 的 root-ancestor 类型为 `syntax-variable` 的调用点一律挂上
`(macro-infer root-library-node document index-node)`，即
`type:expand-and-infer`（macro-expansion.sls），内部调
`ir-auto-route:expand-call-site`：同一份 forms/env/展开，但第 ⑦ 步换成
`chez-ir->annotated-sexp` 重建树，让真实生成器在虚拟文档上跑，
结果按源位置映射回调用点。

`expand-call-site` 的 negative cache 顺序有讲究：
**先查负缓存 → head refs → find-define（最便宜的确定性拒绝）→ env 引导 →
展开**，因为绝大多数调用点根本扩不了（vendored 库里 define-syntax 内部的
几千个宏调用），把便宜的拒绝放前面。

## 4. 一眼例子

```scheme
(define (f xs) (match xs [(a b) (g a b)]))
```

1. `route&add` 认出 head `match` ∈ `(ufo-match)` → 挂上 try-expand rule；
2. step 到该调用点 → `private:try`；
3. `import-source` 找到 workspace 里 ufo-match 的 `.sls` → `load` +
   `build-env` `(chezscheme) (ufo-match) <本文档 imports>` → probe 通过；
4. `find-define-containing` 定位到 `(define (f xs) ...)`；
5. `expand-fresh`：第一次报 unbound `g` → 包 `(lambda (g) (let () form #f))`
   → 成功；
6. IR 里 `a`、`b` 的 prelex 绑定 source 指向 pattern 位置（在本文档
   path 内），uses `(g a b)` 的 ref source 也在；
7. `synthesize!`：anchor = pattern 位置 index-node，scope = clause 子节点
   → `a`、`b` 成为 speculative 引用，跳转定义、补全、unused 检查从此可见。

## 5. 已识别的优化候选（未实施）

1. ~~`chez-ir-call-site-refs` / `call-site-bindings` / `type-flows` 各自
   独立 walk 一遍 IR，三次遍历可合并~~（已做 2026-10-28：refs+bindings
   合并为单次遍历的 `chez-ir-call-site-facts`，旧接口保留为投影；
   type-flows 为测试 oracle 不动）；
2. ~~`private:find-source` 里手工实现的字符串函数~~（已清理 2026-10-28：
   死代码 `private:string-contains?` / `private:token-after` 删除，
   `private:find-substring` 换 srfi :13 `string-contains`，
   `private:split-on` 内部改用 srfi `string-index`）；
3. `private:synthesize!` 里 `pick-index-node-from` 对
   `document-index-node-list` 线性扫描，调用点密集时累计成本可观；
4. `private:try` 等处的 `(car (index-node-children call-node))` 裸结构
   访问是否可用 `match-index-node` 模式匹配优化——待调研。
