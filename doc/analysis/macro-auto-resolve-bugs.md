# 自动宏解析 (syntax-rules->generator:map+expansion) 已知缺陷分析

> 本文档记录对 `syntax-rules->generator:map+expansion` 自动宏解析路径的调试结论。
> 对应测试文件：`tests/analysis/identifier/test-auto-macro-resolve.sps`（match 宏）、
> `tests/analysis/identifier/test-simple-macro-auto-resolve.sps`（simple-let 宏）。

---

## 概述

`expansion-generator->rule` 的目标是通过通用的 `syntax-rules` 展开器替代手写规则（如 `match-process`）。
经测试，该路径对 `match` 宏无法恢复引用（返回空 exports），对最简单的无 ellipses 宏 `simple-let` 也无法恢复局部变量引用。
根本原因不是单一 bug，而是 **三个相互叠加的结构性缺陷**。

---

## Bug 1: `ellipse-*-form` 的 `loop` 未跳过 `ellipse` 伪子节点

**位置**: `analysis/identifier/expanders/pattern.sls:151-154`

**状态**: ✅ 已修复（commit `ecd0863`）

### 问题

`make-pattern` 在解析 ellipsed template（如 `(pat . body) ...` 或 `(match-next v g+s (pat . body) ...)`）时，
会把 `...` 本身也作为一个 `ellipse` 类型的 child 放入 `children` 列表。

例如 `ellipse-pair-form` 的 `children` 实际上是 `(pat body ellipse equal?-datum)`，
其中 `equal?-datum` 的 `content` 是 `'()`。

`expand->index-node-compound-list` 的 `loop` 在遍历 `children` 时，
把 `ellipse` 当作普通 child 处理，调用 `expand->index-node-compound-list` 返回 `'()`，
导致生成的列表中混入多余的 `'()`。

### 以 `match` 宏为例的完整推演

#### 1. `ufo-match` 中 `match` 宏的定义

```scheme
(define-syntax match
  (syntax-rules ()
    ((match atom (pat . body) ...)
     (let ((v atom))
       (match-next v (atom (set! atom)) (pat . body) ...)))
    ...))
```

`match` 的 clause `(_ atom (pat . body) ...)` 中：
- `atom` 是普通 pattern variable（非 ellipsed）
- `(pat . body) ...` 是 ellipsed pair form，匹配调用中的每一个 clause

> 注：Bug 1 处理的不是 pattern 左侧的 `(pat . body) ...`，而是 **template 右侧**
> `(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))` 中的 `(pat . body) ...`。
> 这个 ellipsed pair form 正是来自 `ufo-match.chezscheme.sls:124` 的展开式。

#### 2. `syntax-rules->generator:map+expansion` 的处理流程

当处理调用 `(match expression ((_ (? string? path)) ...) (else '()))` 时：

1. `private:confirm-clause` 匹配 clause `(_ atom (pat . body) ...)`
2. 返回 `(0 . (let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...)))`
3. `expansion-expression` = `(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))`
4. `make-pattern` 把它解析成嵌套的 pattern tree：
   - 顶层：`list-form`，children = `(((v atom)) match-next-call)`
   - `match-next-call`：`list-form`，children = `(match-next v (atom (set! atom)) ellipse-pair-form)`
   - `ellipse-pair-form`：children = `(pat body ellipse equal?-datum)`

#### 3. `expand->index-node-compound-list` 的作用与理想输出

**函数签名**：`(expand->index-node-compound-list template-pattern bindings pattern-context)`

**作用**：把 template pattern 中的 pattern variable 替换为调用中实际匹配的 index-node，
生成一棵和展开后 AST **结构同构**的 "compound list"。

compound list 是一个混合结构：
- 遇到非 pattern variable 的 symbol/number → 保持原样
- 遇到 pattern variable → 替换为对应的 index-node（来自 bindings）
- 遇到 `list-form`/`pair-form` → 递归处理 children，保持 list/pair 结构
- 遇到 `ellipse-*-form` → 按 ellipses 次数复制子结构

**理想输出示例**（以 `simple-let` 为例）：

Template: `(let ([var val]) body)`
调用: `(simple-let ((x 1)) x)`

理想 compound list 应该是：
```scheme
(let ((x-index-node 1)) x-index-node)
```

这样 `private:expansion+index-node->pairs` 可以把展开后 AST 中的每个节点
和 compound list 中的对应节点一一配对，最终 `private:shallow-copy` 就能把
展开后产生的引用（如 `let` body 中 `x` 的 export）复制回原始调用位置。

#### 4. `expand->index-node-compound-list` 进入 `ellipse-pair-form`

`template-pattern` 是整棵 pattern tree。`expand->index-node-compound-list` 从根递归处理：
- 遇到 `let` → 处理 `list-form`
- 遇到 `match-next` → 处理 `list-form`
- 遇到 `(pat . body) ...` → **进入 `ellipse-pair-form` 分支**

此时 bindings 已包含：
- `atom` → `expression` 的 index-node
- `pat` → `(_ (? string? path))` 的 index-node
- `body` → `(...)` 的 index-node

（注：`pat` 和 `body` 各有 **两个** 匹配值，因为调用有两个 clauses，
但 `generate-binding` 把它们打包成了一个结构，此处简化为 `pat1`/`pat2`、`body1`/`body2`）

#### 5. `loop` 实际生成的 `a`（修复前）

| clause | `pat` 匹配值 | `body` 匹配值 |
|--------|-------------|--------------|
| 1 | `(_ (? string? path))` | `(...)` |
| 2 | `else` | `'()` |

`expand->index-node-compound-list` 的 `loop` 遍历 `children = (pat body ellipse equal?-datum)`：

- `i=0`（`pat`）：走 `else` 分支，生成 **一次** `pat-compound`
- `i=1`（`body`）：走 ellipsed 分支，生成 `body1` `body2`
- `i=2`（`ellipse`）：走 `else` 分支，生成 `'()`
- `i=3`（`equal?-datum`）：走 `else` 分支，生成 `'()`

最终 `a` = `(pat-index-node body-index-node-1 body-index-node-2 () ())`

**问题**：`loop` 没有跳过 `ellipse` 和 `equal?-datum` 这两个伪子节点，
导致列表末尾混入两个多余的 `'()`。

#### 转换函数的错误输出（修复前）

对 `a = (pat-index-node body-index-node-1 body-index-node-2 () ())`：

```scheme
(reverse a)                      => (() () body2 body1 pat)
(cdr (reverse a))                => (() body2 body1 pat)
(reverse (cdr (reverse a)))      => (pat body1 body2 ())
(car (reverse a))                => ()
```

结果：`((pat body1 body2 ()) . ())` = `((pat body1 body2 ()))`

- 这是一个 **嵌套 list**，不是 pair list
- `compound-list->printable-list` 展开后变成包含多余 `'()` 的结构
- 调试日志中能看到 `callee-compound` 比 AST children **多了两个 `'()`**

#### 期望的正确输出

如果 `loop` 正确跳过 `ellipse` 和 `equal?-datum`，生成 `(pat1 body1 pat2 body2)`，
转换函数应该返回：

```scheme
((pat1 . body1) (pat2 . body2))
```

这才是 `(match-next v g+s (pat . body) ...)` 展开后应有的结构：
两个 `(pat . body)` pair。

### 修复

在 `expand->index-node-compound-list` 的 `ellipse-*-form` 共用 `loop` 中，
`else` 分支添加了对 `ellipse` child 的跳过逻辑：

```scheme
[else 
  (if (equal? 'ellipse (pattern-type (vector-ref children-vec i)))
    (loop (+ 1 i))
    `(,(expand->index-node-compound-list (vector-ref children-vec i) bindings pattern-context) 
      . 
      ,(loop (+ 1 i))))]
```

这样 `loop` 遇到 `ellipse` 时直接跳过，不再生成多余的 `'()`。

**效果**：`ellipse-pair-form` 和 `ellipse-vector-form` 的展开结构现在正确。
`ellipse-list-form` 不受影响（其 `ellipse` child 原本就在末尾，不会被单独访问）。

### 后果

- 修复前：`match` 宏展开后 `compound-list` 里混入多余的 `'()`，可能导致与 AST children 长度不匹配
- 修复后：`ellipse-*-form` 展开结构正确，不再产生多余的 `'()`

---

## Bug 2: `private:expansion+index-node->pairs` 无法处理长度不匹配

**位置**: `analysis/identifier/expanders/syntax-rules.sls:71-88`

**状态**: ⚠️ 代码层面未修复，但当前测试路径不再触发

### 问题

`expansion-index-node` 是 **未展开** 的原始 template AST。
例如 `(match-next v g+s (pat . body) ...)` 的 `children` 永远是 4 个：
`[match-next v g+s ellipse-pair-form]`。

但 `compound-list`（展开后）可能有 `4 + N` 个元素（N 个 clause）。
`map` 要求两列表等长，如果长度不匹配会直接抛出：

```
Exception in map: lists ... differ in length
```

### 当前代码

```scheme
(define (private:expansion+index-node->pairs compound-list index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)])
    (cond 
      [(index-node? compound-list) `((,index-node . ,compound-list))]
      [(list? compound-list) 
        (apply append 
          (map 
            (lambda (left right) 
            (private:expansion+index-node->pairs left right))
            compound-list
            children))]
      [(vector? compound-list) 
        (private:expansion+index-node->pairs (vector->list compound-list) index-node)]
      [(pair? compound-list) 
        (private:expansion+index-node->pairs `(,(car compound-list) ,(cdr compound-list)) index-node)]
      [else '()])))
```

代码中**没有**长度检查或截断逻辑。`list?` 分支直接使用 `map` 遍历 `compound-list` 和 `children`，
如果两者长度不同就会 crash。

### 为什么当前没有触发

1. **Bug 1 修复后**：`ellipse-pair-form` 不再产生多余的 `'()`，消除了最主要的长度不匹配来源
2. **`tree-has?` 守卫**：`syntax-rules->generator:map+expansion` 在入口处检查调用表达式是否包含 `...`，
   如果包含则直接返回 `#f`。这导致 `match` 宏的自动解析在展开前就被拒绝，不会走到 `private:expansion+index-node->pairs`

### 后果

- 潜在风险：任何使 `compound-list` 与 `children` 长度不同的展开场景都可能触发 crash
- 当前测试：由于上述原因，`match` 和 `simple-let` 的测试均不再触发此问题
- 若未来需要支持多 clause ellipses 宏的自动解析，此问题仍需解决

---

## Bug 3: Symbol 不生成 pair（最深层的缺陷）

**位置**: `analysis/identifier/expanders/syntax-rules.sls:87-88`

**状态**: ✅ 已修复

```scheme
; symbol won't get pairs
[else '()]
```

### 问题

`private:expansion+index-node->pairs` 对 atom（symbol、number 等）直接返回 `'()`，
意味着展开后 AST 中的 **任何 symbol 都不会被映射回原始调用位置**。

**修复**: 在 `private:expansion+index-node->pairs` 中添加 symbol 分支：
```scheme
[(and (symbol? compound-list) (symbol? expression))
  `((,index-node . ,compound-list))]
```
同时修复了 `private:recursive-collect` 不递归子节点的问题，以及
`private:shallow-copy` 中 `compound-export-list` 为 `index-node` 时 `filter` 崩溃的问题。
此外，`route&add` 现在会为具有 `syntax-expander` 的自定义宏自动创建展开规则，
使自动宏解析在 `init-workspace` 阶段就能触发。

以 `simple-let` 为例：

- Template: `(let ([var val]) body)`
- 展开后: `(let ((x 1)) x)`
- `private:expansion+index-node->pairs` 生成的 pairs 只有：
  - `(let-node . let-node)`
  - `(((x 1))-node . ((x 1))-node)`
  - **没有 `x` 的 pair**

`private:shallow-copy` 遍历 `expansion-index-node` 时，确实能找到 `x` 的
`references-export-to-other-node`，但在 `pairs` 里 `assoc` 不到对应位置，
引用复制链断裂。

### 后果

即使修复 Bug 1 和 Bug 2，让无 ellipses 的宏展开成功，**局部变量引用依然无法回传**。
测试验证：`simple-let` 展开后 `x` 的 exports 现在正确包含 `x` 的引用。

---

## 修复难度评估

| 缺陷 | 复杂度 | 状态 | 说明 |
|------|--------|------|------|
| Bug 1 `ellipse-*-form` loop | 中 | ✅ 已修复 | loop 现在会跳过 `ellipse` 伪子节点 |
| Bug 2 长度不匹配 | 高 | ⚠️ 潜在风险 | 代码层面未加防护，但当前测试路径不触发。需让配对器理解 ellipses 语义，或改用「展开后 AST ↔ 展开后 AST」配对 |
| Bug 3 Symbol 不生成 pair | 高 | ✅ 已修复 | symbol 分支已添加；`recursive-collect` 和 `shallow-copy` 的 index-node 处理也已修复 |

## 结论

` syntax-rules->generator:map+expansion` 对 ellipses 的支持仍是**结构性缺失**（Bug 2），
但 symbol 引用的回传已通过单点修复解决。

### 自动解析 vs 手写解析的效果对比

| 维度 | 自动解析 (`expansion-generator->rule`) | 手写解析 (`match-process` 等) |
|------|----------------------------------------|------------------------------|
| **ellipses 支持** | ❌ 直接拒绝（返回 `#f`） | ✅ 完全支持 |
| **复杂 pattern** | ❌ 展开后可能丢失语义 | ✅ 精确处理 |
| **变量绑定** | ⚠️ 依赖 `shallow-copy`，仅浅层复制 | ✅ 精确绑定到 call site |
| **性能** | 🐢 需要展开+重新解析+遍历 | ⚡ 直接分析 AST |
| **适用场景** | 简单、无 ellipses 的宏（如 `simple-let`） | 复杂宏（如 `match`、`try`） |

**`match` 不适合自动解析的原因：**
1. `match` 模板包含 `...`，`syntax-rules->generator:map+expansion` 检测到 ellipses 直接拒绝展开
2. `ufo-match` 内部有 `match-one`、`match-drop-ids` 等几十个辅助宏，自动解析会尝试为每个都创建规则，导致性能爆炸
3. `shallow-copy` 是浅层复制，无法处理 `match` 的复杂嵌套 pattern（如 `quasiquote`、`and`/`or` pattern）

### 当前部署状态

- **Bug 3 核心修复**（`syntax-rules.sls` 的 symbol 分支、`expansion-wrap.sls` 的 `recursive-collect` 修复）已合入代码库
- **`router.sls` 的自动规则**目前被**注释掉**，未正式启用。原因：会为 `.akku/lib/` 中的大量外部库宏创建规则，导致 `init-workspace` 性能急剧下降
- **`test-simple-macro-auto-resolve.sps`** 通过手动调用 `rule` 来验证自动解析功能，不依赖 `router.sls` 的自动触发
- **手写规则**（`match-process`、`let1-process`、`try-process` 等）仍然是处理复杂宏的主要方式
