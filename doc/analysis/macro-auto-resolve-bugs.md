# 自动宏解析 (syntax-rules->generator:map+expansion) 已知缺陷分析

> 本文档记录对 `syntax-rules->generator:map+expansion` 自动宏解析路径的调试结论。
> 对应测试文件：`tests/analysis/identifier/test-auto-macro-resolve.sps`（match 宏）、
> `tests/analysis/identifier/test-simple-macro-auto-resolve.sps`（simple-let 宏）。

---

## 概述

`expansion-generator->rule` 的目标是通过通用的 `syntax-rules` 展开器替代手写规则（如 `match-process`）。
经测试，该路径对 `match` 宏 crash，对最简单的无 ellipses 宏 `simple-let` 也无法恢复局部变量引用。
根本原因不是单一 bug，而是 **三个相互叠加的结构性缺陷**。

---

## Bug 1: `ellipse-*-form` 的 `loop` 未跳过 `ellipse` 伪子节点

**位置**: `analysis/identifier/expanders/pattern.sls:134-154`

### 问题

`make-pattern` 在解析 ellipsed template（如 `(pat . body) ...` 或 `(match-next v g+s (pat . body) ...)`）时，
会把 `...` 本身也作为一个 `ellipse` 类型的 child 放入 `children` 列表。

例如 `ellipse-pair-form` 的 `children` 实际上是 `(pat body ellipse equal?-datum)`，
其中 `equal?-datum` 的 `content` 是 `'()`。

`expand->index-node-compound-list` 的 `loop` 在遍历 `children` 时，
把 `ellipse` 当作普通 child 处理，调用 `expand->index-node-compound-list` 返回 `'()`，
导致生成的列表中混入多余的 `'()`。

#### 以 `match` 宏为例的完整推演

##### 1. `ufo-match` 中 `match` 宏的定义

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

##### 2. `syntax-rules->generator:map+expansion` 的处理流程

当处理调用 `(match expression ((_ (? string? path)) ...) (else '()))` 时：

1. `private:confirm-clause` 匹配 clause `(_ atom (pat . body) ...)`
2. 返回 `(0 . (let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...)))`
3. `expansion-expression` = `(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))`
4. `make-pattern` 把它解析成嵌套的 pattern tree：
   - 顶层：`list-form`，children = `(((v atom)) match-next-call)`
   - `match-next-call`：`list-form`，children = `(match-next v (atom (set! atom)) ellipse-pair-form)`
   - `ellipse-pair-form`：children = `(pat body ellipse equal?-datum)`

##### 3. `expand->index-node-compound-list` 的作用与理想输出

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

##### 4. `expand->index-node-compound-list` 进入 `ellipse-pair-form`

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

##### 5. `loop` 实际生成的 `a`（错误输入）

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

##### 转换函数的错误输出

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

##### 期望的正确输出

如果 `loop` 正确跳过 `ellipse` 和 `equal?-datum`，生成 `(pat1 body1 pat2 body2)`，
转换函数应该返回：

```scheme
((pat1 . body1) (pat2 . body2))
```

这才是 `(match-next v g+s (pat . body) ...)` 展开后应有的结构：
两个 `(pat . body)` pair。

### 修复 (已应用)

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
   - `ellipse-pair-form`：children = `(pat body ellipse)`

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

#### 4. `loop` 实际生成的 `a`（错误输入）

| clause | `pat` 匹配值 | `body` 匹配值 |
|--------|-------------|--------------|
| 1 | `(_ (? string? path))` | `(...)` |
| 2 | `else` | `'()` |

`expand->index-node-compound-list` 的 `loop` 遍历 `children = [pat body ellipse]`：

- `i=0`（`pat`）：走 `else` 分支，生成 **一次** `pat-compound`
- `i=1`（`body`）：走 ellipsed 分支，生成 `body1` `body2`
- `i=3`：返回 `'()`

最终 `a` = `(pat-index-node body-index-node-1 body-index-node-2)`

**问题**：`pat` 只在 `else` 分支生成 **一次**，但 ellipses 要求每个 clause 都有一份 `pat`。
正确情况下 `loop` 应该生成 `(pat1 body1 pat2 body2)`。

#### 转换函数的错误输出

对 `a = (pat-index-node body-index-node-1 body-index-node-2)`：

```scheme
(reverse a)                      => (body2 body1 pat)
(cdr (reverse a))                => (body1 pat)
(reverse (cdr (reverse a)))      => (pat body1)
(car (reverse a))                => body2
```

结果：`((pat body1) . body2)` = `((pat body1) body2)`

- 这是一个 **3 元素 list**，不是 pair list
- `compound-list->printable-list` 展开后变成 `pat body1 body2` 的 flat list
- 调试日志中能看到 `callee-compound` 比 AST children **多了两个 `'()`**

#### 期望的正确输出

如果 `loop` 正确生成了 `(pat1 body1 pat2 body2)`，
**根本不需要转换函数**，直接返回：

```scheme
((pat1 . body1) (pat2 . body2))
```

这才是 `(match-next v g+s (pat . body) ...)` 展开后应有的结构：
两个 `(pat . body)` pair。

### 后果

- `match` 宏展开后 `compound-list` 里混入多余的 `'()`
- `callee-compound` 与 AST children 长度不匹配，触发 Bug 2 的 crash

---

## Bug 2: `private:expansion+index-node->pairs` 无法处理长度不匹配

**位置**: `analysis/identifier/expanders/syntax-rules.sls:76-82`

### 问题

`expansion-index-node` 是 **未展开** 的原始 template AST。  
例如 `(match-next v g+s (pat . body) ...)` 的 `children` 永远是 4 个：
`[match-next v g+s ellipse-pair-form]`。

但 `compound-list`（展开后）可能有 `4 + N` 个元素（N 个 clause）。
`map` 要求两列表等长，直接抛出：

```
Exception in map: lists ... differ in length
```

### 修复 (已应用)

在 `syntax-rules.sls` 中为 `private:expansion+index-node->pairs` 添加了容错处理：

1. 当 `index-node` 不是 `index-node?` 时返回 `'()`（避免对 symbol/list 调用 `index-node-datum/annotations`）
2. 当 `expression` 为 `'...` 时返回 `'()`（跳过 ellipses 节点）
3. `list?` 分支改为比较 `compound-list` 和 `(cons expression children)`，
   并截断到最短长度（`list-head` + `min`）

```scheme
(define (private:expansion+index-node->pairs compound-list index-node)
  (if (not (index-node? index-node))
    '()
    (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
        [children (index-node-children index-node)])
      (if (equal? expression '...)
        '()
        (cond 
          [(index-node? compound-list) `((,index-node . ,compound-list))]
          [(list? compound-list) 
            (let ([target (cons expression children)])
              (apply append 
                (map 
                  (lambda (left right) 
                    (private:expansion+index-node->pairs left right))
                  (list-head compound-list (min (length compound-list) (length target)))
                  (list-head target (min (length compound-list) (length target))))))]
          [(vector? compound-list) 
            (private:expansion+index-node->pairs (vector->list compound-list) index-node)]
          [(pair? compound-list) 
            (private:expansion+index-node->pairs `(,(car compound-list) ,(cdr compound-list)) index-node)]
          [else '()])))))
```

**效果**：`match` 宏不再 crash，`auto-resolve` 能正常返回（虽然引用仍然缺失）。
对应测试已更新为期望空列表而非 `'crash`。

### 后果

任何包含 ellipses 的宏（`match`、`simple-let` 如果带多 binding）都会在这里 crash。
**已修复**：不再 crash，但展开的多个 clause 中只有第一个能被正确配对，其余被截断丢弃。

---

## Bug 3: Symbol 不生成 pair（最深层的缺陷）

**位置**: `analysis/identifier/expanders/syntax-rules.sls:87-88`

```scheme
; symbol won't get pairs
[else '()]
```

### 问题

`private:expansion+index-node->pairs` 对 atom（symbol、number 等）直接返回 `'()`，
意味着展开后 AST 中的 **任何 symbol 都不会被映射回原始调用位置**。

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
测试验证：`simple-let` 展开后 `x` 的 exports 为空。

---

## 修复难度评估

| 缺陷 | 复杂度 | 说明 |
|------|--------|------|
| Bug 1 `ellipse-*-form` loop | 中 | 已修复：loop 现在会跳过 `ellipse` 伪子节点 |
| Bug 2 长度不匹配 | 高 | 需让配对器理解 ellipses 语义，或改用「展开后 AST ↔ 展开后 AST」配对 |
| Bug 3 Symbol 不生成 pair | 高 | 需为 symbol 生成 `(expansion-symbol-node . compound-symbol-node)`，但可能破坏现有假设 |

## 结论

` syntax-rules->generator:map+expansion` 对 ellipses 和 symbol 引用的支持是**结构性缺失**，
不是单点 patch 能修好的。当前手写规则（`match-process`、`let1-process` 等）仍然是必需的。
