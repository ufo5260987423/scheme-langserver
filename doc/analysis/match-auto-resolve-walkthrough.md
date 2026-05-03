# match 宏自动解析完整链路 walkthrough

> 本文档追踪 `ufo-match` 的 `match` 宏从被调用到 `auto-resolve` 处理完毕的完整调用链。
> 以 `analysis/identifier/rules/load.sls` 中的调用为例：
> ```scheme
> (match expression
>   [(_ (? string? path)) (let (...) ...)])
> ```

---

## 0. 前置：宏定义与调用

### 0.1 match 宏定义（`ufo-match`）

```scheme
(define-syntax match
  (syntax-rules ()
    ((match atom (pat . body) ...)
     (let ((v atom))
       (match-next v (atom (set! atom)) (pat . body) ...)))
    ...))
```

### 0.2 调用处的 AST（局部）

```scheme
(match expression [(_ (? string? path)) (let (...) ...)])
```

- `match-call-node`：整个 `(match ...)` 的 `index-node`
- `match-reference`：`match` 的 `identifier-reference`
- `syntax-expander`：`identifier-reference-syntax-expander match-reference`
  - 实际值是 `syntax-rules->generator:map+expansion` 返回的 lambda

---

## 1. `expansion-generator->rule`（入口）

**位置**：`protocol/apis/expansion-wrap.sls:10`

**输入**：
- `proc`：`syntax-rules->generator:map+expansion` 生成的 lambda
- `step`：abstract-interpreter 的主循环函数
- `file-linkage`：当前 workspace 的 file-linkage
- `expanded+callee-list`：空列表 `'()`
- `memory`：空列表 `'()`

**输出**：一个 `rule` lambda

```scheme
(lambda (root-file-node root-library-node document index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [pairs+expansion (proc root-file-node root-library-node document index-node)])
    (if pairs+expansion
      (let* ([pairs (car pairs+expansion)]
          [expansion-index-node (cdr pairs+expansion)]
          [possible-new-memory `(,expression . ,memory)])
        (if (not (contain? memory expression))
          (step root-file-node root-library-node file-linkage
>                 document expansion-index-node expanded+callee-list possible-new-memory))
        (private:shallow-copy pairs expansion-index-node document index-node))
      '())))
```

**工作**：
1. 调用 `proc` 生成 `pairs+expansion`（`(matching-pairs . expansion-index-node)`）
2. 如果 `pairs+expansion` 非 `#f`：
   - 用 `step` 跑一遍展开后的 AST（`expansion-index-node`），让 abstract interpreter 建立引用
   - 用 `private:shallow-copy` 把展开 AST 里的引用复制回原始调用
3. 如果 `#f`（例如 `tree-has?` 发现 `...`），返回 `'()`

---

## 2. `proc` = `syntax-rules->generator:map+expansion`

**位置**：`analysis/identifier/expanders/syntax-rules.sls:17`

**输入**：
- `root-file-node`
- `root-library-node`
- `document`
- `local-index-node`：`match` 调用的 `index-node`

**输出**：`(matching-pairs . expansion-index-node)` 或 `#f`

### 2.1 提取 clause

```scheme
(match (annotation-stripped (index-node-datum/annotations input-index-node))
  [(_ (literals ...) clauses **1) ...])
```

- `input-index-node`：`match` 宏定义处的 `(syntax-rules () ...)` 的 `index-node`
- `literals` = `()`
- `clause-index-nodes`：所有 clause 的 `index-node` 列表
- `local-expression`：`(match expression [(_ (? string? path)) ...])`

### 2.2 `private:confirm-clause`

**位置**：`syntax-rules.sls:90`

**输入**：`literals`、`clause-index-nodes`、`local-expression`

**工作**：用 `eval` + `syntax-case` 把 `local-expression` 逐个 clause 匹配

**输出**：`(0 . expansion-expression)`

```scheme
(let ((v expression))
  (match-next v
    (expression (set! expression))
    ((_ (? string? path)) (let (...) ...))
    ...))
```

这里 `syntax-case` 已经替换了 pattern variables：
- `atom` → `expression`
- `pat` → `(_ (? string? path))`
- `body` → `(let (...) ...)`

### 2.3 构造 pattern 和 template

```scheme
[clause-expression (annotation-stripped (index-node-datum/annotations clause-index-node))]
[pattern-expression (car clause-expression)]
  => '(match atom (pat . body) ...)
[template-expression (car (reverse clause-expression))]
  => '(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))

[pattern (make-pattern pattern-expression)]
[template-pattern (make-pattern template-expression)]
[pattern-context (gather-context pattern)]
```

`pattern-context` = `((atom . <pattern>) (pat . <pattern>) (body . <pattern>))`

### 2.4 `pattern+index-node->pair-list`

**位置**：`pattern.sls:340`

**输入**：`pattern`、`local-index-node`（match 调用的 index-node）

**输出**：`((pattern-node . index-node) ...)`

把 pattern tree 的每个叶子和调用 AST 的对应节点一一配对。

### 2.5 `generate-binding`

**位置**：`pattern.sls:187`

**输入**：`literal`（`'pat'`）、`iterator`

**输出**：`(pat . (pat1-index-node pat2-index-node))`

把 ellipsed pattern variable 的多个匹配值打包成一个 list。

对所有 exposed literals（`atom`、`pat`、`body`）执行后得到：

```scheme
bindings = ((atom . expression-index-node)
            (pat . (pat1 pat2))
            (body . (body1 body2)))
```

### 2.6 `expand->index-node-compound-list`

**位置**：`pattern.sls:108`

**输入**：`template-pattern`、`bindings`、`pattern-context`

**工作**：递归遍历 template pattern tree，把 pattern variable 替换为 bindings 中的 index-node。

**输出**：`callee-compound-index-node-list`（compound list）

#### 展开过程详解

`template-pattern` 的结构（`make-pattern` 生成）：

```
list-form: (let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))
  ├── list-form: ((v atom))
  │     └── pair-form: (v . atom)
  └── list-form: (match-next v (atom (set! atom)) (pat . body) ...)
        ├── equal?-datum: match-next
        ├── pattern-variable/literal-identifier: v
        ├── list-form: (atom (set! atom))
        └── ellipse-list-form: ((pat . body) ...)
              ├── pair-form: (pat . body)
              └── ellipse: ...
```

递归展开：

1. **顶层 `list-form`**：递归处理 children
   - 结果：`(((v-compound atom-compound)) match-next-compound-result)`

2. **`((v atom))`** → `pair-form`：
   - `v` → `v-compound`（来自 `let` 的 binding，不是 pattern variable）
   - `atom` → `expression-index-node`
   - 结果：`(v-compound . expression-index-node)`

3. **`(match-next v (atom (set! atom)) (pat . body) ...)`** → `list-form`：
   - `match-next` → `match-next`
   - `v` → `v-compound`
   - `(atom (set! atom))` → `(expression-index-node (set! expression-index-node))`
   - `(pat . body) ...` → **进入 `ellipse-list-form` 分支**

4. **`ellipse-list-form`**：
   - `children` = `(pair-form ellipse)`
   - `loop i=0`：`pair-form` 不是 ellipse，下一个 `ellipse` 是 ellipse
   - `new-bindings-list` = `(((pat . pat1) (body . body1)) ((pat . pat2) (body . body2)))`
   - 对每个 binding 展开 `pair-form`：
     - binding1：`pat` → `pat1`，`body` → `body1` → `(pat1 . body1)`
     - binding2：`pat` → `pat2`，`body` → `body2` → `(pat2 . body2)`
   - `loop` 结果 = `((pat1 . body1) (pat2 . body2))`
   - 转换函数 `(lambda (a) a)` 返回 `((pat1 . body1) (pat2 . body2))`

**最终 compound list**：

```scheme
(let-compound
  (((v-compound expression-index-node)))
  (match-next v-compound
    (expression-index-node (set! expression-index-node))
    ((pat1 . body1) (pat2 . body2))))
```

> **注意**：这里 `compound-list` 把两个 `(pat . body)` pair flat 地放在一个 list 里，
> 作为 `match-next` 的第四个参数。

### 2.7 `source-file->annotations` → `expansion-index-node`

**位置**：`syntax-rules.sls:43`

**输入**：`expansion-expression`（字符串形式）

**工作**：重新解析展开后的表达式，生成新的 AST

**输出**：`expansion-index-node`

```scheme
(let ((v expression))
  (match-next v
    (expression (set! expression))
    ((_ (? string? path)) (let (...) ...))
    ...))
```

`expansion-index-node` 的结构：
- `match-next` 的 children = `(match-next v (expression (set! expression)) ((_ (? string? path)) ...) ...)`
  - 注意：`syntax-case` 已经展开了 ellipses，所以这里有两个 `(pat . body)` pair
  - children 长度 = 4

### 2.8 `private:expansion+index-node->pairs`

**位置**：`syntax-rules.sls:71`

**输入**：`callee-compound-index-node-list`、`expansion-index-node`

**工作**：把 compound list 和 expansion-index-node 的 AST 节点一一配对

**输出**：`matching-pairs`

#### 配对过程

1. 顶层 `let`：
   - `compound-list` = `(let-compound (((v-compound expression-index-node))) match-next-compound-result)`
   - `expansion-index-node` = `let`
   - `children` = `(((v expression))) (match-next v ...)`
   - `map` 比较：
     - `let-compound` ↔ `let`（symbol vs symbol → `'()`）
     - `(((v-compound expression-index-node)))` ↔ `(((v expression)))`
     - `match-next-compound-result` ↔ `(match-next v ...)`

2. `match-next`：
   - `compound-list` = `(match-next v-compound (expression-index-node (set! expression-index-node)) ((pat1 . body1) (pat2 . body2)))`
   - `expansion-index-node` = `match-next`
   - `children` = `(v (expression (set! expression)) ((_ (? string? path)) ...) ...)`
   - `map` 比较 4 个元素：
     - `match-next` ↔ `match-next`（symbol → `'()`）
     - `v-compound` ↔ `v`（index-node → `((v-node . v-compound))`）
     - `(expression-index-node (set! expression-index-node))` ↔ `(expression (set! expression))`
     - `((pat1 . body1) (pat2 . body2))` ↔ `((_ (? string? path)) ...) ...`

3. **第四个元素配对**（`((pat1 . body1) (pat2 . body2))` vs `((_ (? string? path)) ...) ...`）：
   - `compound-list` = `((pat1 . body1) (pat2 . body2))`，长度 = 2
   - `expansion-index-node` = `((_ (? string? path)) ...) ...`
   - `children` = `(pair-form ellipse)`，长度 = 2
   - `map` 比较 2 个元素：
     - `(pat1 . body1)` ↔ `pair-form`
     - `(pat2 . body2)` ↔ `ellipse`

4. **`(pat2 . body2)` vs `ellipse`**：
   - `compound-list` = `(pat2 . body2)`，是 pair
   - `index-node` = `ellipse`
   - `expression` = `...`
   - `children` = `'()`
   - `private:expansion+index-node->pairs` 处理 `(pat2 . body2)`：
     - `pair?` = `#t`
     - 展开为 `(pat2 body2)`
     - `children` = `'()`
     - `map` 比较 2 个元素和 0 个元素

---

## 🐛 Bug 2 发生点

**位置**：`syntax-rules.sls:76-82`

```scheme
[(list? compound-list)
  (apply append
    (map
      (lambda (left right)
        (private:expansion+index-node->pairs left right))
      compound-list
      children))]
```

当 `compound-list` = `(pat2 body2)`（长度 2）与 `children` = `'()`（长度 0）比较时，
`map` 因长度不匹配直接抛出：

```
Exception in map: lists ... differ in length
```

**触发路径**：
1. `expand->index-node-compound-list` 对 `ellipse-list-form` 生成了 `N` 个展开后的 pair
2. `list-form` 的 `map` 把这 `N` 个 pair 整体作为 `match-next` 的一个参数
3. `private:expansion+index-node->pairs` 比较这 `N` 个 pair 和 `expansion-index-node` 中的 `ellipse` 节点
4. 第二个及以后的 pair 与 `ellipse` 节点配对时，`ellipse` 的 children = `'()`，导致长度不匹配 crash

---

## 🐛 Bug 1 发生点

**位置**：`pattern.sls:151-154`（`expand->index-node-compound-list` 的 `ellipse-*-form` loop）

```scheme
[else 
  (if (equal? 'ellipse (pattern-type (vector-ref children-vec i)))
    (loop (+ 1 i))
    `(,(expand->index-node-compound-list (vector-ref children-vec i) bindings pattern-context) 
      . ,(loop (+ 1 i))))]
```

**问题**：`make-pattern` 会把 `...` 本身也作为一个 `ellipse` 类型的 child 放入 `children`。
`loop` 的 `else` 分支没有跳过这些伪子节点，把它们当成普通 child 处理，生成 `'()`。

具体到这个例子（`ellipse-pair-form`）：
- `children` = `(pat body ellipse equal?-datum)`
- `loop` 展开 `pat` → `pat-compound`
- `loop` 展开 `body`（ellipsed）→ `(body1 body2)`
- `loop` 处理 `ellipse` → `'()`（错误！这是伪子节点）
- `loop` 处理 `equal?-datum` → `'()`（错误！这是伪子节点）
- 结果 = `(pat-compound body1 body2 () ())`

**后果**：多余的 `'()` 被 `ellipse-pair-form` 的转换函数处理，产生错误的 pair 结构
（如 `((pat body1 body2 ()) . ())` 而不是 `((pat1 . body1) (pat2 . body2))`）。

**修复**：`else` 分支添加了对 `ellipse` child 的跳过逻辑，遇到 `ellipse` 直接 `loop (+ 1 i)`。

---

## 🐛 Bug 3 发生点

**位置**：`syntax-rules.sls:87-88`

```scheme
; symbol won't get pairs
[else '()]
```

当 `compound-list` 是 symbol（如 `'match-next`、`'let`）时，`private:expansion+index-node->pairs` 直接返回 `'()`。

这本身不是 bug，但结合 `private:shallow-copy` 的设计：

```scheme
(filter index-node? compound-export-list)
```

`private:shallow-copy` 只复制 `index-node?` 的节点。如果 `compound-list` 是 symbol，
`matching-pairs` 中虽然可能有 pair，但 `private:shallow-copy` 的 `filter` 会把 symbol 过滤掉。

**对 `simple-let` 宏的影响**：
- Template: `(let ([var val]) body)`
- `compound-list` = `(let ((x 1)) x)`
- `x` 在 compound list 中是 `index-node`，在 `expansion-index-node` 中也是 `index-node`
- `private:expansion+index-node->pairs` 对 `x` 返回 `((x-index-node . x-compound))`
- 但 `private:shallow-copy` 修改的是 `expansion-index-node` 中的 `x`，不是原始调用中的 `x`
- `step` 处理 `expansion-index-node` 时，更新 `document` 的 `ordered-reference-list`
- 原始调用中的 `var-node`（binding 中的 `x`）的 `references-export-to-other-node` 不会被更新

---

## 3. `step`（abstract interpreter）

**位置**：`analysis/abstract-interpreter.sls`

**输入**：`root-file-node`、`root-library-node`、`file-linkage`、`document`、`expansion-index-node`、`expanded+callee-list`、`memory`

**工作**：遍历 `expansion-index-node` 的 AST，解析 identifier references，建立 `ordered-reference-list`。

对于 `match` 宏的展开 AST：
- 处理 `let`，建立 `v` 的 binding
- 处理 `match-next`，发现它是一个宏调用，尝试展开
- `match-next` 的展开可能产生新的 `match` 调用，但 `memory` 包含原始调用，所以 `step` 会跳过

---

## 4. `private:shallow-copy`

**位置**：`protocol/apis/expansion-wrap.sls:24`

**输入**：`pairs`、`expansion-index-node`、`document`、`initialization-index-node`（原始调用）

**工作**：
1. 用 `private:recursive-collect` 收集 `expansion-index-node` 中所有 `references-export-to-other-node`
2. 对每个 identifier reference，在 `pairs` 中查找对应的 `compound-export-list`
3. 创建新的 `identifier-reference`，添加到 `compound-export-list` 的 `references-export-to-other-node`
4. 同时更新 `compound-import-list` 的 `ordered-reference-list`

**关键限制**：
- `filter index-node? compound-export-list`：只复制 `index-node?` 的节点，symbol 被过滤
- 修改的是 `expansion-index-node` 中的节点，不是原始调用中的节点
- 原始调用中的 `var-node` 的 `references-export-to-other-node` 不会被更新

---

## 5. 回到 `rule` 的返回值

```scheme
(private:shallow-copy pairs expansion-index-node document index-node)
```

返回值是 `private:shallow-copy` 的结果（一个嵌套的 map 结果），通常被丢弃。

但副作用是：
- `expansion-index-node` 中的节点被添加了引用
- `document` 的 `ordered-reference-list` 被更新

原始调用 `index-node`（`match-call-node`）的 `references-export-to-other-node` 没有被修改。

---

## 总结：三个 Bug 的触发条件

| Bug | 触发条件 | 影响 |
|-----|---------|------|
| Bug 1 | `ellipse-list-form` 展开后，结果结构不匹配 | `compound-list` 与 `expansion-index-node` 结构错位 |
| Bug 2 | `private:expansion+index-node->pairs` 比较展开后的 pair 与 `ellipse` 节点 | `map` 长度不匹配 crash |
| Bug 3 | `private:shallow-copy` 只修改 `expansion-index-node`，不修改原始调用 | 局部变量引用无法回传 |

**实际影响**：
- `match` 宏（含 ellipses）→ `tree-has?` 在入口处拒绝含 `...` 的调用，返回 `'()`。**`match` 不适合自动解析**：其模板包含 ellipses，内部还有 `match-one`、`match-drop-ids` 等几十个辅助宏，且 `shallow-copy` 的浅层复制无法处理复杂嵌套 pattern 的引用回传。`match-process` 等手写规则仍然是处理 `match` 的主要方式。
- `simple-let` 宏（无 ellipses）→ Bug 3 已修复 → `auto-resolve` 现在可以正确回传 `x` 的引用。

### 自动解析 vs 手写解析的适用边界

| 宏类型 | 自动解析 | 手写解析 | 说明 |
|--------|---------|---------|------|
| `simple-let`（无 ellipses、单层） | ✅ 可用 | 可用 | Bug 3 修复后，自动解析效果与手写一致 |
| `match`（含 ellipses、复杂嵌套） | ❌ 不可用 | ✅ 必需 | ellipses 被拒绝；辅助宏太多；浅层复制不够 |
| `try`、`let1`、`define-case-class` 等 | ❌ 不可用 | ✅ 必需 | 这些宏有专门的手写规则处理其特殊语义 |

### 当前部署状态

- **Bug 1、Bug 3 已修复**：`ellipse-*-form` 的伪子节点跳过、symbol 分支的 pair 生成、`recursive-collect` 的子节点递归均已合入
- **`router.sls` 的自动规则被注释掉**：未正式启用。若启用，会为 `.akku/lib/` 中大量外部库宏创建规则，导致 `init-workspace` 性能急剧下降
- **测试策略**：`test-simple-macro-auto-resolve.sps` 通过手动调用 `rule` 验证自动解析功能；`test-auto-macro-resolve.sps` 验证 `match` 自动解析会崩溃（预期行为）
