# match 宏展开过程详解

## 概述

`match` 是 `ufo-match` 库提供的模式匹配宏。它的核心任务是把声明式的 pattern → body 语法，展开成过程式的 `if`/`let` 嵌套代码，同时把 pattern 中的变量（如 `s`、`a`、`b`）转换成 `let` 绑定。

本文以两个具体例子走通完整展开链路，说明各辅助宏的协作方式。

---

## 例子 1：单变量绑定 `(match '(1) [(s) s])`

### 第 1 步：match（入口）

```scheme
(match '(1) [(s) s])
```

`match` 用 `syntax-rules` 定义，对非列表/非向量的 `atom` 输入，走这条 clause：

```scheme
((match atom (pat . body) ...)
 (let ((v atom))
   (match-next v (atom (set! atom)) (pat . body) ...)))
```

展开结果：

```scheme
(let ((v '(1)))
  (match-next v ('(1) (set! '(1))) ((s) s)))
```

> `v` 是临时变量，保存 match 表达式的值。`g+s`（getter+setter）是 `(atom (set! atom))`，用于支持 `get!`/`set!` 模式。

---

### 第 2 步：match-next（包装 failure continuation）

`match-next` 给每个 clause 加上匿名 failure continuation（命名为 `failure`）：

```scheme
(let ((v '(1)))
  (let ((failure (lambda () (error 'match "no matching pattern"))))
    (match-one v (s) ('(1) (set! '(1)))
               (match-drop-ids (begin s))
               (failure)
               ())))
```

> `match-drop-ids (begin s)` 是 success continuation（sk）。`match-drop-ids` 的作用是忽略末尾累积的 identifiers 列表，只保留 `begin` 体。

---

### 第 3 步：match-one（分发到 match-two）

`match-one` 先检查 ellipsis（`...`）。`(s)` 只有一个元素，不是 ellipsis，直接透传：

```scheme
(let ((v '(1)))
  (let ((failure ...))
    (if (and (pair? v) (null? (cdr v)))
        (let ((w (car v)))        ; w = 1
          (match-one w s ((car v) (set-car! v))
                     (match-drop-ids (begin s))
                     (failure)
                     ()))
        (failure))))
```

> `(p)` clause 在 `match-two` 中处理：匹配单元素列表。`w` 是 `car`，然后递归匹配 `w` 和 pattern `s`。

---

### 第 4 步：match-two（核心：symbol pattern）

`match-one w s ...` 透传到 `match-two w s ...`。`s` 是裸 symbol，走这条 clause：

```scheme
((match-two v x g+s (sk ...) fk (id ...))
 (match-check-identifier
  x
  (let-syntax
      ((new-sym?
        (syntax-rules (id ...)
          ((new-sym? x sk2 fk2) sk2)
          ((new-sym? y sk2 fk2) fk2))))
    (new-sym? random-sym-to-match
              (let ((x v)) (sk ... (id ... x)))
              (if (equal? v x) (sk ... (id ...)) fk)))
  (if (equal? v x) (sk ... (id ...)) fk)))
```

这里有两个 hygiene 技巧：

#### 技巧 A：match-check-identifier 确认 x 是 symbol

```scheme
(define-syntax match-check-identifier
  (syntax-rules ()
    ((_ (x . y) success-k failure-k) failure-k)
    (_ #(x ...) success-k failure-k) failure-k)
    ((_ x success-k failure-k)
     (let-syntax
         ((sym?
           (syntax-rules ()
             ((sym? x sk fk) sk)      ; x 是 pattern variable，匹配任何 identifier
             ((sym? y sk fk) fk))))
       (sym? abracadabra success-k failure-k)))))
```

`s` 是 symbol（atom），所以 `sym? abracadabra` 匹配 `((sym? x sk fk) sk)`——因为 `x` 在 `syntax-rules ()` 中是 **pattern variable**，不比较名字，只匹配 shape。结果走 `success-k`。

> 如果 `x` 是列表或向量，`sym? abracadabra` 不会走（fast path 直接 `failure-k`），但 symbol 总是走 `success-k`。

#### 技巧 B：new-sym? 区分"新变量"vs"已绑定变量"

```scheme
(let-syntax
    ((new-sym?
      (syntax-rules (id ...)           ; id ... 是已累积的 bound identifiers
        ((new-sym? x sk2 fk2) sk2)     ; x 若不在 id ... 中，则是 pattern variable
        ((new-sym? y sk2 fk2) fk2))))
  (new-sym? random-sym-to-match
            (let ((s w)) (begin s))     ; sk2：创建 let 绑定
            (if (equal? w s) (begin s) (failure))))  ; fk2：比较 equal?
```

- `s` 第一次出现，不在 `id ...`（空列表）中。
- `new-sym?` 的 `syntax-rules (id ...)` 中，`s` **不是 literal**（因为不在 `id ...` 中）。
- 所以 `s` 在 `((new-sym? s sk2 fk2) sk2)` 中是 **pattern variable**，匹配任何 identifier。
- `random-sym-to-match` 匹配 `s`，走 `sk2`。

如果 `s` 已经在 `id ...` 中（比如 ellipsis 展开后的重复出现），`s` 会是 **literal**，`random-sym-to-match` 不会匹配它，走 `fk2` 做 `equal?` 比较。

---

### 第 5 步：最终展开结果

把上述步骤组装起来：

```scheme
(let ((v '(1)))
  (let ((failure (lambda () (error 'match "no matching pattern"))))
    (if (and (pair? v) (null? (cdr v)))
        (let ((w (car v)))
          (let ((s w))
            (begin s)))
        (failure))))
```

进一步简化：

```scheme
(let ((v '(1)))
  (if (and (pair? v) (null? (cdr v)))
      (let ((s (car v)))
        s)
      (error 'match "no matching pattern")))
```

**关键结论**：`s` 从 pattern 变量变成 `(let ((s (car v))) ...)` 的绑定，靠的就是 `match-two` 中 `new-sym?` 的 `sk2` 分支。

---

## 例子 2：多变量绑定 `(match '(1 2) [(a b) (+ a b)])`

### 第 1-2 步：match → match-next

```scheme
(let ((v '(1 2)))
  (let ((failure (lambda () (error 'match "no matching pattern"))))
    (match-one v (a b) ('(1 2) (set! '(1 2)))
               (match-drop-ids (begin (+ a b)))
               (failure)
               ())))
```

---

### 第 3 步：match-one → match-two（pair pattern）

`(a b)` 是两个元素的 pair，走 `match-two` 的 `(p . q)` clause：

```scheme
(if (pair? v)
    (let ((w (car v))   ; w = 1
          (x (cdr v)))  ; x = '(2)
      (match-one w a ((car v) (set-car! v))
                 (match-one x b ((cdr v) (set-cdr! v))
                            (match-drop-ids (begin (+ a b)))
                            (failure)
                            ())
                 (failure)
                 ()))
    (failure))
```

---

### 第 4 步：递归 match-two（a 和 b）

**对 `a`**（裸 symbol，新变量）：
- `match-check-identifier` → `new-sym?` 走 `sk2`
- 结果：`(let ((a w)) ...)`

**对 `b`**（裸 symbol，新变量）：
- 同理：`(let ((b x)) ...)`

---

### 第 5 步：最终展开结果

```scheme
(let ((v '(1 2)))
  (if (pair? v)
      (let ((w (car v))
            (x (cdr v)))
        (let ((a w))
          (let ((b x))
            (+ a b))))
      (error 'match "no matching pattern")))
```

即：

```scheme
(let ((v '(1 2)))
  (if (pair? v)
      (let ((a (car v))
            (b (cadr v)))
        (+ a b))
      (error 'match "no matching pattern")))
```

---

## 辅助宏协作总结

| 宏 | 职责 |
|----|------|
| **match** | 入口。把表达式绑到临时变量 `v`，调用 `match-next`。 |
| **match-next** | 给每个 clause 加上 failure continuation（`failure` lambda），然后调用 `match-one`。 |
| **match-one** | 检查 ellipsis（`...`）。如果是 list 且第二个元素可能是 `...`，走 `match-check-ellipsis`；否则透传到 `match-two`。 |
| **match-two** | **核心引擎**。28 条 clauses 覆盖所有 pattern 类型：`()`、`quote`、`and`/`or`/`not`、`?` 谓词、`=` 转换、`___` ellipsis、pair、vector、`:_` 通配、裸 symbol（走 `new-sym?`）。 |
| **match-check-identifier** | 用 hygiene 技巧确认输入是 symbol（atom），不是 list/vector。 |
| **match-drop-ids** | 忽略 identifiers 列表，只保留实际 body 表达式。 |
| **match-extract-vars** | 从 pattern 中提取所有新变量，用于 ellipsis 展开时的列表收集。 |
| **match-gen-ellipsis** | 生成 `named let` 循环，处理 `(p ...)` 模式，把匹配到的元素 cons 到各变量的累积列表，最后 `reverse`。 |

---

## 从 Pattern Variable 到 Let Binding 的关键

核心就一句：**`match-two` 的 `new-sym?` 利用 `syntax-rules` 的 literal 列表区分"已绑定"和"新变量"**。

- `new-sym?` 的 `syntax-rules` literals = `(id ...)`，即当前已累积的 bound identifiers。
- 若变量 `x` **不在** `id ...` 中 → `x` 在 pattern 中是 **pattern variable** → `random-sym-to-match` 总能匹配它 → 走 `sk2` → 生成 `(let ((x v)) ...)` 创建绑定。
- 若变量 `x` **在** `id ...` 中 → `x` 在 pattern 中是 **literal** → `random-sym-to-match` 不匹配 → 走 `fk2` → 生成 `(if (equal? v x) ...)` 做值比较。

这就是 `match` 能把 `[(s) s]` 展开成 `(let ((s (car v))) s)` 的本质。
