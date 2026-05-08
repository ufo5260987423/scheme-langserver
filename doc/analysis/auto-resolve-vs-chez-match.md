# Auto Macro Resolve vs Chez `match` —— 能力边界分析

## 问题

Chez 能完整展开 `(match '(1) [(s) s])` 成：

```scheme
(let ((v '(1)))
  (if (and (pair? v) (null? (cdr v)))
      (let ((s (car v)))
        s)
      (error 'match "no matching pattern")))
```

scheme-langserver 的自动宏解析（auto-resolve）能否做到同样的展开？

---

## 结论（先上结论）

**不能。** 自动解析在理论上底层用的是同一个 Chez `eval` + `syntax-case` 引擎，所以**单步展开结果和 Chez 一致**。但 `match` 是一个**深度递归的宏家族**（match → match-next → match-one → match-two → match-one → match-two → ...），自动解析在三个层面被卡死，无法走完完整链路。

---

## 三层卡死点

### 卡死点 1：嵌套宏检测（`match-two` 被拒绝）

`match-two` 的第 28 条 clause 的 template 长这样：

```scheme
((match-two v x g+s (sk ...) fk (id ...))
 (match-check-identifier
  x
  (let-syntax                      ; ← 嵌套宏定义！
      ((new-sym?
        (syntax-rules (id ...)
          ((new-sym? x sk2 fk2) sk2)
          ((new-sym? y sk2 fk2) fk2))))
    (new-sym? random-sym-to-match
              (let ((x v)) (sk ... (id ... x)))
              (if (equal? v x) (sk ... (id ...)) fk)))
  (if (equal? v x) (sk ... (id ...)) fk)))
```

`let-syntax` + 内部 `syntax-rules` 触发了我们的保守策略：

```scheme
(define (private:template-has-nested-macro? template-expr)
  ...
  [(memq (car template-expr) '(let-syntax letrec-syntax syntax-rules define-syntax)) #t]
```

**结果**：`syntax-case->generator:map+expansion` 为 `match-two` 安装一个 no-op generator（返回 `#f`）。

**影响**：自动解析走到 `match-one` → `match-two` 这一步时，`match-two` 的调用直接返回 `#f`，展开链断裂。

> 类似地，`match-check-identifier` 和 `match-check-ellipsis` 的 template 也包含 `let-syntax + syntax-rules`，所以这两个宏同样被自动解析拒绝。

---

### 卡死点 2：级联深度限制（memory cap = 5）

假设我们放宽嵌套宏检测，让 `match-two` 可以展开。`(match '(1) [(s) s])` 的完整链路需要几层？

| 层 | 宏调用 | 展开结果包含 |
|---|--------|-------------|
| 1 | `match` | `match-next` |
| 2 | `match-next` | `match-one` |
| 3 | `match-one` (对 `(s)`) | `match-two` |
| 4 | `match-two` (对 `(s)`) | `match-one` |
| 5 | `match-one` (对 `s`) | `match-two` |
| 6 | `match-two` (对 `s`) | `(let ((s w)) (begin s))` |

**需要 6 层。** 但 `expansion-generator->rule` 的 memory cap 是 5：

```scheme
(if (and (not (contain? memory expression))
         (< (length memory) 5))
  (step ...))
```

第 6 层的 `match-two` 调用会被 memory cap 拦截，不再递归 `step`。

**更复杂的例子**：`(match '(1 2) [(a b) (+ a b)])` 需要 8 层。`(match '(1 2 3) [(a b c) ...])` 需要更多。

---

### 卡死点 3：`shallow-copy` 引用回传只能处理单层

即使前两个问题都解决，`shallow-copy` 还有一个结构性缺陷：

- `shallow-copy` 通过 `pairs`（pattern → expansion 的节点映射）把 expansion AST 上的 identifier-reference 复制回原始调用位置。
- 但 `match` 的展开是多层嵌套的：每次展开都会产生**全新的 AST**，新的 AST 上又有新的宏调用。
- `shallow-copy` 只处理**当前这一层** expansion 的引用映射，无法把深层的 `let` 绑定信息逐层传回最原始的 `(match '(1) [(s) s])` 调用节点。

举个例子：
- 第 3 层 `match-one` 展开为 `match-two`，`shallow-copy` 把 `match-two` 结果中的引用复制回 `match-one` 的调用节点。
- 第 4 层 `match-two` 展开为包含 `match-one` 的 `let`，`shallow-copy` 把这一层的引用复制回 `match-two` 的调用节点。
- 但原始 `match` 调用节点**看不到**深层的 `s` 绑定信息，因为引用只回传了一层。

---

## 单步展开能力验证

虽然完整链路走不通，但**单步展开**的结果和 Chez 是完全一致的。我们用 `debug-trace/layered-auto-expand.ss` 验证过：

```scheme
; Layer 1: match
(match x [(? string? path) path])
; 自动解析展开为：
(let ((v x)) (match-next v (x (set! x)) ((? string? path) path)))
```

这和 Chez 的 `expand` 输出逐字相同。

问题不在于单步展开的正确性，而在于**级联深度**和**嵌套宏**导致的链式断裂。

---

## 如果要支持完整 match 展开，需要做什么

| 改动 | 工作量 | 风险 |
|------|--------|------|
| 放宽嵌套宏检测，允许 `let-syntax` | 小 | `let-syntax` 内部的宏在 auto-resolve 中无法正确解析，可能产生错误结果 |
| 提升 memory cap 到 20+ | 极小 | 性能恶化，对于复杂 match 表达式可能超时 |
| 让 `shallow-copy` 支持多层回传 | 大 | 需要重新设计引用传播机制 |
| 为 `match` 写专门的展开器（类似手写 `match-process`） | 中 | 绕开通用 auto-resolve，直接模拟 match 的展开逻辑 |

---

## 一句话总结

> Auto-resolve 的 `eval` + `syntax-case` 引擎和 Chez 是同一个，**单步展开永远正确**。但 `match` 是一个 6+ 层深度递归、含嵌套 `let-syntax` 的宏家族，auto-resolve 的**嵌套宏保守策略**、**memory 深度限制**和**单层引用回传**三道关卡，导致它无法走完完整链路。对于 `match`，当前仍需依赖手写规则 `match-process`。
