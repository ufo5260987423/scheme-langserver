# shallow-copy 回传单层限制

## 问题描述

`shallow-copy`（在 `expansion-wrap.sls` 中）负责把宏展开结果 AST 上的 `identifier-reference` **复制回**原始宏调用节点。这样 IDE 在原始代码上 hover 或 go-to-definition 时，能看到展开后才有 binding 的变量。

但 `shallow-copy` 只能处理**当前这一层** expansion 的映射，不能逐层穿透回最外层原始代码。

---

## 具体例子：`match` 级联展开

原始代码：
```scheme
(match '(1) [(s) s])
```

### 第一层：`match` → `match-next`

`match` 的 generator 展开为：
```scheme
(let ((v '(1)))
  (match-next v (x (set! x)) [(s) s]))
```

`shallow-copy` 执行：
- 把展开结果中的 `let`、`v` 的 reference 复制回原始节点 `(match '(1) [(s) s])` ✅
- 用户在 `match` 调用上 hover → 能看到 `let`、`v`

### 第二层：`match-next` → `match-one`

`step` 递归处理，遇到 `(match-next v ...)`：

展开为：
```scheme
(let ((failure (lambda () (error 'match "no matching pattern"))))
  (match-one v (s) (x (set! x)) (match-drop-ids (begin s)) (failure) ()))
```

`shallow-copy` 执行：
- 把 `failure`、`match-one` 的 reference 复制回 `(match-next v ...)` 节点 ✅
- 但 `(match-next v ...)` 是**第一层展开结果的子节点**，不是原始代码

### 第三层：`match-one` → `match-two`

展开为：
```scheme
(match-two v (s) (x (set! x)) (match-drop-ids (begin s)) (failure) ())
```

`shallow-copy` 执行：
- 把 `match-two` 的 reference 复制回 `(match-one v (s) ...)` 节点 ✅

### 第四层：`match-two` → `let`

展开为：
```scheme
(if (and (pair? v) (null? (cdr v)))
    (let ((w (car v)))
      (match-one w s ((car v) (set-car! v)) (match-drop-ids (begin s)) (failure) ()))
    (failure))
```

`shallow-copy` 执行：
- 把 `if`、`let`、`w` 的 reference 复制回 `(match-two v (s) ...)` 节点 ✅

### 第五层：`match-one` → `match-two`

展开为：
```scheme
(match-two w s ((car v) (set-car! v)) (match-drop-ids (begin s)) (failure) ())
```

### 第六层：`match-two` → `let`（最终）

展开为：
```scheme
(let ((s w))
  (begin s))
```

`shallow-copy` 执行：
- 把 `let`、`s` 的 reference 复制回 `(match-two w s ...)` 节点 ✅

---

## 核心问题

原始代码节点 `(match '(1) [(s) s])` **看不到**第六层的 `s` binding：

```
原始节点: (match '(1) [(s) s])
    ↑ shallow-copy (第1层)
    只能看到: let, v, match-next

    第1层结果: (let ((v '(1))) (match-next v ...))
                    ↑ shallow-copy (第2层)
                    只能看到: failure, match-one

        第2层结果: (let ((failure ...)) (match-one v (s) ...))
                            ↑ shallow-copy (第3层)
                            只能看到: match-two

            第3层结果: (match-two v (s) ...)
                                ↑ shallow-copy (第4层)
                                只能看到: if, let, w

                第4层结果: (if ... (let ((w (car v))) (match-one w s ...)) ...)
                                        ↑ shallow-copy (第5层)
                                        只能看到: match-two

                    第5层结果: (match-two w s ...)
                                    ↑ shallow-copy (第6层)
                                    只能看到: let, s  ← 用户真正想看到的 binding！

                        第6层结果: (let ((s w)) (begin s))
```

每一层的 `shallow-copy` 只回传到**直接父级宏调用节点**，不会继续向上穿透。

---

## 影响

| IDE 功能 | 在原始 `(match '(1) [(s) s])` 上 | 在展开 AST 的深层节点上 |
|---------|-------------------------------|------------------------|
| Hover 看 `s` 的类型/定义 | ❌ 看不到 `let ((s w))` | ✅ 能看到 |
| Go-to-definition on `s` | ❌ 跳不到 `s` 的 binding | ✅ 能跳到 |
| Rename `s` | ❌ 可能漏掉 | ✅ 完整 |

---

## 为什么不好修复

`shallow-copy` 的 `pairs` 映射是**局部的**（当前 macro call → 当前 expansion）。如果要支持多层回传，需要：

1. **全局映射表**：记录原始代码节点 → 所有层级 expansion 中的对应节点
2. **Expansion 结果持久化**：目前 expansion AST 在 `step` 递归后就丢弃了，只保留了 reference
3. **重新设计引用传播机制**：从"单层回传"改为"级联回传"

这不是一个 bug，而是当前架构的**设计限制**。对于 `match` 这种 6+ 层级联宏，手写规则 `match-process` 仍然是必要的，因为它可以在**不展开**的情况下直接分析 pattern variable 的 binding。

---

## 一句话总结

> `shallow-copy` 是单层引用回传器。对于 `match` 这种级联宏，最深层的 `let` binding 信息只能回传到直接展开它的那一层宏调用节点，无法穿透回最外层的原始代码。IDE 在原始 `match` 调用上看不到 pattern variable 的最终 binding。
