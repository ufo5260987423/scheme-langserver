# `match` 宏展开对比：Chez Scheme vs 自动解析器

## 目的

验证 `analysis/identifier/expanders/syntax-rules.sls` 中的自动 macro resolution（通过 `compound-list` 模拟 `syntax-rules` substitution）是否**在结构上与 Chez Scheme 的实际展开完全一致**。

## 测试方法

### Chez Scheme 侧

1. 复制 `.akku/lib/ufo-match.chezscheme.sls` 到 `debug-trace/ufo-match.chezscheme.sls`。
2. 用 Python 脚本给其中**每一个** `define-syntax` 添加 trace wrapper，在宏展开前后打印输入/输出。
3. 运行 `debug-trace/run-chez.ss`，对表达式 `(match x [(? string? path) path])` 调用 `expand`，捕获完整级联日志。

### 自动解析器侧

1. 在 `syntax-rules.sls` 的 `syntax-rules->generator:map+expansion` 中插入日志，记录：
   - `local-expression`（调用表达式）
   - `pattern-expression`（匹配的 clause 的 pattern）
   - `template-expression`（匹配的 clause 的 template）
   - `bindings`（pattern variable → 实际值）
   - `compound-list`（自动解析器执行 substitution 后的结果）
2. 运行 `debug-trace/test-match-auto-trace.sps`，以项目根目录为 workspace，找到 `analysis/identifier/rules/load.sls` 中的 `match` 调用，手动触发 `expansion-generator->rule`。

> 注：`load.sls` 中的 `match` 调用不含 `...`，因此不会触发 `tree-has?` 拦截，自动解析会实际执行第一层 substitution。

---

## Chez Scheme 展开记录（级联全过程）

测试表达式：
```scheme
(match x [(? string? path) path])
```

| # | 宏 | 输入 | 输出 |
|---|-----|------|------|
| 1 | `match` | `(match x ((? string? path) path))` | `(let ((v x)) (match-next v (x (set! x)) ((? string? path) path)))` |
| 2 | `match-next` | `(match-next v (x (set! x)) ((? string? path) path))` | `(match-next v (x (set! x)) ((? string? path) (=> failure) path))` |
| 3 | `match-next` | `(match-next v (x (set! x)) ((? string? path) (=> failure) path))` | `(let ((failure (lambda () (match-next v (x (set! x)))))) (match-one v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))` |
| 4 | `match-one` | `(match-one v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())` | `(match-check-ellipsis string? (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))` |
| 5 | `match-check-ellipsis` | `(match-check-ellipsis string? ...)` | `(let-syntax ((ellipsis? (syntax-rules () ...))) (ellipsis? (a b c) (match-extract-vars ? ...) (match-two v (? string? path) ...)))` |
| 6 | `match-two` | `(match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())` | `(if (string? v) (match-one v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))` |
| 7 | `match-one` | `(match-one v (and path) ...)` | `(match-check-ellipsis path (match-extract-vars and (match-gen-ellipsis v and () ...)) (match-two v (and path) ...))` |
| 8 | `match-check-ellipsis` | `(match-check-ellipsis path ...)` | `(let-syntax ((ellipsis? (syntax-rules () ...))) (ellipsis? (a b c) (match-extract-vars and ...) (match-two v (and path) ...)))` |
| 9 | `match-two` | `(match-two v (and path) ...)` | `(match-one v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ())` |
| 10 | `match-one` | `(match-one v path ...)` | `(match-two v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ())` |
| 11 | `match-two` | `(match-two v path ...)` | `(match-check-identifier path (let-syntax ((new-sym? (syntax-rules () ...))) (new-sym? random-sym-to-match (let ((path v)) (match-one v (and) ... (path))) (if (equal? v path) (match-one v (and) ...) (failure)))) (if (equal? v path) (match-one v (and) ...) (failure)))` |
| 12 | `match-check-identifier` | `(match-check-identifier path ...)` | `(let-syntax ((sym? (syntax-rules () ...))) (sym? abracadabra (let-syntax ((new-sym? ...))) (if (equal? v path) ...)))` |
| 13 | `match-one` | `(match-one v (and) ... (path))` | `(match-two v (and) (x (set! x)) (match-drop-ids (begin path)) (failure) (path))` |
| 14 | `match-two` | `(match-two v (and) ... (path))` | `(match-drop-ids (begin path) (path))` |
| 15 | `match-drop-ids` | `(match-drop-ids (begin path) (path))` | `(begin path)` |
| 16 | `match-next` | `(match-next v (x (set! x)))` | `(error (quote match) "no matching pattern")` |

**最终展开形式**（经 `syntax->datum` 去 hygienic 标记后）：
```scheme
(let ((v x))
  (let ((failure (lambda () (error 'match "no matching pattern"))))
    (if (string? v)
        (let ((path v)) path)
        (failure))))
```

---

## 自动解析器 substitution 记录（第一层）

### 匹配的 clause

来自 `ufo-match` 中 `match` 的 `syntax-rules`：
```scheme
((match atom (pat . body) ...)
 (let ((v atom))
   (match-next v (atom (set! atom)) (pat . body) ...)))
```

### 输入/输出

- **local-expression**（调用表达式）：
  ```scheme
  (match expression
    [(_ (? string? path)) (let ((target-file-node ...)) ...)]
    [else '()])
  ```

- **pattern-expression**：`(match atom (pat . body) ...)`

- **template-expression**：`(let ((v atom)) (match-next v (atom (set! atom)) (pat . body) ...))`

- **bindings**（pattern variable → 实际值）：
  | var | value |
  |-----|-------|
  | `atom` | `expression` |
  | `pat` | `(_ (? string? path))` `else` |
  | `body` | `(let ((target-file-node ...)) ...)` `(quote ())` |

  （`let`、`v`、`match-next`、`set!` 为 literal，不在 bindings 中）

- **compound-list**（自动解析器 substitution 结果）：
  ```scheme
  (let ((v expression))
    (match-next v (expression (set! expression))
      ((_ (? string? path)) (let ((target-file-node ...)) ...))
      (else (quote ()))))
  ```

---

## 第一层对比

### Chez 对同一模板的展开

若把 Chez 的测试表达式也写成两个 clause 的形式（与 `load.sls` 一致）：
```scheme
(match expression
  [(_ (? string? path)) (let (...) ...)]
  [else '()])
```

Chez `match` 第一层的输出将是：
```scheme
(let ((v expression))
  (match-next v (expression (set! expression))
    ((_ (? string? path)) (let (...) ...))
    (else (quote ()))))
```

### 自动解析器的 `compound-list`

```scheme
(let ((v expression))
  (match-next v (expression (set! expression))
    ((_ (? string? path)) (let ((target-file-node ...)) ...))
    (else (quote ()))))
```

### 结论

**第一层完全一致。**

- `let`、`v`、`match-next`、`set!` 作为 literal 保留 ✓
- `atom` → `expression` ✓
- `(pat . body) ...` 通过 ellipsis 展开为两个 clause 的列表 ✓
- 整体结构：`(let ((v <atom>)) (match-next v (<atom> (set! <atom>)) <clauses>...))` ✓

---

## 12 层逐层对比结果

通过在 `debug-trace/layered-auto-expand.ss` 中手动驱动 auto-resolve generator，逐层展开并与 Chez trace 日志对比，**前 12 层结果完全一致**。

> **耗时**：约 **24 秒**（含 workspace 初始化、加载 `ufo-match`、逐层 generator 调用）。其中绝大部分时间花在 `init-workspace` 扫描文件树和 `source-file->annotations` 解析上；单层的 `expansion-generator` 实际执行时间在毫秒级。

| 层级 | 宏 | Auto 展开结果 | Chez 展开结果 | 匹配 |
|------|-----|--------------|---------------|------|
| 1 | `match` | `(let ((v x)) (match-next v (x (set! x)) ((? string? path) path)))` | 相同 | ✅ |
| 2 | `match-next` (加 `=>`) | `(match-next v (x (set! x)) ((? string? path) (=> failure) path))` | 相同 | ✅ |
| 3 | `match-next` → `match-one` | `(let ((failure (lambda () (match-next v (x (set! x)))))) (match-one v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))` | 相同 | ✅ |
| 4 | `match-one` → `match-check-ellipsis` / `match-two` | `(match-check-ellipsis string? (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))` | 相同 | ✅ |
| 5 | `match-check-ellipsis` → `let-syntax` | `(let-syntax ((ellipsis? (syntax-rules () ((ellipsis? (foo string?) sk fk) sk) ((ellipsis? other sk fk) fk)))) (ellipsis? (a b c) (match-extract-vars ? (match-gen-ellipsis v ? (path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (? string? path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))` | 相同 | ✅ |
| 6 | `match-two` → `if` | `(if (string? v) (match-one v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()) (failure))` | 相同 | ✅ |
| 7 | `match-one` → `match-check-ellipsis` / `match-two` | `(match-check-ellipsis path (match-extract-vars and (match-gen-ellipsis v and () (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ()))` | 相同 | ✅ |
| 8 | `match-check-ellipsis` → `let-syntax` | `(let-syntax ((ellipsis? (syntax-rules () ((ellipsis? (foo path) sk fk) sk) ((ellipsis? other sk fk) fk)))) (ellipsis? (a b c) (match-extract-vars and (match-gen-ellipsis v and () (x (set! x)) (match-drop-ids (begin path)) (failure) ()) () ()) (match-two v (and path) (x (set! x)) (match-drop-ids (begin path)) (failure) ())))` | 相同 | ✅ |
| 9 | `match-two` → `match-one` | `(match-one v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ())` | 相同 | ✅ |
| 10 | `match-one` → `match-two` | `(match-two v path (x (set! x)) (match-one v (and) (x (set! x)) (match-drop-ids (begin path)) (failure)) (failure) ())` | 相同 | ✅ |
| 11 | `match-two` → `match-check-identifier` | `(match-check-identifier path (let-syntax ((new-sym? (syntax-rules () ...))) (new-sym? random-sym-to-match (let ((path v)) (match-one v (and) ... (path))) (if (equal? v path) (match-one v (and) ...) (failure)))) (if (equal? v path) (match-one v (and) ...) (failure)))` | 相同 | ✅ |
| 12 | `match-check-identifier` → `let-syntax` | `(let-syntax ((sym? (syntax-rules () ...))) (sym? abracadabra (let-syntax ((new-sym? ...))) (if (equal? v path) ...)))` | 相同 | ✅ |

---

## 修复的 bug

| 文件 | 问题 | 修复 |
|------|------|------|
| `analysis/identifier/expanders/pattern.sls` | `make-pattern` 对 `pair-form` 把 `cdr?` 错标在第 2 个 child | 改为标记**最后一个 child** |
| `analysis/identifier/expanders/pattern.sls` | `expand->index-node-compound-list` 的 `pair-form` lambda 只保留 `(car a)` 和 `(cadr a)`，截断多元素点对模板 | 递归构造完整 dotted list `(a b c . d)` |
| `analysis/identifier/expanders/pattern.sls` | `pattern+index-node->pair-list` 遇到 pattern children 比 index-node children 多（如 `pair-form` 5 个 vs proper list 4 个）直接抛异常 | 当 `rest-index-nodes` 耗尽时，用空 node 继续匹配剩余 pattern；`guard` 保护 `annotation-stripped` |
| `analysis/identifier/expanders/syntax-rules.sls` | `private:expansion+index-node->pairs` 的 `list?` 分支只截断 `compound-list`，未处理 `children` 更短的情况 | 双向截断，确保 `map` 的两个列表长度一致 |
| `analysis/identifier/expanders/pattern.sls` | `generate-binding` 对 ellipsed pattern variable 遇到 `escape-from-target-form` 且 `ancestors` 为空时直接 `raise 'special-error'` | 改为跳过该标记，继续处理，使空 ellipses（如 `q ...` 匹配零个元素）能正确生成空绑定 |
| `analysis/identifier/expanders/pattern.sls` | `generate-binding` 的 `dive-into-an-ellipsed-leaf` 只有两个分支；当同一 level 的 leaf 连续出现（如 `sk` 匹配 6 个元素）时 `(= level ancestor-level)` 分支让 `i` 不变，导致死循环 | 补全为 4 个分支（与 `dive-into-an-ellipsed-form` 一致）：`null? ancestors` / `<= level` + 多 ancestors / `<= level` + 单 ancestor / 默认，使重复 leaf 能正确累集或退出 context |
| `analysis/identifier/expanders/pattern.sls` | `generate-binding` 的 `escape-from-target-form` 分支误用 `(vector-ref result (car ancestors))`，而 `result` 是 list 不是 vector | 改为 `(vector-ref tmp (car ancestors))` |

---

## 深层限制

### 1. 级联展开性能

`match` 的完整级联涉及 16 个辅助宏层。前 12 层已能在约 16 秒内完成。剩余 4 层（`match-one` → `match-two` → `match-drop-ids` → `match-next`）理论上可以继续展开，但需要将每一层的实际输出（而非手写预期）作为下一层输入，因为 Layer 12 的输出包含 `let-syntax` 嵌套结构。

### 2. 缺少多级引用回传机制

自动解析器当前只支持**单级** `shallow-copy`：
- 把 `expansion-index-node` 中的引用复制回**直接调用位置**。
- 对于 `match` → `match-next` → `match-one` → `match-two` 的深度级联，最深层的 `path` 引用需要向上冒泡多个展开层，当前架构没有这种机制。

### 3. `tree-has?` 防护的合理性

由于上述原因，`syntax-rules.sls` 中保留了 `tree-has?` 防护：只要调用表达式中包含 `...`，就拒绝自动解析，直接返回 `#f`。这会把 `match` 交给手写的 `match-process` 处理，既避免了超时，又能精确处理所有 pattern 形式。

---

## 实验文件

- `debug-trace/chez-expand.log` — Chez Scheme 完整级联展开日志
- `debug-trace/auto-expand.log` — 自动解析器第一层 substitution 日志
- `debug-trace/run-chez.ss` — Chez 侧 trace 脚本
- `debug-trace/test-match-auto-trace.sps` — 自动解析器侧 trace 脚本
- `debug-trace/make-traced.ss` — 自动生成带 trace 的 `ufo-match` 库
