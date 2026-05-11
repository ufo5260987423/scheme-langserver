# scheme-langserver Record Type Inference 深度分析

> 本文分析 `define-record-type` 在 type inference 子系统中的设计意图、实现冲突及改进方向。

---

## 1. 涉及的核心文件与职责

| 文件 | 职责 |
|------|------|
| `analysis/identifier/rules/define-record-type.sls` | Identifier 规则阶段：解析 `(define-record-type point ...)`，创建 `point`(syntax)、`make-point`(constructor)、`point?`(predicator)、`point-x`(getter)、`point-x-set!`(setter) 等 `identifier-reference`，并设置 `export-to-other-node` |
| `analysis/type/substitutions/rules/record.sls` | Type 规则阶段：遍历 `define-record-type` 的 index-node，收集其下的 identifier-reference，为 getter/setter/constructor 设置 `type-expressions` |
| `analysis/type/substitutions/rules/trivial.sls` | 兜底规则：对 usage site 的 identifier 生成 substitution。对 predicator **硬编码** `(boolean? <- (inner:list? something?))` |
| `analysis/type/domain-specific-language/interpreter.sls` | 类型解释器：解释 substitution / type-expression，生成最终类型字符串 |
| `tests/analysis/type/substitutions/rules/test-record-comprehensive.sps` | 综合测试，覆盖 record type inference 的各条路径 |

---

## 2. 设计意图

scheme-langserver 的 type inference 采用 **substitution + 解释器** 架构：

1. **Identifier 阶段**：`define-record-type.sls` 把 record 定义解析成一组 `identifier-reference`，每个 reference 带有 `type` 字段（`constructor` / `getter` / `setter` / `predicator` / `syntax`）。
2. **Type 阶段**：`record.sls` 在 AST 上找到 `(define-record-type ...)` 节点，收集其下的 identifier-reference，然后为它们设置 `type-expressions`。
3. **Usage 阶段**：`trivial.sls` 在代码使用点（如 `(make-point 1 2)` 或 `(point-x p)`）找到对应的 identifier-reference，根据其 `type` 决定如何生成 substitution。
4. **解释阶段**：`interpreter.sls` 把 substitution 解释成最终类型。

**理想的数据流**：

```
(define-record-type point (fields (mutable x)))
  ↓ identifier 阶段
创建 make-point(constructor), point?(predicator), point-x(getter), point-x-set!(setter)
  ↓ type 阶段
设置 type-expressions:
  make-point: (point? <- (inner:list? something? ...))
  point?:     (boolean? <- (inner:list? something?))
  point-x:    (something? <- (inner:list? point?))
  point-x-set!:(void? <- (inner:list? point? something?))
  ↓ usage 阶段
(point-x p) 的 usage node 的 substitution = point-x 的 identifier-reference
  ↓ interpreter 阶段
解释 identifier-reference → 读取其 type-expressions →
输出 "(something? <- (inner:list? point?))"
```

---

## 3. 设计 vs 实现：六大冲突

### 冲突一：Predicator 的「双源」问题——谁该决定它的类型？

**设计意图**：`record.sls` 统一负责所有 record identifier 的 type-expressions。

**现实**：

`record.sls:41-46` 中 predicator 的 `type-expressions` 被**显式注释掉**：

```scheme
;; Predicator type-expressions are handled by trivial.sls (hard-coded).
;; Keeping it here risks inconsistency because trivial.sls and record.sls
;; may run in different orders depending on Scheme library semantics.
;; (identifier-reference-type-expressions-set! 
;;   predicator
;;   `((,(construct-type-expression-with-meta 'boolean?) <- (inner:list? something?))))
```

原因：开发者担心 `record.sls` 和 `trivial.sls` 的执行顺序不确定。如果 `trivial.sls` 先执行，它会根据 predicator 的 `type-expressions` 生成 substitution；如果 `record.sls` 后执行并修改 `type-expressions`，前面的 substitution 就过期了。

但 `trivial.sls:98-99` 对 predicator 的处理是**硬编码**：

```scheme
[(and (equal? 'predicator type) (not (null? target-index-node)))
  (extend-index-node-substitution-list index-node `(,private-boolean? <- (inner:list? something?)))]
```

这造成了**类型来源的分裂**：
- constructor / getter / setter 的类型来自 `record.sls` 设置的 `type-expressions`
- predicator 的类型来自 `trivial.sls` 的硬编码，与 `identifier-reference-type-expressions` 完全无关

测试 `test-record-comprehensive.sps` 的 Group B/C 明确验证了这一分裂：
- `point?` 的 usage node substitution 是一个**函数类型列表**（`(boolean? <- ...)`），而非 `identifier-reference`
- 而 `make-point` 的 usage node substitution 是 `identifier-reference` 对象本身

**后果**：
1. 如果以后想给 predicator 增加更精确的类型（如 `(boolean? <- (inner:list? point?))` 而非 `(boolean? <- (inner:list? something?))`），修改点分散在 `trivial.sls` 中，无法利用 `record.sls` 已经知道的 predicator 信息。
2. `identifier-reference-type-expressions` 对 predicator 永远是空的，破坏了「每个 identifier-reference 都携带自己的类型信息」这一设计假设。

---

### 冲突二：Constructor 返回类型中嵌入了 Identifier-Reference 对象——类型 AST 被运行时对象污染

**设计意图**：type-expressions 应该是纯粹的类型 AST，使用自定义 DSL 表示。

**现实**：

`record.sls:47-49` 设置 constructor 的 type-expression：

```scheme
(identifier-reference-type-expressions-set! 
  constructor 
  `((,predicator <- (inner:list? something? ...))))
```

这里 `predicator` 是一个 **`identifier-reference` 对象**（由 `find` 从 collection 中取出），被直接嵌入到 type-expression 的 quasiquote 中。

`interpreter.sls:287-293` 处理 `identifier-reference?` 的逻辑：

```scheme
[(identifier-reference? expression)
  (cond 
    [(and (not (null? (identifier-reference-type-expressions expression))) 
          (contain? '(constructor getter setter) (identifier-reference-type expression)))
      (type:environment-result-list-set! env (identifier-reference-type-expressions expression))]
    [else (type:environment-result-list-set! env `(,expression))])]
```

**关键问题**：当 interpreter 在解释 constructor 的 type-expression `((,predicator <- ...))` 时，它会递归解释内部的 `predicator`（一个 identifier-reference）。由于 `predicator` 的 `type` 是 `'predicator`，不在 `(constructor getter setter)` 列表中，所以走 `else` 分支——**直接返回 `identifier-reference` 对象本身**。

这导致 hover 输出中 constructor 的返回类型显示为：

```
([identifier-reference point?] <- (inner:list? something? ...))
```

而不是期望的：

```
(point? <- (inner:list? something? ...))
```

测试 `test-record-comprehensive.sps` Group D（L200-207）明确验证了这一现象：

```scheme
(test-assert "constructor return type is identifier-reference (predicator)"
  (identifier-reference? (car type-expr)))
(test-equal "constructor return type identifier is point?"
  'point? (identifier-reference-identifier (car type-expr)))
```

**后果**：
1. 类型 AST 不再是纯粹的 DSL，而是混合了 `identifier-reference` 和 `index-node` 等运行时对象。
2. `inner-type-checker.sls` 和 `interpreter.sls` 被迫处理这些运行时对象，增加了复杂度。
3. hover 输出中出现 `[identifier-reference point?]` 这种对终端用户不友好的字符串。

---

### 冲突三：Record 继承（Parent）在 Type 侧完全缺失

**设计意图**：R6RS 的 `define-record-type` 支持 `(parent parent-name)`，允许 record 继承父 record 的字段。

**现实**：

`define-record-type.sls:43-67` 有完整的 parent 处理逻辑：
- 解析 `(parent parent-name)`
- 查找父 record 的 `define-record-type` 节点
- 遍历父 record 的 fields，为当前 record 生成继承的 getter/setter

但 `record.sls` 完全没有处理 parent：

```scheme
(define (define-record-type-process document index-node)
  (match expression
    [(_ dummy0 dummy1 ...) 
      (let ([collection (private-collect-identifiers index-node)])
        ...)]))
```

`private-collect-identifiers` 只是递归收集 `index-node-references-export-to-other-node`，如果 parent 的 fields 已经被正确地 export 到当前节点，理论上可以收集到。但 `record.sls` 假设 `predicator` 和 `constructor` 都存在于 `collection` 中，而 parent 场景下这些 identifier 的分布更复杂。

更重要的是，即使收集到了 parent 的 getter/setter，它们的 type-expression 中嵌入的 `predicator` 应该是**父 record 的 predicator**，而非当前 record 的 predicator。`record.sls` 对此没有任何区分。

**后果**：带 `(parent ...)` 的 record 定义，其继承字段的 type inference 结果不正确或缺失。

---

### 冲突四：执行顺序依赖导致「时而工作、时而失败」

**设计意图**：type inference 应该是确定性的，不受 Scheme 库加载顺序影响。

**现实**：

`record.sls` 依赖 identifier 阶段已经创建并 export 了所有 identifier-reference。它通过 `private-collect-identifiers` 遍历 `index-node-references-export-to-other-node` 来收集这些 reference。

但 `export-to-other-node` 的设置是在 `define-record-type.sls` 中完成的，而 `record.sls` 的调用是在 `generator.sls` 的 `step` 中。`step` 的调用时机取决于：
1. `init-references` 的完成时机
2. `construct-substitutions-for` 的调用时机
3. 各 `.sls` 文件的加载顺序（Scheme 库的 `import` 解析顺序）

`record.sls` 的注释明确承认了这一风险：

> "trivial.sls and record.sls may run in different orders depending on Scheme library semantics"

`test-record-comprehensive.sps` 的 Group F（L235-252）甚至发现了一个相关异常：

```scheme
(let ([idx-node (identifier-reference-index-node make-point-ref)])
  (display (if (index-node? idx-node) "index-node"
             (if (annotation? idx-node) "annotation"
               (if (null? idx-node) "null" "other")))))
```

`identifier-reference-index-node` 有时返回 `annotation` 对象而非 `index-node`。这说明 identifier 创建和 type 规则之间的数据一致性存在问题。

**后果**：
1. 同样的代码在不同的环境（Chez 版本、加载顺序）下可能产生不同的 type inference 结果。
2. 测试可能在某些机器上通过，在另一些机器上失败。

---

### 冲突五：Getter/Setter 的类型签名过于粗糙

**设计意图**：getter 返回字段类型，setter 接收 record 实例和新值。

**现实**：

`record.sls` 为所有 getter 设置的 type-expression 是：

```scheme
(something? <- (inner:list? ,predicator))
```

为所有 setter 设置的是：

```scheme
(void? <- (inner:list? ,predicator something?))
```

这里有两个问题：

1. **getter 返回类型永远是 `something?`**：如果 record 定义了 `(fields (mutable x))`，`point-x` 的返回类型理论上应该是 `x` 的初始化类型。但当前实现完全丢失了这个信息，一律用 `something?`（万能类型）。

2. **setter 的新值类型也是 `something?`**：没有对 mutable field 的类型做任何约束。

对比 lambda 的类型推断（参数类型可以从调用点推导），record 的字段类型在定义时就已确定（从 `make-point` 的调用参数），但系统没有建立这种联系。

**后果**：
1. Record 的 getter/setter 类型推断对用户几乎没有价值——`something?` 等于「我不知道」。
   - **注**：`interpreter.sls` 的 `private-substitute-index-node&macro` 曾把 `(something? <- (inner:list? point?))` 错误折叠为裸的 `something?`，已修复（见下方「已修复的 Interpreter Bug」）。现在 hover 至少会显示完整的函数签名。
2. 无法检测 `(point-x-set! p "hello")` 是否类型正确（如果 `x` 原本是 `number?`）。

---

### 已修复：Interpreter 二次解释时 Lambda 类型表达式被错误折叠

**问题**：

`type:interpret->strings` 在格式化类型结果前，会先调用 `private-substitute-index-node&macro` 做预处理。原实现中 `[(list? target) ...]` 分支包含一个短路逻辑：

```scheme
(let ([tmp (map private-substitute-index-node&macro target)])
  (if (equal? 'something? (car tmp))
    'something?
    tmp))
```

当目标是一个 lambda 类型表达式 `(something? <- (inner:list? predicator))` 时，递归 map 后 `car tmp` 恰好是 `something?`，导致**整个 lambda 被折叠成裸的 `'something?`**。随后 `type:interpret-result-list` 再次解释时只看到一个 symbol，最终 hover 输出就是光秃秃的 `something?`，用户完全不知道自己 hover 的是一个函数。

**修复**（`interpreter.sls:58`）：

在 `private-substitute-index-node&macro` 中增加 `[(inner:lambda? target) (map private-substitute-index-node&macro target)]` 分支，匹配优先级高于 `list?`。lambda 类型表达式从此跳过 `(equal? 'something? (car tmp))` 的短路检查，保留完整的 `(return-type <- param-list)` 结构。

**效果**：
- hover record getter 时，从显示 `something?` 变为显示 `(something? <- (inner:list? [identifier-reference point?]))`
- 该修复对所有 return type 为 `something?` 的 lambda 类型表达式均有效，不限于 record

---

### 冲突六：Type-Expressions 的更新不是原子的——Mutable 状态导致数据竞争

**设计意图**：`identifier-reference` 的 `type-expressions` 字段一旦设置就不变。

**现实**：

`record.sls` 使用 `identifier-reference-type-expressions-set!` 直接修改 mutable 字段：

```scheme
(identifier-reference-type-expressions-set! getter `((something? <- (inner:list? ,predicator))))
```

在多线程模式下（`threaded?=#t`），`init-references` 和 `construct-substitutions-for` 可能并行执行。虽然 `workspace.sls` 有 `mutex`，但 `identifier-reference` 对象本身没有锁。

如果多个文档同时定义了同名的 record type（比如两个库都定义了 `point`），它们的 type-expressions 设置可能互相覆盖。

更现实的问题是：**增量更新**。当用户修改文件中的 `define-record-type`（比如增加一个字段），`refresh-workspace-for` 会重新运行 `construct-substitutions-for`。但旧的 `identifier-reference` 对象可能仍然存在于其他文档的 cache 中，其 `type-expressions` 已经是过时的。

**后果**：
1. 多线程或增量更新场景下，type inference 结果可能不稳定。
2. 无法安全地缓存 `identifier-reference` 的 type-expressions。

---

## 4. 改进方案

### 方案 A：统一 Predicator 来源，消除双源（短期）

**改动**：
1. 取消 `record.sls` 中对 predicator 的注释，恢复设置 `type-expressions`。
2. 修改 `trivial.sls`，移除对 predicator 的硬编码逻辑，统一走 `identifier-reference-type-expressions`。
3. 在 `workspace.sls` 中确保 `construct-substitutions-for` 在 `init-references` **完全结束后**再调用。

**好处**：消除类型来源分裂，predicator 也能返回精确的 `(boolean? <- (inner:list? point?))`。

**风险**：需要验证执行顺序在所有 Scheme 实现中都稳定。

---

### 方案 B：Type-Expressions 中不再嵌入 Identifier-Reference 对象（中期）

**改动**：
1. Constructor 的返回类型从 `((,predicator <- ...))` 改为 `((point? <- ...))`——用**纯符号**代替 `identifier-reference`。
2. `interpreter.sls` 在解释类型表达式中的符号时，在环境中查找其对应类型（类似当前查找 `rnrs-meta-rules` 的方式）。
3. 引入 `type:environment` 的符号表，记录 `point?` → `boolean?` 的映射。

**好处**：
- 类型 AST 保持纯粹，不再混入运行时对象。
- hover 输出干净，用户看到 `point?` 而非 `[identifier-reference point?]`。
- 更容易序列化和缓存 type-expressions。

---

### 方案 C：引入结构化 Record Type DSL（中期）

**改动**：
1. 新增 `inner:record?` 类型表达式：
   ```scheme
   (inner:record? point? (fields (x number?) (y string?)))
   ```
2. `record.sls` 不再分别设置 constructor/getter/setter 的 type-expressions，而是统一设置 record 本身的类型定义。
3. `interpreter.sls` 根据 `inner:record?` 自动推导：
   - constructor: `(point? <- (inner:list? number? string?))`
   - getter `point-x`: `(number? <- (inner:list? point?))`
   - setter `point-x-set!`: `(void? <- (inner:list? point? number?))`

**好处**：
- 字段类型不再丢失，从 `something?` 提升到具体类型。
- 天然支持 parent 继承：`(inner:record? child? (parent point?) (fields (z boolean?)))`
- 类型错误检测能力大幅提升（如 `point-x-set!` 收到 `string?` 可以报警）。

---

### 方案 D：把 Record Type 处理合并到 Identifier 阶段（长期）

**改动**：
1. 在 `define-record-type.sls` 创建 `identifier-reference` 时，**直接设置** `type-expressions`。
2. 删除 `record.sls` 的 `define-record-type-process`，或将其简化为空壳。
3. `generator.sls` 的 `step` 对 `define-record-type` 节点只做递归遍历 children，不再做特殊的 type rule 处理。

**好处**：
- 彻底消除 identifier 阶段和 type 阶段的执行顺序依赖。
- `type-expressions` 在对象创建时就确定，之后不再 mutable，消除数据竞争。
- 代码路径更简单，易于维护。

**风险**：`define-record-type.sls` 当前不知道字段的具体类型（如 `x` 是 `number?`），只能设置 `something?`。如果要支持精确字段类型，需要等 `construct-substitutions-for` 完成后才能知道。因此这个方案与「精确字段类型」目标有冲突。

---

### 方案 E：延迟精确化（Lazy Refinement）

**改动**：
1. Identifier 阶段创建 reference 时，设置**粗略**的 type-expressions（字段类型用 `something?`）。
2. `construct-substitutions-for` 完成后，运行一个**二次遍历**（`refine-record-types`），根据 `make-point` 在代码中的实际调用参数，推断字段的具体类型，然后更新 type-expressions。
3. 使用 immutable 的 type-expression 结构，更新时创建新对象而非 `set!`。

**好处**：
- 结合方案 A 和 D 的优点：创建时就有类型，后续还能精确化。
- 不引入 mutable 状态，支持多线程和缓存。
- 可以从 `(make-point 1 2)` 的调用点推断 `x` 和 `y` 是 `number?`。

---

## 5. 测试覆盖现状

当前已有测试：

| 测试文件 | 覆盖内容 |
|---------|---------|
| `tests/analysis/type/substitutions/rules/test-record.sps` | 真实项目中的 `define-record-type`（`position->alist` 的 `position-line`、document 的 setter） |
| `tests/analysis/type/substitutions/rules/test-record-comprehensive.sps` | `record-type` fixture 的全面测试：constructor/getter/setter/predicator 的 type-expressions、interpreter 结果、predicator 双源问题、identifier-reference-index-node 异常 |
| `tests/robustness-editor-fuzz.sps` | 把 `record-type/point.scm.txt` 作为 fuzzing fixture，验证变异后不会崩溃 |

**测试缺口**：
- 没有覆盖 `(parent ...)` 继承场景的类型推断
- 没有覆盖增量更新（修改 `define-record-type` 后 refresh）的类型一致性
- 没有覆盖多线程并发创建同名 record type 的场景

---

*文档版本：2024-05-09*
