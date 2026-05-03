# 用宏重构 identifier rules 的方案

## 1. 问题陈述

`analysis/identifier/rules/` 目录下有 **30 个 `.sls` 文件**，每个文件实现一个特殊形式的解析规则（如 `let-process`、`define-process`、`lambda-process`）。这些文件以及 `abstract-interpreter.sls` 中的规则注册代码存在大量机械重复。

### 1.1 规则文件层面的重复

每个规则文件都遵循完全相同的模板：

```scheme
(library (scheme-langserver analysis identifier rules <name>)
  (export <name>-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (<name>-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ ...) ...]
      [else '()]))))
```

重复内容包括：
- Library header 和 imports（仅名称不同）
- `let*` + `annotation-stripped` + `match expression` 骨架
- `make-identifier-reference` 的 8 个位置参数
- `index-node-references-export-to-other-node-set!` + `append` + `` `(,reference) `` 的引用附着模式
- `append-references-into-ordered-references-for` 的重复调用
- `[else '()]` fallback

### 1.2 `abstract-interpreter.sls` 中的规则注册重复

`establish-available-rules-from` 中有 40+ 行 `cond` 分支，每行都是：

```scheme
[(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
[(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
[(equal? r '(let-values)) (private-add-rule rules `((,let-values-process) . ,identifier))]
...
```

这些行的结构完全一致，只有标识符名称和对应的 process 函数不同。

### 1.3 为什么需要用宏

- **减少维护负担**：新增一个特殊形式（如 `when`、`unless`）时，只需写核心逻辑，不用复制 30 行 boilerplate
- **统一错误模式**：reference 的创建和附着目前有 4-5 种略有不同的写法，宏可以强制统一
- **降低认知负担**：新开发者只需理解规则的核心语义（"这个形式如何绑定变量"），不用被 boilerplate 分散注意力
- **规则注册可表驱动**：将 `cond` 分支转换为数据表，更易于扩展和审查

---

## 2. 方案设计

提供三个宏，分别解决三个层面的重复：

| 宏 | 解决什么问题 | 位置 |
|----|-----------|------|
| `define-identifier-rule` | 规则文件的 boilerplate | `analysis/identifier/rules/common-macros.sls` |
| `with-reference` | `make-identifier-reference` + attach 的重复 | 同上，作为辅助宏 |
| `register-rules` | `abstract-interpreter.sls` 中的 `cond` 分支 | `analysis/identifier/rules/common-macros.sls` |

---

### 2.1 `define-identifier-rule`

#### 作用

定义一个标识符解析规则，自动生成 library header、imports 和 `let*` + `annotation-stripped` 骨架。

#### 语法

```scheme
(define-identifier-rule <process-name>
  [export <extra-export> ...]
  [import <extra-import> ...]
  <body>)
```

- `<process-name>`：如 `let-process`、`define-process`
- `export` 子句（可选）：额外导出的符号（如 `let-parameter-process`）
- `import` 子句（可选）：额外的导入（如 `(scheme-langserver analysis identifier util)`）
- `<body>`：规则的函数体，可以引用自动绑定的变量 `expression`

#### 展开示例

输入（改造后的 `lambda.sls`）：

```scheme
(define-identifier-rule lambda-process
  [export parameter-process]
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ...)]
    [else '()]))
```

展开为：

```scheme
(library (scheme-langserver analysis identifier rules lambda)
  (export lambda-process parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))
  (define (lambda-process root-file-node root-library-node document index-node)
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [(_ (identifier **1) fuzzy ... ) 
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            ...)]
        [else '()]))))
```

#### 实现要点

```scheme
(define-syntax define-identifier-rule
  (syntax-rules (export import)
    [(_ name [export extra ...] [import extra-imports ...] body)
     (let ([module-name (symbol->library-name 'name)])
       (library module-name
         (export name extra ...)
         (import 
           (chezscheme) (ufo-match)
           (scheme-langserver analysis identifier reference)
           (scheme-langserver virtual-file-system index-node)
           (scheme-langserver virtual-file-system library-node)
           (scheme-langserver virtual-file-system document)
           (scheme-langserver virtual-file-system file-node)
           extra-imports ...)
         (define (name root-file-node root-library-node document index-node)
           (let* ([ann (index-node-datum/annotations index-node)]
               [expression (annotation-stripped ann)])
             body))))
    [(_ name body)
     (define-identifier-rule name [export] [import] body)]))
```

---

### 2.2 `with-reference`

#### 作用

封装 `make-identifier-reference` + `export-to-other-node-set!` + `append-references-into-ordered-references-for` 的重复模式。

#### 语法

```scheme
(with-reference (identifier document index-node initialization-index-node library-id type)
  [attach-to target-node]
  [import-into import-node]
  [exclude-from exclude-node]
  [return? #t])
```

#### 展开示例

输入：

```scheme
(with-reference (expression document index-node initialization-index-node '() 'variable)
  [attach-to (index-node-parent index-node)])
```

展开为：

```scheme
(let ([reference (make-identifier-reference
                   expression document index-node initialization-index-node
                   '() 'variable '() '())])
  (index-node-references-export-to-other-node-set!
    index-node
    (append (index-node-references-export-to-other-node index-node) `(,reference)))
  (append-references-into-ordered-references-for document (index-node-parent index-node) `(,reference))
  `(,reference))
```

#### 适用场景

改造前的 `let-parameter-process`（17 行）：

```scheme
(let ([reference 
        (make-identifier-reference
          expression document index-node initialization-index-node
          '() type '() '())])
  (index-node-references-export-to-other-node-set! 
    index-node
    (append (index-node-references-export-to-other-node index-node) `(,reference)))
  (append-references-into-ordered-references-for document let-node `(,reference))
  `(,reference))
```

改造后（5 行）：

```scheme
(with-reference (expression document index-node initialization-index-node '() type)
  [attach-to let-node])
```

---

### 2.3 `register-rules`

#### 作用

将 `abstract-interpreter.sls` 中 `establish-available-rules-from` 的 `cond` 分支转换为声明式表。

#### 语法

```scheme
(register-rules rules identifier top
  (let let-process)
  (let* let*-process)
  (let-values let-values-process)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion)
  (begin do-nothing begin-process)
  ...)
```

规则：
- `(name process)` → 只有 pre-process：`((,process) . ,identifier)`
- `(name process post-process)` → pre + post：`((,process . ,post-process) . ,identifier)`

#### 展开示例

输入：

```scheme
(register-rules rules identifier top
  (let let-process)
  (let* let*-process)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion))
```

展开为：

```scheme
(cond 
  [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
  [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
  [(equal? r '(define-syntax)) (private-add-rule rules `((,define-syntax-process . ,define-syntax:attach-generator) . ,identifier))]
  [(equal? r '(syntax-rules)) (private-add-rule rules `((,syntax-rules-process . ,syntax-rules->generator:map+expansion) . ,identifier))]
  [else rules])
```

#### 环境约束的处理

对于需要检查 `top-environment` 的规则（如 `(define define-process r6rs)`），语法扩展为：

```scheme
(register-rules rules identifier top
  (define define-process r6rs)
  (define define-r7rs-process r7rs)
  ...)
```

展开为：

```scheme
(cond 
  [(and (equal? r '(define)) (private:top-env=? 'r6rs top))
    (private-add-rule rules `((,define-process) . ,identifier))]
  [(and (equal? r '(define)) (private:top-env=? 'r7rs top))
    (private-add-rule rules `((,define-r7rs-process) . ,identifier))]
  ...)
```

---

## 3. 改造前后对比

### 3.1 `lambda.sls`（改造前 vs 改造后）

**改造前（121 行）**：Library header + imports（15 行）+ `lambda-process`（65 行）+ `parameter-process`（40 行），大量 `make-identifier-reference` + `index-node-references-export-to-other-node-set!` 重复。

**改造后（约 50 行）**：

```scheme
(define-identifier-rule lambda-process
  [export parameter-process]
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        (if (not (null? rest))
          (begin
            (parameter-process index-node (car rest) index-node '() document)
            (loop (cdr rest)))))]
    [(_ (? symbol? identifier) fuzzy ... ) 
      (parameter-process index-node (cadr (index-node-children index-node)) index-node '() document)]
    [(_ (identifier . rest) fuzzy ... ) 
      (with-reference (identifier document omg-index-node index-node '() 'parameter)
        [attach-to index-node])
      (let loop ([rest rest])
        ...)]
    [else '()]))

(define (parameter-process initialization-index-node index-node lambda-node exclude document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (with-reference (expression document index-node initialization-index-node '() 'parameter)
        [attach-to lambda-node]
        [exclude-from (index-node-parent index-node) exclude])
      '())))
```

### 3.2 `abstract-interpreter.sls`（改造前 vs 改造后）

**改造前（lines 180-252，约 70 行 `cond`）**：

```scheme
(cond 
  [(and (equal? r '(define)) (private:top-env=? 'r6rs top))
    (private-add-rule rules `((,define-process) . ,identifier))]
  [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
  [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
  ...
  [else rules])
```

**改造后（约 15 行）**：

```scheme
(register-rules rules identifier top
  (define define-process r6rs)
  (define define-r7rs-process r7rs)
  (define define-s7-process s7)
  (define-syntax define-syntax-process define-syntax:attach-generator)
  (define-record-type define-record-type-process)
  (do do-process)
  (case-lambda case-lambda-process)
  (lambda lambda-process)
  (set! define-top-level-value-process)
  (let let-process)
  (let* let*-process)
  (let-values let-values-process)
  (let*-values let*-values-process)
  (let-syntax let-syntax-process)
  (letrec letrec-process)
  (letrec* letrec*-process)
  (letrec-syntax letrec-syntax-process)
  (syntax-case syntax-case-process r6rs)
  (syntax-rules syntax-rules-process syntax-rules->generator:map+expansion)
  (identifier-syntax identifier-syntax-process r6rs)
  (with-syntax with-syntax-process r6rs)
  (library library-import-process export-process)
  (import import-process)
  (begin do-nothing begin-process)
  (define-library library-import-process-r7rs export-process-r7rs))
```

---

## 4. 实施步骤

### Phase 1：基础设施（1 个文件）

新建 `analysis/identifier/rules/common-macros.sls`：
- 定义 `define-identifier-rule`
- 定义 `with-reference`
- 定义 `register-rules`

### Phase 2：试点改造（2-3 个文件）

选择最简单的规则文件进行试点：
1. `lambda.sls`（中等复杂度，有 `parameter-process` 辅助函数）
2. `let.sls`（稍复杂，有 `generate-naive-let-process-with` 工厂函数）
3. `define.sls`（最复杂，多种 pattern）

验证改造后的文件行为与改造前完全一致（运行相关测试）。

### Phase 3：批量改造（剩余 27 个文件）

按以下优先级分批改造：
1. **简单绑定规则**：`let*`、`letrec`、`letrec*`、`let-values`、`let*-values`、`fluid-let`
2. **参数绑定规则**：`case-lambda`、`lambda*`、`define*`、`define-macro`
3. **语法规则**：`let-syntax`、`letrec-syntax`、`fluid-let-syntax`、`syntax-case`、`with-syntax`、`identifier-syntax`
4. **顶级定义**：`define`、`define-record-type`、`define-top-level-value`、`define-top-level-syntax`
5. **库相关**：`library-import`、`import`、`invoke-library`、`load`、`load-program`、`load-library`
6. **控制流**：`do`、`begin`

### Phase 4：`abstract-interpreter.sls` 改造

将 `establish-available-rules-from` 中的 `cond` 替换为 `register-rules`。

### Phase 5：清理和文档

- 删除每个规则文件顶部的 `; reference-identifier-type include` 注释块（宏已内置类型说明）
- 更新 AGENTS.md 中的编码规范
- 确保所有测试通过

---

## 5. 风险评估

### 5.1 宏的调试难度

**风险**：如果 `define-identifier-rule` 展开后有 bug，错误信息会指向展开后的代码，而不是原始宏调用。

**缓解**：
- 在 `common-macros.sls` 中提供 `define-identifier-rule-debug` 变体，展开时打印生成的代码
- 在 `AGENTS.md` 中记录宏展开的检查方法：`scheme --script` + `(expand 'expr)`
- 保持宏足够简单（只做结构展开，不做复杂逻辑），降低出错概率

### 5.2 对现有测试的影响

**风险**：改造后的文件必须行为完全一致，否则会影响 LSP 的语义分析结果。

**缓解**：
- 每改造一个文件，运行其对应的测试文件
- 改造前后做 diff（包括 `ordered-reference-list` 和 `export-to-other-node` 的内容）
- 优先改造测试覆盖率高的文件

### 5.3 编译时开销

**风险**：宏在编译时展开，如果 `define-identifier-rule` 很复杂，可能增加编译时间。

**缓解**：
- 宏只做简单的语法转换（拼接 library header + 插入 body），不执行复杂计算
- 实测编译时间差异，如果超过 10% 则优化宏实现

### 5.4 与 `ufo-match` 的兼容性

**风险**：`define-identifier-rule` 内部使用 `match`，而 body 中也可能使用 `match`，需要确保语法不冲突。

**缓解**：
- 使用 `syntax-rules` 或 `syntax-case` 实现 `define-identifier-rule`，不用 `ufo-match`
- body 中的 `match` 保持原样，由 Chez Scheme 的 reader 正常解析

---

## 6. 附录：完整改造后的 `lambda.sls` 示例

```scheme
(library (scheme-langserver analysis identifier rules lambda)
  (export lambda-process parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules common-macros)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define-identifier-rule lambda-process
  (match expression
    [(_ (identifier **1) fuzzy ... ) 
      (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        (if (not (null? rest))
          (begin
            (parameter-process index-node (car rest) index-node '() document)
            (loop (cdr rest)))))]
    [(_ (? symbol? identifier) fuzzy ... ) 
      (parameter-process index-node (cadr (index-node-children index-node)) index-node '() document)]
    [(_ (identifier . rest) fuzzy ... ) 
      (let* ([omg-index-node (cadr (index-node-children index-node))])
        (with-reference (identifier document omg-index-node index-node '() 'parameter)
          [attach-to index-node])
        (let loop ([rest rest])
          (cond 
            [(pair? rest) 
              (with-reference ((car rest) document omg-index-node index-node '() 'parameter)
                [attach-to index-node])
              (loop (cdr rest))]
            [(not (null? rest)) 
              (with-reference (rest document omg-index-node index-node '() 'parameter)
                [attach-to index-node])]
            [else '()])))]
    [else '()]))

(define (parameter-process initialization-index-node index-node lambda-node exclude document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (with-reference (expression document index-node initialization-index-node '() 'parameter)
        [attach-to lambda-node]
        [exclude-from (index-node-parent index-node) exclude])
      '())))
)
```

对比原始 121 行，改造后约 50 行，核心语义逻辑完全保留，boilerplate 被宏吸收。
