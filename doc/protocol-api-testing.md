# Protocol API 测试优化指南

## 背景

`tests/protocol/apis/` 下的旧测试通过构造 JSON-RPC 消息流启动完整服务器循环来验证 LSP 协议实现。这种模式存在以下问题：

- 没有验证任何业务结果（唯一断言是 `(server-shutdown? server-instance)`）
- 硬编码 `Content-Length` header、JSON 请求体、文件路径
- 依赖外部 `.akku/lib` 下的文件作为测试目标
- 输出到 `~/scheme-langserver.log/out` 等硬编码路径
- 大量重复样板代码

## 新模式

### 核心原则

1. **直接调用 handler**：跳过 JSON 序列化和消息循环，直接 import `(scheme-langserver protocol apis <api>)` 并调用 handler 函数
2. **使用项目自身源码作为目标**：如 `util/binary-search.sls`、`util/association.sls`，开发者对内容有天然认知
3. **AST 推导位置**：光标位置从 AST 中动态计算，不写死 line/character
4. **验证正确性**：不仅检查"存在"，还要验证返回数据的具体值与 AST 一致

### 标准改造步骤

```scheme
(let* ([root (current-directory)]
       [workspace (init-workspace root 'akku 'r6rs #f #f)]
       [target-path (string-append root "/util/binary-search.sls")]
       [uri (path->uri target-path)]

       ;; 1. 获取 document 和 AST
       [file-node (walk-file (workspace-file-node workspace) target-path)]
       [document (file-node-document file-node)]
       [root-node (car (document-index-node-list document))]

       ;; 2. 从 AST 定位目标标识符的定义节点
       [def-node (find-define-by-name root-node 'binary-search)]
       [name-node (define-node->name-node def-node)]

       ;; 3. 计算光标位置（name-node 的 start）
       [cursor-pos (document+bias->position-list document (index-node-start name-node))]
       [cursor-line (car cursor-pos)]
       [cursor-char (cadr cursor-pos)]

       ;; 4. 构造 params 并直接调用 handler
       [params (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line cursor-line 'character cursor-char))]
       [result (handler workspace params)])
  ;; 5. 断言返回结果的正确性
  ...)
```

### 各 Handler 的验证策略

| Handler | 返回类型 | 验证重点 |
|---------|---------|---------|
| `document-symbol` | `DocumentSymbol[]` | 数量、name、kind、range 与 AST name-node 位置一致 |
| `definition` | `Location[]` | 返回 range 与定义名称节点位置一致 |
| `find-references` | `Location[]` | 数量与 AST 中调用节点数量一致，每个 range 匹配某个调用点的函数名位置 |
| `hover` | `Hover` | `contents` 非空且包含标识符名称；开启 type-inference 时包含 `"Type Inference"` |

## 关键工具函数

- `(find-define-by-name root-node 'symbol)` — 从 AST 中按名称查找 `define` 节点
- `(define-node->name-node define-node)` — 提取 define 中的名称子节点
- `(document+bias->position-list document bias)` — 将 bias 转为 `[line character]` 列表
- `(index-node-start node)` / `(index-node-end node)` — 获取节点的源码偏移量

## 发现的陷阱

### `member` 与 `find`

Chez Scheme 中 `(member x lst eq?)` 虽然可以工作，但编译器会发出参数数量警告。建议改用 `(find (lambda (y) (string=? x y)) lst)`。

### `string-contains?` 不可用

`(srfi :13 strings)` 在 Chez 环境下没有导出 `string-contains?`。如需字符串包含检查，可自定义：

```scheme
(define (string-contains? s substr)
  (let ([len (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i len) (string-length s)) #f]
        [(string=? (substring s i (+ i len)) substr) #t]
        [else (loop (+ i 1))]))))
```

### `document-symbol` / `definition` 返回的 range

这两个 handler 返回的 range 对应的是**标识符名称节点**的精确位置（如 `binary-search` 这个词的 start/end），而不是整个 `define` 表单的范围。

### type inference 崩溃

对 `util/binary-search.sls` 开启 type inference 时，`hover` 会触发崩溃：

```
Exception in filter: ((((...) . d3) . d4) . #[#{identifier-reference ...}])
is not a proper list
```

这表明 type inference 引擎在处理递归函数（如 `binary-search`）时存在 bug。测试 type inference 时应改用非递归的简单函数，如 `util/association.sls` 中的 `assq-ref`。

## 已改造文件

- `tests/protocol/apis/test-document-symbol.sps`
- `tests/protocol/apis/test-definition.sps`
- `tests/protocol/apis/test-references.sps`
- `tests/protocol/apis/test-hover.sps`

## 已删除的冗余 JSON

- `tests/resources/document-symbol.json`
- `tests/resources/document-format.json`
- `tests/resources/definition.json`
- `tests/resources/hover.json`
