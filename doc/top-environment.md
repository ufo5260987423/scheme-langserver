# `top-environment` 机制说明

本文档从 `run.ss` 出发，说明 `scheme-langserver` 的 `top-environment` 是如何从命令行参数一路传递到 tokenizer 和抽象解释器的规则分发的。

## 1. 命令行入口：`run.ss`

`run.ss` 通过 SRFI-37 `args-fold` 解析 CLI。与 `top-environment` 相关的代码集中在以下位置：

```scheme
(define default-top-environment 'r6rs)

(define (top-environment-parse str)
  (cond
    ((string-ci=? str "r6rs") 'r6rs)
    ((string-ci=? str "r7rs") 'r7rs)
    ((string-ci=? str "s7")   's7)
    ((string-ci=? str "goldfish") 's7)   ; goldfish 是 s7 的别名
    (else #f)))
```

有效值只有三个符号：

| CLI 值 | 内部符号 | 说明 |
|--------|----------|------|
| `r6rs` | `'r6rs` | 默认。R6RS / Chez Scheme 语义。 |
| `r7rs` | `'r7rs` | R7RS 语义。 |
| `s7`   | `'s7`   | S7 Scheme 语义。 |
| `goldfish` | `'s7` | Goldfish 基于 S7，因此内部统一用 `'s7`。 |

解析后的值存入 options hashtable，最后作为第 6 个参数传给 `init-server`：

```scheme
(init-server
  (standard-input-port)
  (standard-output-port)
  log-port
  multi-thread
  type-inference
  top-environment)
```

## 2. 进入服务器层

`init-server` 定义在 `scheme-langserver.sls`，会把 `top-environment` 放进 `protocol/server.sls` 中定义的 `server` 记录：

```scheme
(define-record-type server
  (fields
    ...
    (immutable top-environment)))
```

之后通过 `(server-top-environment server-instance)` 读取，主要用在两个地方：

- `initialize`（`scheme-langserver.sls:176,182`）—— 创建 workspace 时传给 `init-workspace`
- `workspace/didChangeWorkspaceFolders` 处理函数 —— 新增/删除文件夹时同样要创建 workspace，也需要传递

## 3. 落到 Workspace

`analysis/workspace.sls` 中 `workspace` 记录有一个 `top-environment` 字段：

```scheme
(define-record-type workspace
  (fields
    ...
    (immutable top-environment)))
```

`init-workspace` 拿到 `top-environment` 后，会把它一路传下去：

```
init-workspace
  ├── init-virtual-file-system  ... top-environment
  ├── init-library-node         ... top-environment
  ├── init-file-linkage         ... top-environment
  └── make-workspace            ... top-environment
```

后续几个增量更新/文档初始化函数也会使用它：

- `refresh-workspace` —— 整体刷新
- `init-document` —— 新文件扫描成 document 时，调用 tokenizer
- `update-file-node-with-tail` —— 文档内容变更后重新 tokenize
- `attach-new-file` —— 新增文件
- `refresh-workspace-for` —— 局部刷新

## 4. 进入 Tokenizer

`analysis/tokenizer.sls` 中 `source-file->annotations` 的完整签名为：

```scheme
([source path start-position tolerant? maybe-document top-environment]
  ...)
```

`init-document` 和 `update-file-node-with-tail` 都以 6 参数形式调用它。

Tokenizer 只有在 `top-environment` 是 `'r7rs`、`'s7` 或 `'goldfish` 时，才会启用容错修复：

```scheme
(if (memq top-environment '(r7rs s7 goldfish))
    (private:r7rs-fixable? condition source position)
    #f)
```

目前支持的修复如下：

| 触发条件 | 修复函数 | 行为 |
|----------|----------|------|
| `#u8(...)` | `private:r7rs-fix-u8` | 改写成 `#vu8(...)` |
| `#<...>` | `private:s7-fix-bracket-symbol` | 包成符号 `|#<...>|` |
| `#"..."` | `private:s7-fix-raw-string` | 改写成普通字符串 `"..."` |
| `#_id` | `private:s7-fix-underscore` | 去掉下划线前缀 |
| `#\null`、`#\escape` | `private:r7rs-fix-char` | 改写成 `#\nul`、`#\esc` |

这些修复只在遇到 read 异常时才会触发，且只修改源字符串，不改变 token 位置。

## 5. 抽象解释器的规则分发

`analysis/abstract-interpreter.sls` 的 `establish-available-rules-from` 用 `private:top-env=?` 判断一个 identifier-reference 是否属于当前环境：

```scheme
(define (private:top-env=? standard top)
  (find (lambda (item)
          (eq? standard (identifier-reference-top-environment item)))
        top))
```

根据环境选择不同的规则处理器，例如：

| 特殊形式 | `'r6rs` | `'r7rs` / `'s7` |
|----------|---------|------------------|
| `define` | `define-process` | `define-r7rs-process` |
| `import` | `import-process` | `r7-import-process` |
| `define-library` | — | `library-import-process-r7rs` |
| `define*` | — | `define*-process`（仅 `'s7`） |
| `lambda*` | — | `lambda*-process`（仅 `'s7`） |
| `define-macro` | — | `define-macro-process`（仅 `'s7`） |

普通形式如 `lambda`、`let`、`let*` 等不随环境变化。

## 6. 其他受环境影响的分析点

### 6.1 库形式识别

`analysis/util.sls` 的 `get-library-identifiers-list` 根据环境匹配不同的顶层形式：

- `'r6rs` → `(library (name) ...)`
- `'r7rs` / `'s7` → `(define-library (name) ...)`

### 6.2 依赖图解析

`analysis/dependency/file-linkage.sls` 根据环境选择 import 处理器：

```scheme
(case top-environment
  ['r6rs library-import-process]
  ['r7rs library-import-process-r7rs]
  ['s7   library-import-process-r7rs])
```

### 6.3 内置库与元标识符表

`analysis/identifier/meta.sls` 提供两套元信息：

- `meta-library?` —— 返回哪些库被视为当前环境的内置库
- `find-meta` —— 返回某个内置库在当前环境下的标识符表

例如 `'s7` 在 R7RS 内置库基础上额外加入 `(s7)`。

## 7. 数据流一览

```
run.ss
  │ 解析 --top-environment
  ▼
init-server (scheme-langserver.sls)
  │
  ▼
server record (protocol/server.sls)
  │
  ▼
initialize / didChangeWorkspaceFolders
  │
  ▼
init-workspace (analysis/workspace.sls)
  │
  ├──► init-virtual-file-system
  ├──► init-library-node
  ├──► init-file-linkage
  └──► make-workspace (存储 top-environment)
          │
          ├──► init-document ──► source-file->annotations
          │                         │
          │                         ▼
          │              环境门控的 R7RS/S7 修复
          │
          └──► abstract-interpreter
                    │
                    ▼
          根据 top-environment 分发规则
```

## 8. 常见陷阱

- `'goldfish` 在 CLI 解析后就被映射为 `'s7`，代码里大多数地方只认 `'s7`。少数 `memq` 调用（如 tokenizer 的容错门控）会显式列出 `'goldfish` 作为冗余保护。
- `top-environment` 只影响「tokenize 容错」和「抽象解释规则分发」。它**不**改变底层 Chez Scheme reader 本身；reader 仍然按 Chez 的 R6RS 方式读源码，只是触发异常后由 tokenizer 做源码级补丁再重新解析。
- 新增一个 `top-environment` 值时，需要同时改动：`run.ss` 的解析器、`protocol/server.sls`/`analysis/workspace.sls` 中的记录字段（通常不需要变）、tokenizer 中的环境门控、`analysis/util.sls` 的库形式识别、`analysis/dependency/file-linkage.sls` 的 import 处理器选择、`analysis/identifier/meta.sls` 的元库表、以及 `analysis/abstract-interpreter.sls` 的规则分发。
