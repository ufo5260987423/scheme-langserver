# scheme-langserver 多线程死锁/阻塞研究计划

> 基于 codebase 审计、日志分析和实验验证。
> 
> **核心结论修正**：初步假设认为 `workspace-mutex` 自死锁是根源，但**实验验证表明 Chez Scheme 的 `with-mutex` 是可重入的**（同一线程嵌套获取同一 mutex 完全成功）。
> 
> **最终结论**：真正的问题是 **`threaded-map` 的异常传播缺陷**——子线程异常导致 `optional-finished?` 永不被设置，`de-optional` 永久 `condition-wait`。`workspace-mutex` 在此过程中被无限期持有，成为**放大器**（所有后续请求阻塞），而非死锁根源。

---

## 1. 现象描述

从生产日志观察到：
- `read-message` 持续接收客户端请求（直到 `shutdown`）
- `send-message` 在某个时间点（如 id=12）后完全停止
- 服务器进程并未退出，CPU 也不升高，而是静默无响应

这符合**工作线程永久阻塞**的特征，而非主循环崩溃。

---

## 2. 根因分析

### 2.1 初步假设：workspace-mutex 自死锁（已证伪）

**假设路径**：
1. `init-references` 获取 `workspace-mutex`
2. serial pre-phase 消耗 `make-engine` ticks
3. ticks 耗尽触发 `expire`
4. `expire` 中的 `with-mutex (workspace-mutex workspace)` 试图重入获取
5. 假设 `with-mutex` 不可重入 → 自死锁

**证伪实验**：

```scheme
(let ([m (make-mutex)])
  (with-mutex m
    (with-mutex m
      (display "nested acquire succeeds\n"))))
; => "nested acquire succeeds"
```

Chez Scheme 的 `mutex-acquire` 和 `with-mutex` 均支持同一线程嵌套获取。**自死锁假设不成立。**

`expire` 在同一线程中重入获取 `workspace-mutex` 不会阻塞，它会正常执行 `remove:from-request-tickal-task-list` 并返回 `#<void>`，导致 `make-engine` 终止当前请求。这不会导致全局 `send-message` 停止。

### 2.2 真正的问题：`threaded-map` 异常传播导致全局阻塞

#### 2.2.1 `optional-wrapper` 的致命缺陷

`.akku/src/ufo-threaded-function/ufo-threaded-function.sls:21-29`：

```scheme
(define (optional-wrapper proc)
  (lambda args
    (let ( [optional (make-optional (make-mutex) (make-condition) #f #f) ])
      (thread-pool-add-job default-pool 
        (lambda() 
          (let((value (apply proc args)))
            (with-mutex (optional-mutex optional)
              (optional-value-set! optional value)
              (optional-finished?-set! optional #t)
              (condition-broadcast (optional-condition optional)))))))))```

**缺陷**：`(apply proc args)` **没有异常守卫**。如果 `proc`（即 `private-init-references`）抛出异常或进入无限循环：
1. 异常在子线程中传播，未被捕获
2. `optional-finished?` 永远不会被设为 `#t`
3. `condition-broadcast` 永远不会被调用
4. 主线程的 `de-optional` 永远 `condition-wait`
5. `threaded-map` 永久卡住

#### 2.2.2 与 `workspace-mutex` 的连锁反应

`init-references` 在 `workspace-mutex` 下调用 `threaded-map`：

```scheme
(with-mutex (workspace-mutex workspace-instance)
  ...
  (threaded-map 
    (lambda (pair) (private-init-references ...))
    path+syntax-pairs))
```

当 `threaded-map` 卡住时：
1. **当前 worker thread** 永远阻塞在 `de-optional`
2. **`workspace-mutex` 永远被持有**
3. 后续 `did-change` / `hover` / `completion` / `documentSymbol` 等请求尝试获取 `workspace-mutex`，全部阻塞
4. `request-queue` 的 worker threads（仅 2 个）全部耗尽
5. **客户端观察到 `send-message` 完全停止**
6. `read-message` 在主线程中不受影响，继续接收请求

#### 2.2.3 触发条件

`private-init-references` 中可能抛出异常或无限循环的环节：
- `step`（`analysis/abstract-interpreter.sls`）— 宏展开异常、语法错误
- `construct-substitutions-for`（type inference）— 类型约束矛盾、无限递归
- `process-library-identifier-excluded-references` — 库引用解析失败

已知的高风险触发点：
- `analysis/abstract-interpreter.sls:74` — 自定义宏缺少递归守卫，可导致无限循环
- 类型推断在复杂约束下可能抛出 `condition?`

#### 2.2.4 为什么不是 `expire` 的问题

`expire` 只在 `make-engine` 的 ticks 耗尽时触发。在 `threaded-map` 的 `de-optional` 阶段，主线程在 `condition-wait` 中睡眠，**不消耗 ticks**，`expire` **不会**触发。

因此 `send-message` 停止的场景不可能是 `expire` 导致的，而是 `threaded-map` 的 `de-optional` 永久等待。

---

## 3. `workspace-mutex` 的设计目的与正确性

### 3.1 设计目的

`workspace-mutex` 的核心使命（`analysis/workspace.sls:131-135`）：

> **Isolate editor document-sync operations from background analysis operations so that the workspace is never in a partially-updated state while `step` or `clear-references-for` is running.**

具体保护范围：
- `did-change` / `did-change-watched-files` 修改 document text、index-node tree、diagnoses
- `init-references` 的 serial pre-phase 调用 `clear-references-for` + `document-diagnoses-set!`
- `init-references` 的 `threaded-map` 阶段调用 `private-init-references`，修改 index-node references、document diagnoses、document refreshable flag

### 3.2 为什么不能简单移除或缩小 `workspace-mutex`

如果释放 `workspace-mutex` 后执行 `threaded-map`：
- `did-change` 可能同时修改同一 document 的 `index-node-list`
- `private-init-references` 中的 `step` 正在遍历旧的 `index-node-list` 并写入 `index-node-references`
- 两者并发会导致 dangling pointer 或半初始化节点（`c752796` 曾修复过此类崩溃）

**`workspace-mutex` 是必需的，不能移除。**

---

## 4. 影响范围评估

### 4.1 哪些请求会触发

任何最终调用 `refresh-workspace-for` → `init-references` 的请求，在多线程模式下都有风险：

| 请求 | 调用路径 |
|------|---------|
| `textDocument/documentSymbol` | `document-symbol` → `refresh-workspace-for` |
| `textDocument/completion` | `completion` → `refresh-workspace-for` |
| `textDocument/hover` | `hover` → `refresh-workspace-for` |
| `textDocument/definition` | `definition` → `refresh-workspace-for` |
| `textDocument/references` | `find-references` → `refresh-workspace-for` |
| `workspace/didCreateFiles` | `did-create` → `refresh-workspace-for` |
| `textDocument/didOpen` / `didChange` | `did-open` / `did-change` → `refresh-workspace-for`（仅在文件首次打开或 library header 变化时） |

### 4.2 触发概率

- **高概率场景**：包含自定义宏或复杂类型推断的 workspace，子线程进入无限循环或抛出未捕获异常
- **中等概率场景**：文件在分析过程中被外部修改，导致 `source-file->annotations` 读取到不一致内容
- **低概率场景**：标准库文件，分析路径简单且稳定

---

## 5. 修复方案

### 5.1 方案 A：在 `threaded-map` 的 lambda 中捕获异常（首选，最小改动）

不修改外部库，在调用点使用项目已有的 `(ufo-try)` 捕获异常：

```scheme
; analysis/workspace.sls:150-152
(threaded-map 
  (lambda (pair) 
    (try 
      (private-init-references workspace-instance (car pair) (cdr pair))
      (except c
        [(condition? c)
          (let ([document 
                  (file-node-document 
                    (walk-file (workspace-file-node workspace-instance) (car pair)))])
            (document-diagnoses-set! document
              (append (document-diagnoses document)
                `((0 0 1 ,(string-append "Analysis error: " 
                    (with-output-to-string (lambda () (pretty-print c))) 
                    "analysis" "analysis-error")))))
            '())]
        [else 
          (let ([document 
                  (file-node-document 
                    (walk-file (workspace-file-node workspace-instance) (car pair)))])
            (document-diagnoses-set! document
              (append (document-diagnoses document)
                `((0 0 1 ,(string-append "Analysis error: " 
                    (with-output-to-string (lambda () (pretty-print c))) 
                    "analysis" "analysis-error")))))
            '())])))
  path+syntax-pairs)
```

**优点**：
- 不修改外部依赖 `.akku/src/ufo-threaded-function/`
- 异常被捕获后，子线程正常设置 `optional-finished?`，`de-optional` 返回
- 错误信息被写入 `document-diagnoses`，客户端可见
- `workspace-mutex` 被正常释放

**风险**：需要确保 `document-diagnoses` 的格式与现有诊断一致。

### 5.2 方案 B：缩小 `workspace-mutex` 到 serial pre-phase（架构改进）

让 `workspace-mutex` 只保护 `clear-references-for` 和 `document-diagnoses-set!`（serial pre-phase），不包裹 `threaded-map`。`threaded-map` 的子线程通过 **per-document mutex** 与 `did-change` 同步。

```scheme
; 为 document 记录添加 mutex 字段
(define-record-type document 
  (fields 
    ...
    (immutable mutex)))

; init-references
(mutex-acquire (workspace-mutex workspace-instance))
(let ([path+syntax-pairs (map ...)])
  (mutex-release (workspace-mutex workspace-instance))
  (threaded-map 
    (lambda (pair)
      (let* ([doc (file-node-document 
                    (walk-file (workspace-file-node workspace-instance) (car pair)))])
        (with-mutex (document-mutex doc)
          (private-init-references workspace-instance (car pair) (cdr pair)))))
    path+syntax-pairs))
```

`did-change` 同样获取目标 document 的 `document-mutex`。

**优点**：单个子线程卡住只影响该 document，不影响全局。
**风险**：改动大，需要验证 `private-init-references` 是否访问其他 document 的状态。

### 5.3 方案 C：暂不修改 `expire` 中的 `workspace-mutex`

经重新审视，`expire` 中的 `workspace-mutex` 有其设计意图：

1. `tickal-task-stop? = #t` 表示任务被 `$/cancelRequest` 取消
2. 被取消的任务（如正在执行 `init-references`）可能正在更新 workspace
3. `workspace-mutex` 保护 workspace 一致性
4. `expire` 获取 `workspace-mutex` 的意图是：在清理 task 时，确保与 workspace 修改操作互斥

**当前实现的问题**：`expire` 获取 `workspace-mutex` 后，仅清理 `tickal-task-list` 并返回 `#<void>`，并未等待子线程完成。子线程（`threaded-map` 的 `private-init-references`）不持有 `workspace-mutex`，会继续修改 document。因此当前实现是**意图正确但实现不完整**的状态。

**结论**：在无法添加"等待子线程完成"机制之前，保留 `workspace-mutex` 不会带来额外危害（`with-mutex` 可重入），也不提供额外保护。暂不移除，也不依赖它。重点放在方案 A（异常捕获）上。

---

## 6. 推荐方案

**短期（立即实施）**：**方案 A**
- 在 `private-init-references` 的 `threaded-map` lambda 中添加 `try` / `except`，捕获所有异常，写入 `document-diagnoses`

**长期**：**方案 B**
- 将 `workspace-mutex` 降级为 serial pre-phase 专用锁
- 引入 `document-mutex` 实现 per-document 并发控制
- 这需要完整的 race condition 审计

---

## 7. 验证计划

### 7.1 异常注入测试

编写一个测试 fixture，其中包含一个会抛出异常的自定义宏。初始化 workspace，触发 `init-references`，验证：
- `threaded-map` 不会卡住
- `workspace-mutex` 被正常释放
- 后续 `did-change` 请求能正常响应

### 7.2 压力测试

向服务器发送 100 个 `documentSymbol` 请求，同时随机发送 `$/cancelRequest` 和 `did-change`，观察 30 分钟内是否出现 `send-message` 停止。

---

## 8. 相关文件与行号

| 文件 | 行号 | 说明 |
|------|------|------|
| `analysis/workspace.sls` | 136 | `init-references` 获取 `workspace-mutex` |
| `analysis/workspace.sls` | 150-152 | `threaded-map` 调用 `private-init-references` |
| `analysis/workspace.sls` | 168-195 | `private-init-references` 可能抛异常的环节 |
| `.akku/src/ufo-threaded-function/ufo-threaded-function.sls` | 21-29 | `optional-wrapper` 缺少异常捕获 |
| `protocol/analysis/request-queue.sls` | 59 | `expire` 中的 `workspace-mutex`（可重入，非死锁根源） |
| `analysis/abstract-interpreter.sls` | 74 | 自定义宏无限循环风险 |
| `scheme-langserver.sls` | 226-231 | `private:shutdown-server` 调用 `thread-pool-stop!` |
