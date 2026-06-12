# Workspace Persistence Task

> 目标：scheme-langserver 重启后，可直接从本地硬盘的 bytevector 缓存加载 virtual-file-system，对变更文件及其依赖重新分析，大幅降低重启时间。
>
> 硬约束：**不能使用 FASL**，因为 FASL 与 Chez Scheme 版本绑定，不可接受。

---

## 1. 需求（来自产品决策）

1. 持久化对象：**整个 workspace**（`file-node` 树、`document` 列表、`library-node` 树、`file-linkage`、每个 `document` 的 `index-node-list` 和 `ordered-reference-list`、identifier-reference 网络，以及 type-inference 结果）。
2. 格式：**版本无关的 bytevector 协议**，不依赖 Chez 版本。
3. 增量：重启时仅对内容变更的文件及其依赖重新分析。
4. 缓存路径：**由 `run.ss` 命令行参数输入**，而非硬编码在项目目录下。

---

## 2. `run.ss` 命令行参数变更

新增选项：

```
-c, --cache-path <dir>     Directory to read/write workspace cache.
                           Default: disabled (no caching).
                           Example: --cache-path ~/.cache/scheme-langserver
```

改动点：
- `run.ss` 的 `options` 列表增加 `option '(#\c "cache-path") ...`
- `make-default-options` 中默认值为 `#f`（表示不启用缓存）
- `init-server` 签名增加 `cache-path` 参数（或 `#f`）
- 该路径一路透传到 `init-workspace`，由 `workspace-cache.sls` 使用

---

## 3. 序列化引擎：`ufo-persistence`

**决策：使用 `ufo-persistence` 替代从零手写 bytevector 协议。**

`ufo-persistence` 是已经实现并测试过的 Chez Scheme 对象持久化库，核心能力：

- 自定义二进制格式（magic `"UFOP"`，带 CRC32，非 FASL）
- 支持：null、bool、fixnum、bignum、flonum、char、symbol、string、bytevector、pair、vector、eq-hashtable、eqv-hashtable、**record**（需注册）、**循环引用**
- 四阶段解码器：占位符 → record 实例化 → pair/vector 填充 → mutable record 字段回填
- 提供 `object->bytevector` / `bytevector->object` 以及原子文件写入 `persist-object` / `restore-object`

### 3.1 为什么可以替代手写协议

原 `task.md` 计划从零实现：varint、tag 协议、object table、2-pass encode/decode、手动 RTD 反射和字段序列化。`ufo-persistence` 已经完整实现了这些，且额外支持循环引用和原子文件写入。直接复用可节省 ~2 周实现时间。

### 3.2 必须解决的问题

| 问题 | `ufo-persistence` 现状 | 解决方案 |
|------|------------------------|----------|
| **Record 注册** | 所有自定义 record 类型必须在反序列化前通过 `register-record-type!` 注册 | 在 `workspace-cache.sls` 初始化阶段，为 scheme-langserver 的全部 workspace record 类型调用 `register-record-type!` |
| **`equal-hashtable`** | 编码时会检测 `equal?` 并写入 tag `#x24`，但**解码时直接抛出 `unsupported-type-error`**（reserved） | `file-linkage` 的 `path->id-map` 使用 `(make-hashtable string-hash equal?)`。必须在**序列化前**将 `path->id-map` 替换为空 `eq-hashtable`；加载后再从 `id->path-map` 重建。 |
| **`annotation` / `source-object`** | Chez 内置密封 record，无公开构造函数，`ufo-persistence` 无法原生处理 | 在 dummy port 上构造示例 `annotation` / `source` 实例，获取 RTD 与构造函数，通过 `register-record-type!` 注册。序列化/反序列化直接走 `ufo-persistence`，无需代理替换。 |
| **`document-diagnoses`** | 字符串等基础类型完全支持 | 该字段是运行时状态，本来就不持久化 |

### 3.3 Record 注册清单

scheme-langserver 的 workspace 对象图中包含以下自定义 record 类型，反序列化前必须注册：

| Record | 定义文件 | 需注册字段数 | 备注 |
|--------|----------|--------------|------|
| `file-node` | `virtual-file-system/file-node.sls` | `children`, `document` | `parent` immutable，通过 `children` 反向引用形成环 |
| `library-node` | `virtual-file-system/library-node.sls` | `children`, `file-nodes` | 同 `file-node` |
| `document` | `virtual-file-system/document.sls` | `text`, `index-node-list`, `ordered-reference-list`, `refreshable?`, `line-length-vector` | `diagnoses` 不持久化 |
| `index-node` | `virtual-file-system/index-node.sls` | `parent`, `children`, `references-*`, `excluded-references`, `substitution-list`, `import-file-nodes`, `expansion-generator` | `datum/annotations` 直接序列化 |
| `identifier-reference` | `analysis/identifier/reference.sls` | `index-node`, `initialization-index-node`, `parents`, `type-expressions`, `top-environment`, `syntax-expander`, `usage-count` | 循环引用密集 |
| `file-linkage` | `analysis/dependency/file-linkage.sls` | `path->id-map`, `id->path-map`, `matrix` | `path->id-map` 在序列化前清空，加载后重建 |

`workspace` record **不直接序列化**，因为它的 `facet` 字段是过程。实际序列化的是一个 alist payload，加载后再用 `rebuild-workspace-from-payload` 重新创建 `workspace` 记录。

注册方式（在 `workspace-cache.sls` 初始化函数中）：

```scheme
(init-workspace-cache-registry!)
```

该函数会注册所有自定义 record 以及 Chez 内置的 `source-file-descriptor`、`annotation`、`source`。

---

## 4. 缓存文件结构

缓存目录由 `run.ss` 的 `--cache-path` 指定，内部结构：

```
<cache-path>/
└── workspace.bin              # 单文件：wrapper + manifest + payload
```

旧版本曾拆分为 `manifest.sexp` + `workspace.bin`，现已合并为单个文件。

顶层结构是 `ufo-persistence` 序列化的对象：

```scheme
(cache-wrapper
  <manifest>
  <payload>)
```

- `cache-wrapper`：固定标签，用于识别格式并快速拒绝旧格式缓存。
- `<manifest>`：元数据信封。
- `<payload>`：workspace 快照（alist）。

`ufo-persistence` 的 `persist-object` 已实现原子写入（`.tmp` → `rename`），直接使用。

### 4.1 Manifest

```scheme
(cache-manifest
  (format-version 1)
  (ufo-persistence-version 2)
  (langserver-version "2.1.0-5-gxxxxxx")
  (facet txt)
  (top-environment r6rs)
  (created-at "2026-06-07T09:00:00"))
```

字段说明：

| 字段 | 含义 |
|------|------|
| `format-version` | scheme-langserver 缓存结构版本，当前为 `1` |
| `ufo-persistence-version` | `ufo-persistence` 内部格式版本，当前为 `2` |
| `langserver-version` | scheme-langserver 二进制版本，由 `git describe --tags --always --dirty` 得到 |
| `facet` | 文件过滤模式，如 `akku` / `txt` |
| `top-environment` | 顶层环境，如 `r6rs` / `r7rs` / `s7` |
| `created-at` | 缓存创建时间，ISO 8601 格式 |

manifest 匹配失败会直接冷启动。

---

## 5. 持久化内容清单

以下对象通过 alist payload 完整持久化到 `workspace.bin`：

| 对象 | 是否持久化 | 说明 |
|------|------------|------|
| `file-node` 树 | ✅ | 完整树结构 |
| `library-node` 树 | ✅ | 完整树结构 |
| `file-linkage` | ✅ | 含矩阵、`id->path-map`。`path->id-map` 在序列化前清空、加载后重建 |
| `document` record | ✅ | **包括 `document-text`** |
| `document:diagnoses` | ❌ | 运行时诊断，启动时清空 |
| `index-node-list` | ✅ | 每个 document 的 AST（`datum/annotations` 直接序列化） |
| `index-node:substitution-list` | ✅ | Type-inference 第一阶段结果 |
| `ordered-reference-list` | ✅ | document 级别的引用列表 |
| `identifier-reference` 网络 | ✅ | 含 `parents`、`type-expressions` 等 mutable 字段 |

### 5.1 为什么要持久化 type-inference 结果

Type inference 是 scheme-langserver 最昂贵的计算之一：
- Phase I（substitution generation）为每个 `index-node` 产生 `substitution-list`。
- Phase II（DSL interpreter）在调用时才对 `identifier-reference-type-expressions` 求值。

如果缓存中不保存 `index-node-substitution-list`，启动后每个文件都需要重新跑 Phase I，仍然非常慢。保存它之后，只有在文件内容变更时才需要重新生成替换。

`identifier-reference-type-expressions` 同样要持久化，因为它由 `substitution-list` 和引用图共同决定，重建成本和 Phase I 相当。

---

## 6. 加载与失效流程

### 6.1 Wrapper / Manifest 匹配

1. `init-workspace` 被调用时，如果 `cache-path` 非 `#f`：
   a. 检查 `<cache-path>/workspace.bin` 是否存在。
   b. `restore-object` 读取顶层 wrapper。
   c. 校验 wrapper 是否为 `(cache-wrapper <manifest> <payload>)`。
   d. 校验 manifest：format-version、ufo-persistence-version、langserver-version、facet、top-environment。
   e. 任一不匹配：跳过缓存，走完整初始化。
   f. 匹配：反序列化 payload，用 `rebuild-workspace-from-payload` 重新创建 `workspace` 记录。
   g. 从 `id->path-map` 重建 `path->id-map`。

### 6.2 文件内容一致性校验

**原则**：`document-text` 必须保存在缓存中，启动时直接使用缓存文本；但同时必须校验缓存文本与磁盘当前内容是否一致，以决定缓存是否失效。

**当前实现（保守策略）**：

1. 加载 `workspace.bin` 后，每个 `document` 已经带有 `text`。
2. 遍历缓存中的所有 `file-node`：
   - 读取磁盘当前内容。
   - 与缓存中的 `document-text` 执行 `string=?` 比较。
   - 文件不存在，或内容与缓存不一致，视为失效。
3. **只要任一文件失效**，就调用 `refresh-workspace` 从磁盘全量重建 workspace 并重新分析。
4. 如果全部一致，则直接使用缓存。

> 这是第一版的保守实现，优先保证正确性。真正的增量刷新见 §9。

### 6.3 为什么 `document-text` 必须保存并校验

- LSP 的 source-of-truth 在服务器内存中的 document，不是磁盘文件。`textDocument/didChange` 只会更新内存，不会立即写盘。
- 如果只存 AST 不存 text，启动时从磁盘重新读 text，那么上次运行时客户端推送的未保存修改会全部丢失。
- 因此 `document-text` 必须随缓存一起保存；启动时直接用缓存 text 作为工作文本。
- 失效机制用 `string=?` 比较缓存 text 与当前磁盘内容：一致则缓存有效；不一致则缓存失效。

### 6.4 缓存重写策略

当前采用"整个 `workspace.bin` 重写"：
- 单文件缓存简单，record 之间的引用不需要跨文件序列化。
- `ufo-persistence` 的 `persist-object` 已提供原子写入。
- workspace 大小在典型项目中可控；重写一次的成本远低于重新分析所有文件。

写缓存时机：
- 启动时如果缓存有效且文件未变，不写入。
- 服务器退出时（收到 `exit` 通知或客户端断开 EOF）调用 `save-workspace-cache-for!` 写入。

---

## 7. 待确认项 / 分歧点

| # | 事项 | 当前方案 | 状态 |
|---|------|----------|------|
| 1 | 缓存路径 | 由 `run.ss` 的 `--cache-path` 参数传入，默认不启用 | ✅ 已确认 |
| 2 | 单文件 vs 多文件缓存 | **单文件 `workspace.bin`（内嵌 wrapper + manifest）** | ✅ 已确认 |
| 3 | `document-text` | **保存到缓存，启动时用 `string=?` 对比磁盘内容以校验一致性** | ✅ 已确认 |
| 4 | type-inference 结果 | **持久化 `index-node-substitution-list` 和 `identifier-reference-type-expressions`** | ✅ 已确认 |
| 5 | 缓存失效粒度 | **当前保守全量刷新**；增量刷新见 §9 | ⚠️ 部分完成 |
| 6 | 调试工具 | `bin/dump-workspace-cache.sps` | ✅ 已实现 |
| 7 | 写缓存时机 | 启动时读取，**退出时写入** | ✅ 已确认 |
| 8 | annotation 处理 | **直接注册 Chez 内置 `annotation` / `source` record 类型** | ✅ 已确认 |
| 9 | `equal-hashtable` | `file-linkage:path->id-map` **序列化前清空、加载后重建** | ✅ 已确认 |

---

## 8. 实施计划（已更新状态）

### Step 1: `workspace-cache.sls` 骨架 + Record 注册
- ✅ 新建 `analysis/workspace-cache.sls`
- ✅ 引入 `ufo-persistence`，实现 `init-workspace-cache-registry!`
- ✅ 实现 `load-workspace-cache` / `save-workspace-cache!`
- ✅ 单元测试：注册后序列化/反序列化 record 实例

### Step 2: 最小 Workspace 快照
- ✅ 序列化 `file-node` 树 + `library-node` 树 + `file-linkage`
- ✅ `file-linkage` 的 `path->id-map` 加载后重建
- ✅ 测试 `walk-file` / `walk-library` 加载后正常

### Step 3: Document + Index-node
- ✅ 加入 `document`（包括 `document-text`，不含 `diagnoses`）和 `index-node-list`
- ✅ 加入 `index-node-substitution-list`
- ✅ `annotation` 直接通过 record 注册序列化
- ✅ 测试解析结果等价性

### Step 4: Identifier-reference 网络
- ✅ 加入 `ordered-reference-list`、`references-*`、parent chains
- ✅ 加入 `identifier-reference-type-expressions`
- ✅ 测试循环引用加载后 `eq?` 成立

### Step 5: 失效 + run.ss 参数
- ✅ `string=?` 文本对比
- ✅ `--cache-path` 命令行参数
- ✅ 退出时保存缓存
- ⚠️ 增量依赖传播（§9）

### Step 6: 测试与基准
- ✅ 缓存命中/失效/部分变更测试
- ⬜ 用 `bin/benchmark-type-inference.ss` 测重启时间

---

## 9. 增量刷新计划（下一阶段）

### 9.1 目标

当前实现是"任一文件变化就全量刷新"。下一阶段要实现**只重新分析变更文件及其依赖者**，从而保留未变更文件的缓存分析结果。

### 9.2 变更文件检测

保持现有逻辑：加载缓存后，遍历 `file-node` 树，对每个 `document` 用 `string=?` 比较缓存 `document-text` 与磁盘文本。变更文件集合记为 `changed-paths`。

### 9.3 处理变更文件

对 `changed-paths` 中的每个文件调用 `update-file-node-with-tail`：

1. 更新 `document-text` 为磁盘当前内容。
2. 重新生成 `document-line-length-vector`。
3. 用 `source-file->annotations` 重新生成 `index-node-list`。
4. 如果 library header（imports/exports/library name）发生变化：
   - 更新 `library-node` 树。
   - 调用 `init-file-linkage` 重建 `file-linkage`。
5. 将该文件标记为 `document-refreshable?=#t`。
6. 通过 `get-reference-path-to` 找出所有依赖该文件的文件，也把它们的 `document-refreshable?` 设为 `#t`。

### 9.4 收集需刷新的文件

第二次遍历 `file-node` 树，收集所有 `document-refreshable?=#t` 的文件路径，得到 `refreshable-paths`。

### 9.5 拓扑批次与重新分析

```scheme
(let ([linkage (workspace-file-linkage workspace-instance)]
      [batches (shrink-paths linkage refreshable-paths)])
  (workspace-undiagnosed-paths-set! workspace-instance refreshable-paths)
  (init-references workspace-instance batches))
```

- `shrink-paths` 会按依赖拓扑把路径分成批次。
- `init-references` 只处理这些批次，未变更文件保持原有分析结果。

### 9.6 Script 文件的特殊处理

Script 文件没有 library identifier，不进入 `file-linkage`。对变更的 script 文件，需要单独把它加入 `workspace-undiagnosed-paths`，并直接调用：

```scheme
(init-references workspace-instance `((,script-path)))
```

### 9.7 新增 / 删除文件

- **新增文件**：调用 `attach-new-file` 把它加入 `file-node` / `library-node` / `file-linkage`，然后按变更文件处理。
- **删除文件**：从 `file-node` 树、`library-node` 树、`file-linkage` 中移除，并把它依赖的文件标记为 `refreshable?`（因为它们失去了一个依赖）。

### 9.8 复杂度与风险

| 风险 | 说明 | 缓解 |
|------|------|------|
| Library header 变更 | 会改变 `file-linkage`，影响 `shrink-paths` 结果 | 在 `update-file-node-with-tail` 中处理，先重建 linkage 再计算批次 |
| 多文件变更重叠 | 多个变更文件的依赖者可能重复 | 收集 `refreshable-paths` 时去重 |
| Script 文件 | 不进入 linkage，需要单独批次 | 单独处理，不经过 `shrink-paths` |
| 循环依赖 | `shrink-paths` 已能处理 SCC | 复用现有逻辑 |
| 性能退化 | 如果很多文件变更，增量可能不如全量 | 可设阈值，变更文件超过一定比例时回退到 `refresh-workspace` |

### 9.9 验收标准

1. 修改 fixture 中一个不常被依赖的文件（如独立 script），再次加载缓存时只重新分析该文件，其他文件保持缓存结果。
2. 修改一个被多处 import 的 library，只重新分析该 library 及其依赖者。
3. 新增/删除文件后，workspace 结构与冷启动一致。
4. 缓存命中且无文件变化时，启动时间显著低于冷启动。

---

## 10. 相关文件

- `run.ss`：`--cache-path` 参数解析
- `scheme-langserver.sls`：`init-server` 透传 cache-path，退出时调用 `save-workspace-cache-for!`
- `analysis/workspace.sls`：`init-workspace` 集成加载/保存，`private:cache-consistency-check`
- `analysis/workspace-cache.sls`：序列化/反序列化核心，record 注册
- `analysis/dependency/file-linkage.sls`：`private:rebuild-file-linkage-path->id-map!`
- `tests/virtual-file-system/test-workspace-cache.sps`：缓存相关测试
- `bin/dump-workspace-cache.sps`：缓存内容调试工具
- `doc/workspace-cache.md`：序列化格式文档

---

## 11. 参考文档

- `ufo-persistence` 源码：`/home/ufo/Documents/workspace/uv-environment/ufo-persistence/`
- 序列化格式文档：`doc/workspace-cache.md`
