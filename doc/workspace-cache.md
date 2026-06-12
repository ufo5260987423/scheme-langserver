# Workspace Cache 序列化格式

scheme-langserver 使用 `ufo-persistence` 把整个 workspace 序列化到磁盘，以便下次启动时跳过解析、类型推断等耗时步骤。

## 1. 缓存文件位置

缓存由启动参数 `--cache-path <dir>` 指定：

```bash
./run --cache-path /path/to/cache
```

实际缓存文件只有一个：

```
<cache-path>/
└── workspace.bin
```

如果 `--cache-path` 未提供，则完全不启用缓存。

> 旧版本曾使用 `manifest.sexp` + `workspace.bin` 两个文件。新版本已合并为单个 `workspace.bin`，但保存时会自动删除遗留的 `manifest.sexp`。

## 2. 顶层 Wrapper

`workspace.bin` 是 `ufo-persistence` 生成的对象文件，顶层结构是一个 list：

```scheme
(cache-wrapper
  <manifest>
  <payload>)
```

- `cache-wrapper`：固定标签，用于区分旧格式（旧 `workspace.bin` 直接存 payload，没有 wrapper）。
- `<manifest>`：缓存的元数据信封。
- `<payload>`：真正的 workspace 快照（alist）。

加载时先 `restore-object` 整个 wrapper，校验 `cache-wrapper` 和 manifest，通过后再使用 payload。这样可以在不反序列化整个 workspace 图的情况下，快速判断缓存是否过期。

## 3. Manifest 格式

```scheme
(cache-manifest
  (format-version 1)
  (ufo-persistence-version 2)
  (langserver-version "v2.1.1-12-gabc1234")
  (facet txt)
  (top-environment r6rs)
  (created-at "2026-06-08T14:18:00"))
```

字段说明：

| 字段 | 含义 |
|------|------|
| `format-version` | 缓存文件结构版本，当前为 `1` |
| `ufo-persistence-version` | `ufo-persistence` 内部格式版本，当前为 `2` |
| `langserver-version` | scheme-langserver 的 git 版本，由 `git describe --tags --always --dirty` 得到，拿不到时为 `"unknown"` |
| `facet` | 文件过滤模式，如 `akku` / `txt` |
| `top-environment` | 顶层环境，如 `r6rs` / `r7rs` / `s7` |
| `created-at` | 缓存创建时间，ISO 8601 格式 |

### 缓存失效规则

只要以下任一条件不匹配，缓存即被视为无效，服务器会冷启动：

- `format-version` 不一致
- `ufo-persistence-version` 不一致
- `langserver-version` 不一致（防止代码升级后读到旧语义的数据）
- `facet` 不一致
- `top-environment` 不一致

> manifest 不记录文件内容哈希或文件列表。文件内容一致性在加载后通过比较缓存中的 `document-text` 与磁盘当前文本来判断（目前尚待实现增量刷新）。

## 4. Payload 格式

Payload 是一个 alist，不是直接序列化 `workspace` 记录本身（因为 workspace 的 `facet` 字段是过程，无法序列化）：

```scheme
((file-node . <root-file-node>)
 (library-node . <root-library-node>)
 (file-linkage . <file-linkage>)
 (threaded? . #f)
 (type-inference? . #f)
 (top-environment . r6rs)
 (undiagnosed-paths . ()))
```

### 保存前清理

序列化之前会对 workspace 做清理，避免把运行时状态写入磁盘：

1. 清空所有 `document-diagnoses`。
2. 清空 `workspace-undiagnosed-paths`。
3. 清空 `file-linkage` 的 `path->id-map`（`equal-hashtable` 无法被 `ufo-persistence` 解码），只保留 `id->path-map` 和矩阵。

### 加载后重建

从 payload 恢复 workspace 后：

1. 根据 `id->path-map` 重建 `path->id-map`。
2. 用 `rebuild-workspace-from-payload` 重新组装 `workspace` 记录，并重新生成 facet（基于项目根路径，而不是缓存路径）。

## 5. 缓存一致性检查

manifest 只能保证**缓存格式和版本**匹配，不能保证磁盘文件自缓存写入以来没有变化。因此加载 payload 后会做一次文件内容一致性检查：

1. 遍历 workspace 下的所有 `file-node`。
2. 对每个带有 `document` 的文件节点，读取磁盘当前文本。
3. 与缓存里的 `document-text` 做 `string=?` 比较。
4. 只要发现任一文件：
   - 不存在了，或
   - 内容与缓存不一致

   就认为缓存过期，直接调用 `refresh-workspace` 从磁盘重新构建整个 workspace 并重新分析。

> 这是一个简单但保守的策略：只要有一个文件变了，就全量刷新。未来可以改成只刷新变更文件及其依赖者，但目前优先保证正确性。

## 6. Record 类型注册

`ufo-persistence` 反序列化时需要知道每个 record 类型的 RTD 和构造函数。因此加载缓存前必须调用：

```scheme
(init-workspace-cache-registry!)
```

该函数会注册两类类型：

- Chez 内置类型：`source-file-descriptor`、`annotation`、`source`。
- scheme-langserver 自定义类型：`file-node`、`library-node`、`document`、`index-node`、`identifier-reference`、`file-linkage`。

## 6. 生命周期

```
启动
  │
  ▼
init-server ──► initialize
  │
  ▼
init-workspace
  │
  ├── cache-path 未设置 ──► 冷启动，不缓存
  │
  └── cache-path 已设置
        │
        ▼
    workspace-cache-available?
        │
        ├── 存在 workspace.bin ──► restore-object → 校验 wrapper/manifest
        │                              │
        │                              ├── 校验通过 ──► 重建 workspace
        │                              └── 校验失败 ──► 冷启动
        │
        └── 不存在 ──► 冷启动
        │
        ▼
   运行时处理请求 / 文件变更
        │
        ▼
   退出（收到 exit / 客户端断开 EOF）
        │
        ▼
   save-workspace-cache-for! ──► persist-object(workspace.bin)
```

要点：

- **只在启动时加载一次缓存**。
- **只在退出时保存一次缓存**。
- 运行中的 `didChangeWorkspaceFolders` 等动态变更不走缓存。

## 7. 代码入口

- `analysis/workspace-cache.sls`：序列化/反序列化核心，定义 wrapper、manifest、文件读写。
- `analysis/workspace.sls`：`init-workspace`、`save-workspace-cache-for!`、payload 组装与重建。
- `scheme-langserver.sls`：在 `exit` 通知和 EOF shutdown 时触发保存。
- `run.ss`：命令行参数解析，`--cache-path` 传入 `init-server`。

## 8. 调试

如果想查看缓存是否生效，可以检查日志或缓存目录：

```bash
# 查看缓存文件是否存在
ls <cache-path>/workspace.bin

# 简单验证 wrapper 结构（ Scheme 中）
(init-workspace-cache-registry!)
(restore-object "<cache-path>/workspace.bin")
;; 应返回 (cache-wrapper (cache-manifest ...) ((file-node . ...) ...))
```

> 注意：`restore-object` 必须在 `init-workspace-cache-registry!` 之后调用，否则会因缺少 RTD 注册而失败。
