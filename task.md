# Debug Task：scheme-langserver MCP 初始化超时 + workspace cache 保存 hang

## 一、问题现象

1. 用 scheme-langserver MCP 初始化项目根目录 `/home/ufo/Documents/workspace/scheme-langserver` 时，Bridge 30 秒超时。
2. 手动冷启动耗时：
   - `--multi-thread disable --type-inference disable`：约 43 秒，初始化可完成。
   - `--multi-thread enable`：初始化阶段就 hang，超过 120 秒未完成。
3. cache 保存表现：
   - 单线程模式下 cache save 可以完成（约 35 秒写入 39MB FASL）。
   - 但 cache 加载在跨进程时会失败，错误为 `~s is not of type ~s`（file-linkage record）。

## 二、根因

**Chez Scheme 的 `fasl-write`/`fasl-read` 对 generative record type 不能跨进程复用。**

项目里所有被 cache 的 record type 原来都是 generative 的（没有 `(nongenerative <uid>)`）：
- `file-node`
- `library-node`
- `document`
- `index-node`
- `identifier-reference`
- `file-linkage`

每次 Chez 进程启动时，这些 record type 的 RTD/UID 都会变化。用进程 A 写的 FASL cache，在进程 B 读取时，记录能重建但类型不匹配，`file-linkage?` 等谓词返回 `#f`， accessor/setter 抛出 `~s is not of type ~s`。

## 三、修复

把上述 6 个 record type 改成 **nongenerative** 并赋予固定 UID：

```scheme
(define-record-type (file-node make-file-node file-node?)
  (nongenerative scheme-langserver-file-node)
  (fields ...))
```

改动文件：
- `virtual-file-system/file-node.sls`
- `virtual-file-system/library-node.sls`
- `virtual-file-system/document.sls`
- `virtual-file-system/index-node.sls`
- `analysis/identifier/reference.sls`
- `analysis/dependency/file-linkage.sls`

另外，`scheme-langserver.sls` 的 `private:save-workspace-cache-if-any` 保留了更详细的异常日志（写入 server log），方便以后排查。

## 四、验证结果

### 4.1 cache save 成功

```bash
cd /home/ufo/Documents/workspace/scheme-langserver
source .akku/bin/activate
python3 <<'PY'
# send initialize + exit
PY | scheme --script run.ss \
  --cache-path /tmp/scheme-langserver-main-cache \
  --multi-thread disable --type-inference disable --top-environment R6RS
```

结果：
- 耗时约 80 秒
- 生成 `/tmp/scheme-langserver-main-cache/workspace.fasl`，大小约 39MB

### 4.2 cache load 成功

使用 wrapper 脚本 `run-mcp-cache.sh` 启动服务器：

```bash
time ./run-mcp-cache.sh < /tmp/init-msg.json
```

结果：
- initialize response 在约 **2.7 秒** 发出
- 整个进程因 EOF 触发 exit handler 的 cache save，约 46 秒后退出

`.scheme-langserver.log` 摘录：

```
read-message
2026 6 26 11 50 37 ... {"jsonrpc": "2.0", "id": 1, "method": "initialize", ...}
send-message
2026 6 26 11 50 40 ... {"jsonrpc":"2.0","id":1,"result":{"capabilities":...}}
```

### 4.3 复现用 JSON

`/tmp/init-msg.json`：

```json
Content-Length: 234

{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"processId": null, "rootPath": "/home/ufo/Documents/workspace/scheme-langserver", "rootUri": "file:///home/ufo/Documents/workspace/scheme-langserver", "capabilities": {}}}
```

## 五、MCP 集成现状

- 已创建 wrapper 脚本：
  - `run-mcp-cache.sh`（shell 包装）
  - `run-mcp-cache.py`（Python 包装，备用）
- wrapper 手动测试通过，initialize response 在 3 秒内返回。
- **尚未在 MCP Bridge 里完整跑通**：
  - Bridge 的 30 秒超时可能仍会被 exit handler 的同步 cache save 阻塞（即使 initialize response 已发出）。
  - 此前调试时意外杀死了 MCP Bridge 进程，当前 Bridge 未运行，需要外部重启后才能进一步验证。

## 六、建议的下一步

1. **重启 MCP Bridge**，然后用 `lsp_initialize` 指定 `langserver_path` 为 `run-mcp-cache.sh` 再测一次。
2. 如果 Bridge 仍超时，考虑把 exit handler 的 cache save 改成：
   - 仅在 workspace 实际有变更时才 save；或
   - 用单独线程/进程异步 save，避免阻塞主线程响应 shutdown/exit。
3. **multi-thread enable 冷初始化 hang** 是另一个独立问题，需单独排查（可能与 `threaded-map`、全局状态或 macro expansion 有关）。

## 七、相关文件

- `scheme-langserver.sls` — exit handler、cache save 调用
- `analysis/workspace.sls` — `init-workspace`、`save-workspace-cache-for!`
- `analysis/workspace-cache.sls` — `save-workspace-cache!`、`private:save-fasl`
- `virtual-file-system/{file-node,library-node,document,index-node}.sls` — 4 个 nongenerative record type
- `analysis/identifier/reference.sls` — identifier-reference nongenerative
- `analysis/dependency/file-linkage.sls` — file-linkage nongenerative
- `run-mcp-cache.sh` / `run-mcp-cache.py` — MCP wrapper
