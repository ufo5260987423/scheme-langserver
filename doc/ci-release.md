# CI Release 流程与下游协作

本文档记录 `scheme-langserver` 的 GitHub Actions CI release 流程，以及产物如何被下游项目 [Magic Scheme](https://github.com/ufo5260987423/magic-scheme) 消费。

> 信息来源核对：
> - scheme-langserver CI: `.github/workflows/release.yaml`、`.github/workflows/manually-release.yaml`
> - Magic Scheme 源码: `src/download.ts`、`scripts/download-langserver.js`、`package.json`

---

## 1. CI Release 流程

### 1.1 触发条件

| Workflow | 文件 | 触发方式 |
|----------|------|----------|
| 自动 release | `.github/workflows/release.yaml` | `push` 到符合 `*.*.*` 格式的 git tag |
| 手动 release | `.github/workflows/manually-release.yaml` | `workflow_dispatch` 手动触发，生成 `auto-build-YYYY-MM-DD-HH-MM-SS` tag |

### 1.2 构建步骤

#### Step 1 — Checkout
`actions/checkout@v4`，`fetch-depth: 0`，确保 `git describe` 能读取完整 tag 历史。

#### Step 2 — Build Docker 镜像
构建两个镜像：

| 镜像 | Dockerfile | 基础系统 | 说明 |
|------|------------|----------|------|
| glibc | `Dockerfile` | `debian:bullseye` | 编译 Chez 10.4.1，打 `compile-chez-program` 补丁，akku 安装依赖 |
| musl | `Dockerfile.musl` | Alpine | 与 glibc 类似，使用 musl libc |

两个镜像都接收 `VERSION` build-arg，写入容器内的 `.version` 文件。

#### Step 3 — 编译静态二进制
分别在两个容器内执行：

```bash
source .akku/bin/activate
compile-chez-program run.ss --static
```

产物通过 host volume 挂载写入 `build/` 目录，并统一重命名为：

| 平台 | 产物文件名 |
|------|-----------|
| glibc | `scheme-langserver-x86_64-linux-glibc` |
| musl | `scheme-langserver-x86_64-linux-musl` |

文件名**不包含版本号段**，以便下游通过 GitHub permalink 稳定获取。

#### Step 4 — Release
使用 `softprops/action-gh-release@v2`：

- 自动 release 读取 git tag 作为 release tag
- 手动 release 生成时间戳 tag
- 上传两个 asset 到 GitHub Release
- `manually-release.yaml` 额外计算 MD5 / SHA256 写入 release body

### 1.3 产物与 permalink

每次 release 包含两个 asset，GitHub 提供 permalink：

```
https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-glibc
https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-musl
```

由于文件名稳定（无版本号），`latest/download/` 始终指向最新 release 中的对应文件。

---

## 2. 下游消费：Magic Scheme

[Magic Scheme](https://github.com/ufo5260987423/magic-scheme) 是 VSCode 扩展，为 Scheme 提供 LSP 客户端。它需要 `scheme-langserver` 可执行文件作为 LSP 服务端。

### 2.1 二进制查找优先级

Magic Scheme 启动时按以下优先级查找 `scheme-langserver`（`src/download.ts`）：

1. **用户配置的 `serverPath`** —— `magicScheme.scheme-langserver.serverPath`
2. **`$PATH` 中的 `scheme-langserver`** —— 调用 `--help` 验证可执行性
3. **工作区根目录的 `./run`** —— 常见于 scheme-langserver 开发自身
4. **之前自动下载的二进制** —— 位于 VS Code global storage（`~/.config/Code/User/globalStorage/.../scheme-langserver`）
5. **自动下载**（仅限 Linux x64）—— 从 GitHub Release permalink 拉取

### 2.2 自动下载实现

**下载 URL**（hardcoded 在 `src/download.ts`）：

```typescript
const DOWNLOAD_URL =
  'https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-glibc';
```

**保存位置**：VS Code 的 `globalStorageUri.fsPath`，文件名固定为 `scheme-langserver`。

**流程**：
- 扩展首次激活（`onLanguage:scheme`）时，如果前 4 个来源都找不到有效二进制，且 `autoDownload` 为 `true`、平台为 `linux && x64`，则弹出下载通知
- 使用 `fetch()` 下载，30 秒超时，支持用户取消
- 下载完成后 `chmod 755`，并尝试执行 `--help` 验证完整性
- 验证通过后，将路径写入 workspace 的 `magicScheme.scheme-langserver.serverPath` 配置

### 2.3 自动更新机制

**配置项**：`magicScheme.scheme-langserver.autoUpdate`

| 模式 | 行为 |
|------|------|
| `notify`（默认） | 每 24 小时检查一次；发现新版本时在状态栏提示，用户手动触发更新 |
| `auto` | 静默后台下载并替换旧二进制，完成后自动重启 LSP |
| `off` | 不检查更新 |

**版本检测方式**：
- 不使用 `api.github.com`（避免 rate limit）
- 对 `https://github.com/ufo5260987423/scheme-langserver/releases/latest` 发送 `HEAD` 请求，跟踪重定向
- 从最终 URL 的 `/tag/{version}` 提取版本号（例如 `2.1.1`）
- 与 global storage 中的 `scheme-langserver.version` 文件对比

**更新流程**：
1. 下载到临时文件 `scheme-langserver.new`
2. 验证可执行性
3. 原子替换旧文件（unlink + rename）
4. 写入新版本号到 `scheme-langserver.version`
5. 重启 LSP

### 2.4 测试脚本下载

Magic Scheme 的 E2E 测试需要真实二进制，通过 `npm run download-langserver` 执行：

```javascript
// scripts/download-langserver.js
const DOWNLOAD_URL = 'https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-glibc';
const DEST_FILE = path.join(__dirname, '../.vscode-test', 'scheme-langserver');
```

与生产代码使用同一 URL，只是保存路径不同。

### 2.5 平台支持矩阵

| 平台 | 自动下载 | 说明 |
|------|----------|------|
| Linux x64 (glibc) | ✅ | 默认自动下载 glibc 产物 |
| NixOS | ⚠️ | 自动下载可能因 glibc 不兼容而 SIGSEGV；推荐 `nix-shell -p akkuPackages.scheme-langserver` |
| macOS | ❌ | 无预构建二进制，需手动编译或 Nix 安装 |
| Windows | ❌ | 无预构建二进制，推荐 WSL2 |
| Linux ARM | ❌ | 无预构建二进制，需手动编译 |

---

## 3. 修改注意事项

如果调整 CI release 行为，必须同步评估对 Magic Scheme 的影响：

| 改动 | 影响范围 | 建议 |
|------|----------|------|
| 修改 glibc 产物文件名 | Magic Scheme 的 `DOWNLOAD_URL` 硬编码了文件名，必须同步修改 `src/download.ts` 和 `scripts/download-langserver.js` | 保持文件名不变是最安全的 |
| 增加/删除产物 | 不影响现有自动下载；如需 musl 自动下载，需在 Magic Scheme 增加平台选择逻辑 | 当前 Magic Scheme 只下载 glibc |
| 更换仓库/owner | permalink 域名和路径全部变化，必须同步修改 `DOWNLOAD_URL` 和 `LATEST_RELEASE_URL` | — |
| 更换 release tag 格式 | 不影响 permalink；但可能影响 Magic Scheme 的版本号正则提取（`/\/tag\/([^/]+)$/`) | 保持 tag 格式兼容 |
| 产物中增加版本号段（如 `scheme-langserver-2.1.1-x86_64-linux-glibc`） | `latest/download/` permalink 失效，Magic Scheme 必须改为通过 GitHub API 或 release 列表查找 | 强烈不建议 |

---

## 4. 相关文件速查

### scheme-langserver 侧

| 文件 | 作用 |
|------|------|
| `.github/workflows/release.yaml` | 自动 release workflow（tag push 触发） |
| `.github/workflows/manually-release.yaml` | 手动 release workflow（workflow_dispatch 触发） |
| `Dockerfile` | glibc 构建镜像 |
| `Dockerfile.musl` | musl 构建镜像 |
| `docker/compile-chez-program.patch.pl` | 补丁 `compile-chez-program` 以正确报告 linker 错误 |

### Magic Scheme 侧

| 文件 | 作用 |
|------|------|
| `src/download.ts` | 自动下载、版本检查、更新逻辑 |
| `scripts/download-langserver.js` | E2E 测试用的下载脚本 |
| `package.json` | 扩展配置定义（`autoDownload`、`autoUpdate`、`serverPath` 等） |
