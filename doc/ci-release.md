# CI Release 流程与下游协作

本文档记录 `scheme-langserver` 的 GitHub Actions CI release 流程，以及产物如何被下游项目（Magic Scheme）消费。

## 1. CI Release 流程

### 触发条件

Release 由两个 workflow 驱动：

- **自动 release**（`.github/workflows/release.yaml`）：当 `main` 分支收到符合 `*.*.*` 格式的 git tag push 时触发。
- **手动 release**（`.github/workflows/manually-release.yaml`）：通过 `workflow_dispatch` 手动触发，生成 `auto-build-YYYY-MM-DD-HH-MM-SS` 格式的 tag。

### 构建步骤

#### Step 1: Checkout
使用 `actions/checkout@v4`，`fetch-depth: 0` 保证能读取完整 git 历史用于 `git describe`。

#### Step 2: Build Docker 镜像
在同一 job 中构建两个镜像：

1. **Linux glibc**（`Dockerfile`）
   - 基础镜像：`debian:bullseye`
   - 编译 Chez Scheme 10.4.1（含 `getlogin` stub 和 `compile-chez-program` 补丁）
   - 用 `akku` 安装项目依赖
   - 接收 `VERSION` build-arg，写入 `/root/scheme-langserver/.version`
   - 清理 `.akku/libobj/*.so` 避免 fasl 版本不匹配

2. **Linux musl**（`Dockerfile.musl`）
   - 基础镜像使用 Alpine（musl libc）
   - 其余流程与 glibc 镜像类似

#### Step 3: 编译可执行文件
在两个独立的 step 中，分别运行对应镜像并编译静态二进制：

```bash
compile-chez-program run.ss --static
```

产物重命名规则：

| 平台 | 文件名 |
|------|--------|
| glibc | `scheme-langserver-x86_64-linux-glibc` |
| musl | `scheme-langserver-x86_64-linux-musl` |

文件名**不包含版本号段**，以便下游通过 GitHub 的 `releases/latest/download/{filename}` permalink 稳定获取。

编译产物通过 host volume 挂载到 `build/` 目录。

#### Step 4: 生成 Release
使用 `softprops/action-gh-release@v2` 发布到 GitHub Release：

- 自动 release：读取 git tag 作为 release tag
- 手动 release：生成 `auto-build-...` 时间戳 tag
- 上传两个 asset 文件
- `manually-release.yaml` 额外计算 MD5 / SHA256 并写入 release body

### 产物清单

每次成功 release 后，GitHub Release 页面包含以下 asset：

```
scheme-langserver-x86_64-linux-glibc
scheme-langserver-x86_64-linux-musl
```

GitHub 提供 permalink，例如：
```
https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-glibc
```

## 2. 下游消费：Magic Scheme

[Magic Scheme](https://github.com/ufo5260987423/magic-scheme) 是一个 VSCode 扩展，为 Scheme 代码提供 LSP 客户端功能。它需要 `scheme-langserver` 的可执行文件作为 LSP 服务端。

### 协作关系

- **scheme-langserver** 负责：
  - 维护 LSP 服务端实现
  - 通过 CI 自动编译并发布 Linux x86_64 静态二进制（glibc 和 musl）
  - 保证产物文件名稳定（无版本号段），便于 permalink 下载

- **Magic Scheme** 负责：
  - 维护 VSCode 扩展客户端
  - 在扩展安装或首次激活时，自动（或引导用户）下载 `scheme-langserver-x86_64-linux-glibc`
  - 管理可执行文件的路径配置，并启动 LSP 进程

### 下载方式

Magic Scheme 通过 GitHub Release permalink 获取最新 glibc 产物：

```
https://github.com/ufo5260987423/scheme-langserver/releases/latest/download/scheme-langserver-x86_64-linux-glibc
```

由于文件名不包含版本号，`latest/download/`  permalink 始终指向最新 release 中的对应文件，不需要在 Magic Scheme 代码中维护版本映射。

### musl 产物的定位

`scheme-langserver-x86_64-linux-musl` 目前主要面向 Alpine Linux 或其他基于 musl 的环境。Magic Scheme 默认下载 glibc 版本；如果用户运行在 musl 系统上（如 Alpine WSL），可以手动切换为 musl 产物。

## 3. 修改注意事项

如果未来需要调整 CI release 的行为，请同步评估对 Magic Scheme 的影响：

| 改动 | 影响 |
|------|------|
| 修改产物文件名 | Magic Scheme 的下载 URL 需要同步更新 |
| 增加/删除产物 | Magic Scheme 可能需要增加平台选择逻辑 |
| 更换 release tag 格式 | 不影响 permalink，但可能影响版本解析逻辑 |
| 更换仓库或 owner | permalink 域名和路径全部变化，必须同步通知下游 |

## 4. 相关文件

- `.github/workflows/release.yaml` —— 自动 release workflow
- `.github/workflows/manually-release.yaml` —— 手动 release workflow
- `Dockerfile` —— glibc 构建镜像定义
- `Dockerfile.musl` —— musl 构建镜像定义
- `docker/compile-chez-program.patch.pl` —— 补丁 `compile-chez-program` 以正确报告 linker 错误
