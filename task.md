# Windows CI 构建任务书

## 背景

`scheme-langserver` 目前只通过 GitHub Actions 发布 Linux (glibc) 可执行文件。用户希望在 Windows 上也能获得原生 `.exe` 可执行文件。

## 核心方案（基于 `prepare-windows-build.sh`）

`prepare-windows-build.sh` 的设计非常巧妙：

- `compile-chez-program` 生成的 `run.generated.c` 只包含 Chez Scheme 虚拟机字节码，是**平台无关**的。
- 因此，**在 Linux 上生成 `.c` 文件，在 Windows 上只负责链接**，即可得到原生 Windows 可执行文件。

这个方案**完全绕开**了 Windows 构建的两个最大障碍：
1. **Akku 没有 Windows 原生版本** —— Akku 只在 Linux 上运行，安装好依赖即可。
2. **SRFI 库文件名中的冒号问题** —— Windows 不允许文件名包含冒号（如 `srfi/:37/args-fold`），但在 Linux 上编译 `.c` 文件时已经不涉及这些源文件。

## 分阶段 Workflow 设计

### 阶段 1：Linux Runner —— 生成平台无关的 C 文件

在 `ubuntu-latest` 上：
1. `actions/checkout@v4` 检出代码（需带完整 git 历史，用于生成版本号）
2. 安装 Akku（Linux amd64 预构建包）
3. `akku install` 安装所有 Scheme 依赖
4. `source .akku/bin/activate && compile-chez-program run.ss` 生成 `run.generated.c`
5. `git describe --tags --always --dirty > .version` 生成版本文件
6. 将 `run.generated.c` 和 `.version` 作为 artifact 上传

### 阶段 2：Windows Runner —— 链接成 `.exe`

在 `windows-latest` 上：
1. 下载阶段 1 的 artifact（`run.generated.c` + `.version`）
2. 下载并安装 Chez Scheme Windows 版（`ChezScheme10.4.1.exe`）
3. 激活 MSVC（`ilammy/msvc-dev-cmd@v1`，runner 自带 VS2022）
4. 克隆 `ufo5260987423/chez-exe` 并构建 Windows 版（获取 `console_main.obj` 和 `petite-chez.lib`）
5. 用 `cl.exe` 链接生成 `run.exe`（使用 `/MT` 静态 CRT，不依赖 VC++ Redistributable）
6. 将 `run.exe` 和 `.version` 作为 release artifact 上传

## 链接命令

```batch
cl /nologo /MT /Fe:run.exe run.generated.c console_main.obj rpcrt4.lib ole32.lib advapi32.lib User32.lib petite-chez.lib
```

使用 `/MT`（静态 CRT 链接），目标机器**不需要安装任何额外的 VC++ 运行时库**。

## 三个风险点及解释

### 风险点 1：Chez Scheme Windows 安装程序的静默安装

**问题**：GitHub Actions 的 Windows runner 是无人值守环境，无法点击安装向导。需要确认 `ChezScheme10.4.1.exe` 支持**静默安装**（silent install），以及正确的命令行参数。

**常见安装程序类型的静默参数**：
- **Inno Setup**（很多开源软件使用）：`/SILENT` 或 `/VERYSILENT`，安装目录用 `/DIR="C:\ChezScheme"`
- **NSIS**（另一种常见安装程序）：`/S`
- **自定义安装程序**：参数可能不同

**当前状态**：`ChezScheme10.4.1.exe` 的具体安装程序类型和静默参数**尚未验证**。如果静默安装失败，整个 workflow 会在这一步中断。

**验证方法**：在本地 Windows 或 GitHub Actions 测试环境中尝试：
```batch
ChezScheme10.4.1.exe /?
ChezScheme10.4.1.exe /S
ChezScheme10.4.1.exe /SILENT
```

**备选方案**：如果静默安装不可行，可以改用 **MSYS2 的 Chez Scheme 包**（`mingw-w64-x86_64-chez-scheme`），通过 `pacman` 命令行安装，完全不需要交互式安装向导。

---

### 风险点 2：`ufo5260987423/chez-exe` fork 的 Windows 构建

**问题**：`prepare-windows-build.sh` 中引用的 `chez-exe` 原始仓库是 `gwatt/chez-exe`，但本项目实际使用的是 `ufo5260987423/chez-exe` fork。需要确认这个 fork 是否包含**完整的 Windows 构建支持**。

**需要验证的内容**：
1. `gen-config.ss` 在 Windows 上是否能正确生成 `tools.ini`（从代码看，Windows 分支存在，生成 `tools.ini` 而非 `make.in`）
2. 仓库中是否有 `Makefile.win` 或类似的 nmake 构建文件
3. `nmake install` 在 Windows 上是否能正确编译出 `console_main.obj`、`gui_main.obj` 和 `petite-chez.lib`

**当前状态**：该 fork 的 Windows 构建流程**尚未在 CI 中验证过**。如果 `nmake` 构建失败，整个 workflow 会中断。

**验证方法**：在本地 Windows 或 GitHub Actions 测试环境中尝试：
```batch
git clone https://github.com/ufo5260987423/chez-exe.git
cd chez-exe
scheme --script gen-config.ss --bootpath C:\ChezScheme\lib\csv10.4.1\ta6nt
dir Makefile.win  REM 确认 Makefile.win 是否存在
nmake /f Makefile.win
```

**备选方案**：如果 fork 的 Windows 构建有问题，可以回退到原始仓库 `gwatt/chez-exe`，或者手动提取 `console_main.obj` 和 `petite-chez.lib`（如果 Chez Scheme 官方提供预构建的 Windows 库）。

---

### 风险点 3：静态链接 vs 动态链接（已解决）

**问题**：MSVC 的 `/MD`（动态 CRT）要求目标机器安装对应版本的 VC++ Redistributable，而 `/MT`（静态 CRT）则把所有运行时库打包进 `.exe`，无需额外依赖。

**决策**：使用 `/MT` 静态 CRT 链接。release 分发的 `.exe` 对终端用户最友好，开箱即用。

## 与其他 CI workflow 的关系

- `release.yaml`：在 `push: tags: "*.*.*"` 时触发，目前只构建 Linux glibc 版本。Windows 构建可以作为**新增 job** 加入同一个 workflow，也可以作为**独立的 workflow**（如 `windows-release.yaml`）。
- `manually-release.yaml`：手动触发，同样可以加入 Windows 构建 job。

## 建议的下一步行动

1. **验证风险点 1**：在测试环境中确认 `ChezScheme10.4.1.exe` 的静默安装参数
2. **验证风险点 2**：在测试环境中确认 `ufo5260987423/chez-exe` fork 能否在 Windows 上成功构建
3. **创建 workflow 文件**：基于验证结果编写 `.github/workflows/windows-build.yaml`
4. **测试 end-to-end**：从 tag push 到 Windows artifact 上传的完整流程
