# 在 VS Code 中用 Magic Scheme + scheme-langserver 支持 Fluent Scheme

本文介绍如何把 Ansys Fluent 的嵌入式 Scheme 开发体验搬进 VS Code：语法高亮、自动补全、
跳转定义、悬停文档、类型推断（早期阶段）。只需 VS Code + Magic Scheme 扩展 +
scheme-langserver，**不需要安装 Fluent 或 Chez Scheme**——语言服务做的是纯静态分析。

> 需要 scheme-langserver **≥ 2.1.10**：Fluent 顶层环境（`fluent` top environment）
> 从该版本起内置。

---

## 1. 背景：Fluent Scheme 是什么

Ansys Fluent 内嵌了一个 Scheme 解释器（基于 Petite Chez Scheme），广泛用于：

- **求解器参数读写**：`rp-var-define`、`%rpgetvar`、`rpsetvar`、`RP_Get_Real` 等；
- **自动化脚本 / journal**：批量设置、参数化计算；
- **Cortex 界面开发**：`cx-create-panel`、`cx-create-button`、`cx-create-real-entry`
  等 UI 面板 API。

这些 API 传统上没有任何 IDE 支持，写错函数名要到运行时才暴露。scheme-langserver 的
Fluent 顶层环境把常用 API 作为"内置标识符"建模，配合 Petite Chez 标准库，提供补全和
跳转。

## 2. 前置条件

| 组件 | 说明 |
|------|------|
| VS Code | 任意近期版本 |
| Magic Scheme 扩展 | 市场搜索 "Magic Scheme"（发布者 ufo5260987423） |
| scheme-langserver ≥ 2.1.10 | **Linux x64**：首次激活时 Magic Scheme 自动下载，无需手动安装；其他平台参考 [README](./README.md) 手动安装 |

## 3. 配置步骤

### 3.1 打开工作区

用 VS Code 打开存放 Fluent Scheme 脚本（`.scm`）的文件夹。语言服务以打开的文件夹为
工作区根目录，**跨文件跳转只覆盖根目录内的文件**。

### 3.2 设置顶层环境为 Fluent

两种方式任选：

**方式一：配置向导（推荐）**

1. `Ctrl+Shift+P`（macOS `Cmd+Shift+P`）→ 运行 **`Configure Magic Scheme Project`**；
2. 点击 `topEnvironment` 一行 → 选择 **Custom value...** → 输入 `Fluent`；
3. 点击 **✓ Done**，向导会生成 `.vscode/magic-scheme.json` 并自动重启语言服务。

**方式二：直接手写 `.vscode/magic-scheme.json`**

```json
{
  "topEnvironment": "Fluent",
  "multiThread": "enable",
  "typeInference": "enable",
  "logPath": ".vscode/scheme-langserver.log",
  "cachePath": ".vscode/scheme-langserver-cache"
}
```

保存后 Magic Scheme 自动重启语言服务生效。`topEnvironment` 的值最终传给服务器的
`--top-environment` 参数（大小写不敏感，`Fluent`/`fluent` 均可）。

### 3.3 文件关联

确保 VS Code 把 `.scm` 识别为 Scheme。工作区 `settings.json`：

```json
{
  "files.associations": {
    "*.scm": "scheme",
    "*.ss": "scheme"
  }
}
```

文件过滤无需额外配置：普通文件夹（没有 `.akku` 项目结构）默认接受所有标准 Scheme
扩展名（`.scm` `.ss` `.sls` `.sps` `.sld`）。

> **不要把 `.jou` journal 文件关联成 Scheme**。journal 是 Fluent 的命令记录格式，
> 不是 Scheme 源码，交给语言服务只会产生噪音诊断。

## 4. Fluent 环境内置了什么

`fluent` 顶层环境 = **38 个 Fluent 专有 API** + **全套 Chez Scheme 标识符**。

专有 API（节选，完整清单见 `analysis/identifier/meta.sls` 的 `fluent-raw`）：

| 类别 | 标识符 |
|------|--------|
| 求解器变量 | `rp-var-define`、`%rpgetvar`、`rpsetvar`、`make-new-rpvar`、`RP_Get_Real`、`RP_Get_String`、`RP_Set_Real`、`RP_Variable_Exists_P` |
| Cortex 面板 | `cx-create-panel`、`cx-create-button`、`cx-create-button-box`、`cx-create-drop-down-list`、`cx-create-integer-entry`、`cx-create-real-entry`、`cx-create-text-entry`、`cx-create-toggle-button`、`cx-create-table`、`cx-create-list`、`cx-create-taskpage`、`cx-show-panel`、`cx-show-taskpage` 等 |
| 面板控件读写 | `cx-set-integer-entry`、`cx-set-real-entry`、`cx-set-text-entry`、`cx-set-toggle-button`、`cx-set-list-items`、`cx-set-list-selections`、`cx-show-integer-entry`、`cx-show-list-selections` 等 |
| 菜单 | `cx-add-hitem` 等 |

效果：这些内置函数有补全、悬停和参数感知；你自己 `define` 的函数照常支持跳转定义
和查找引用。

## 5. 最小可用示例

```scheme
; 定义一个求解器参数
(rp-var-define 'my-swirl 0.5 'real #f)

; 做一个简单面板
(define (make-panel)
  (let ([panel (cx-create-panel "My Panel" "demo" #f #f)])
    (cx-create-real-entry panel "Swirl" 'my-swirl)
    (cx-create-button panel "Apply" 'apply-settings)
    (cx-show-panel panel)))

(define (apply-settings . args)
  (rpsetvar 'my-swirl (RP_Get_Real 'my-swirl)))
```

打开含此文件的文件夹并按第 3 节配置后：`rp-var-define`、`cx-create-panel` 等可直接
补全；跳转到 `apply-settings` 正常工作；`my-swirl` 等局部绑定有局部补全。

## 6. 已知边界与建议

- **API 覆盖是常用子集（38 个）**。例如 TUI 执行宏 `ti-menu-load-string` 不在其中
  ——不会报错，只是没有补全/悬停。两种应对：
  1. 脚本开头写桩声明让分析器认识它们，例如
     `(define ti-menu-load-string (lambda (cmd) #f))`（运行时由 Fluent 提供真实实现）；
  2. 向 scheme-langserver 提 PR 扩展 `analysis/identifier/meta.sls` 中的 `fluent-raw`
     表（格式：`(标识符 procedure)`）。
- **脚本文件 vs 库文件**：Fluent 脚本不是 `(library ...)` 形式，会作为脚本文件直接
  挂在工作区根下——这是正常行为，不影响分析。
- **大工作区冷启动**：Fluent 脚本项目通常很小（秒级初始化），`cachePath` 保持默认
  即可；若把语言服务用在大型混合工作区上，冷启动可能超过客户端超时，此时建议精简
  打开的文件夹范围。
- **`typeInference`**：对 Fluent 脚本可酌情 `disable`（省初始化时间，代价是失去
  悬停类型信息）。
- **多根工作区（multi-root）**：每个根文件夹使用各自的
  `.vscode/magic-scheme.json`。

## 7. 故障排查

| 现象 | 排查 |
|------|------|
| 状态栏一直初始化 | 首次冷启动在大工作区可达分钟级；看 `logPath` 日志确认在推进 |
| 补全没有 Fluent API | 检查 `.vscode/magic-scheme.json` 的 `topEnvironment` 是否为 `Fluent`；状态栏应显示 `scheme-langserver 2.1.10+` |
| 某些函数无补全 | 见第 6 节"已知边界"，该 API 可能不在 38 个内置中 |
| 服务器没起来 | `logPath` 指向的日志文件（默认 `.vscode/scheme-langserver.log`）；确认 scheme-langserver ≥ 2.1.10（`scheme-langserver --version`） |
| 诊断噪音多 | Fluent 脚本大量依赖运行时注入的全局绑定，属正常现象；关注真正的括号/语法错误诊断即可 |
