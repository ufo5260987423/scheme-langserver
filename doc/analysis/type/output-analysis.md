# output-type-analysis

`bin/output-type-analysis.ss` 是一个命令行工具，用于批量导出 Scheme 项目中 library `(export ...)` 子句里各标识符的类型推断结果。

---

## 前置条件

运行前必须激活 Akku 环境，否则 Chez Scheme 找不到依赖库：

```bash
source .akku/bin/activate
```

---

## 两种运行模式

### Mode A：单库模式（3 参数）

只分析指定 library，输出该库所有 export 标识符的类型。

```bash
scheme --script bin/output-type-analysis.ss \
  <target-dir> \
  "<library-name>" \
  <output-file>
```

- `<target-dir>` — 项目根目录（会被 `init-workspace` 扫描）
- `<library-name>` — library 名称，用 S-expression 字符串形式，如 `"(fixtures simple-lib math)"`
- `<output-file>` — 输出文件路径

**示例**：

```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  "(fixtures simple-lib math)" \
  /tmp/math-types.txt
```

**示例输出**：

```
library:	( fixtures simple-lib math)
path:		tests/resources/workspace-fixtures/simple-lib/math.scm.txt
identifier:	add
type:		([identifier-reference number?] <- (inner:list? [identifier-reference number?] [identifier-reference number?] ) )
```

### Mode B：批量模式（2 参数）

遍历目录下**所有** library，输出每个 library 的 export 标识符类型。

```bash
scheme --script bin/output-type-analysis.ss \
  <target-dir> \
  <output-file>
```

**示例**：

```bash
scheme --script bin/output-type-analysis.ss \
  tests/resources/workspace-fixtures/simple-lib \
  /tmp/all-types.txt
```

---

## 输出格式

每个 file-node 产生一个输出块：

```
library:	(<library-name>)
path:		<absolute-path>
identifier:	<symbol>
type:		<type-expression>
...
```

- 同一 identifier 的多条 type 变体会被去重合并。
- 无 `(library ...)` 头的脚本文件不产生输出（不会输出 `library: ()`）。

---

## 自动文件过滤器

脚本会根据目标目录特征自动选择扫描范围：

| 条件 | 过滤器 | 扫描文件 |
|------|--------|---------|
| 目录下存在 `.akku` | `'akku` | `.sps` `.sls` `.scm` `.ss` |
| 目录下无 `.akku` | `'txt` | `.scm.txt` |

因此 fixture 目录（无 `.akku`）和项目自身源码（有 `.akku`）都可以直接运行，无需手动指定过滤器。

---

## 注意事项

1. **每次修改 `analysis/` 下的 `.sls` 源码后，若用此工具验证，建议先 `rm -rf .akku/libobj/scheme-langserver`，避免加载旧的 `.so` 缓存。**
2. 类型推断依赖 `init-workspace` 的完整分析流程（VFS → library-node → file-linkage → abstract interpreter），首次运行需要编译缓存，耗时较长（数十秒到数分钟）。
3. 批量模式在项目自身（~200 个 `.sls`）上运行可能需要数分钟，属正常现象。

## 4. 历史与实现细节

### 4.1 输出过滤策略的演变

`output-type-analysis.ss` 对原始推断结果的处理经历过三次变化：

**第一阶段（2026-06-18 之前）**：简单过滤 `"something? "`。

```scheme
(filter 
  (lambda (i) (not (equal? i "something? ")))
  ...)
```

这导致 `assq-ref` 被错误显示为返回 `boolean?`：因为 `assq-ref` 的真实返回 union 是 `{something?, boolean?}`，过滤掉 `something?` 后只剩下 `boolean?`。

**第二阶段（2026-06-18，commit `1c9ebfe9`）**：引入 `private:merge-something-union`。

只要结果中出现任意 `something?`，就把所有函数签名的返回值统一替换为 `something?`。这修复了 `assq-ref` 的 false positive，但也把 `contain?` 等真正返回 `boolean?` 的递归函数的精确签名压掉了。

**第三阶段（2026-06-20，commit `7691c08`）**：移除合并与过滤。

当前实现只保留 `dedupe`，直接输出推断层原始结果。`something?` 被视为"不够精确"而非"错误签名"。例如 `contain?` 会同时显示：

```text
type:		([identifier-reference boolean?] <- (inner:list? something? something? something? ) ) 
type:		(something? <- (inner:list? something? something? something? ) ) 
```

### 4.2 为什么使用 `type:interpret-result-list`

`output-type-analysis.ss` 原本可以调用 `type:recursive-interpret-result-list`（表达式级广度优先展开），但实验表明：

- 对非递归函数，输出没有明显改善，有时甚至退化（如 `type:solved?` 的 `boolean?` 重载被替换为 `something?`）。
- 对递归函数（`util/contain`、`util/binary-search`），会因未解决表达式集合爆炸而超时。

因此当前仍使用 `type:interpret-result-list`，配合 `PRIVATE-MAX-RESULTS = 500` 的结果数量预算，在合理时间内完成推断。

### 4.3 常见审查标准

对输出结果通常按以下维度判断：

| 维度 | 合理（✅） | 部分合理（⚠️） | 不合理（❌） | 无输出（➖） |
|------|------------|----------------|-------------|-------------|
| 是否有类型输出 | 有具体类型 | 有但含 `something?` | 有但明显错误 | 完全无输出 |
| 过程参数数量 | 与源码一致 | 部分一致 | 明显不一致 | — |
| 返回类型 | 与源码一致 | 过于保守 | 明显错误 | — |
| record 相关 | predicate/constructor/accessor 类型清晰 | 只有 `something?` | signature 明显错误 | — |

**明显错误的例子**：
- 已知返回 `boolean?` 的 predicate 被推断为 `number?`。
- 接受 2 个参数的函数被推断为 `(something? <- (inner:list? something?))`（1 个参数）。

### 4.4 用于评估的 6 个 library

为系统评估类型推断质量，常选取以下 6 个复杂度递增的 library：

| Library | 选择理由 |
|---------|----------|
| `(scheme-langserver util contain)` | 纯函数、递归 predicate |
| `(scheme-langserver util json)` | 字符串/列表操作、少量分支 |
| `(scheme-langserver virtual-file-system file-node)` | `define-record-type`、getter/setter |
| `(scheme-langserver analysis identifier reference)` | records、多字段、导出过程 |
| `(scheme-langserver analysis type domain-specific-language interpreter)` | 类型系统自身，自举测试 |
| `(scheme-langserver util binary-search)` | 已知表现差的递归/索引算术案例 |

完整评估结果见 [`benchmark.md`](benchmark.md) 和 [`evaluation-report.md`](evaluation-report.md) 。
