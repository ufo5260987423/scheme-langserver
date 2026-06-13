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
