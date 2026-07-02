# Unused Local Identifier 清理任务

> **状态**：待处理  
> **原则**：先记录，再逐个解决；未达成共识前不要批量改代码。

---

## 1. 背景

通过 scheme-langserver 自身的诊断能力扫描本项目（排除 `.akku/` 下的依赖），发现大量 `Unused local variable` 警告。这些变量大多是函数参数（parameter），在函数体内未被使用，但所在函数却在外部被频繁调用。

本文件记录扫描结果、分类和待办清单，方便逐个评估、修复或标注为有意保留。

---

## 2. 扫描方法

1. 使用 workspace cache 热启动 `init-workspace`。
2. 调用 `init-references` 重新分析所有文件，以填充 `usage-count` 和诊断。
3. 提取所有类型为 `'parameter` 且 `usage-count = 0` 的 identifier reference。
4. 对每个 unused parameter，向上回溯到所在函数名。
5. 统计该函数名在代码库中作为调用目标（application operator）出现的次数。
6. 排除 `.akku/` 目录。

---

## 3. 统计结果

- **未使用局部变量总数**：174 个
- **源码文件（非 `tests/`）**：155 个
- **测试文件**：19 个
- **有名函数且被调用的情况（去重后）**：124 个 unique 条目

---

## 4. TOP 20 重点关注

| 调用次数 | 文件 | 函数 | 未使用参数 | 备注 |
|----------|------|------|------------|------|
| ~41 | `protocol/analysis/request-queue.sls` | `request-queue-push` | `potential-request-processor` | API 占位参数，当前实现未使用 |
| ~13 | `analysis/identifier/rules/library-export.sls` | `match-clause` | `root-file-node` | `ufo-match` 宏生成的辅助函数 |
| ~13 | `analysis/identifier/rules/r7rs/define-library-import.sls` | `match-clause` | `root-file-node` | `ufo-match` 宏生成的辅助函数 |
| ~13 | `analysis/identifier/rules/r7rs/define-library-export.sls` | `match-clause` | `root-file-node` | `ufo-match` 宏生成的辅助函数 |
| ~12 | `virtual-file-system/index-node.sls` | `private` | `document` | ✅ 已删除，同步清理 8 个判断函数 |
| ~7 | `analysis/identifier/rules/load.sls` | `load-process` | `root-library-node` | 统一接口参数 |
| ~4 | `analysis/type/substitutions/generator.sls` | `establish-available-rules-from` | `current-document` | 统一接口参数 |
| ~4 | `analysis/type/substitutions/generator.sls` | `establish-available-rules-from` | `expanded+callee-list` | 统一接口参数 |
| ~4 | `analysis/identifier/self-defined-rules/router.sls` | `route&add` | `identifier-list` | 统一接口参数 |
| ~4 | `analysis/identifier/self-defined-rules/router.sls` | `route&add` | `current-document` | 统一接口参数 |
| ~3 | `analysis/util.sls` | `do-nothing` | `fuzzy` | 可能是测试/占位辅助函数 |
| ~3 | `analysis/type/substitutions/util.sls` | `do-nothing` | `fuzzy` | 可能是测试/占位辅助函数 |
| ~3 | `analysis/type/substitutions/rules/let.sls` | `let-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/do.sls` | `do-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/define.sls` | `define-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/begin.sls` | `begin-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/letrec.sls` | `letrec-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/record.sls` | `define-record-type-process` | `document` | 统一接口参数 |
| ~3 | `analysis/type/substitutions/rules/case-lambda.sls` | `case-lambda-process` | `document` | 统一接口参数 |
| ~3 | `analysis/identifier/rules/fluid-let.sls` | `fluid-let-parameter-process` | `exclude` | 统一接口参数 |

---

## 5. 分类说明

### 5.1 API 占位参数

**特征**：函数当前实现没用到该参数，但调用方每次都按要求传入，未来可能扩展。

**代表**：
- `protocol/analysis/request-queue.sls` 的 `request-queue-push` 中的 `potential-request-processor`。

**处理建议**：
- 若未来确实需要，保留并用 `_` 前缀或注释说明。
- 若永远不会使用，删除该参数并同步修改所有调用点。

### 5.2 统一接口参数

**特征**：大量 `*-process` / `*-parameter-process` 函数为了接口一致性，都带 `document`、`root-file-node`、`root-library-node` 等参数。某些具体实现没用到。

**代表**：
- `analysis/identifier/rules/*.sls` 中各种 `*-process` 的 `root-file-node` / `root-library-node`。
- `analysis/type/substitutions/rules/*.sls` 中各种 `*-process` 的 `document`。

**处理建议**：
- 不建议为了单个函数改动接口，否则调用点爆炸。
- 可考虑在函数体内用 `_` 重命名或加 `; unused` 注释，关闭诊断。
- 或者让诊断器对这种统一接口参数做白名单处理。

### 5.3 宏生成的辅助函数

**特征**：`ufo-match` 等宏会生成 `match-clause` 这类辅助函数，其参数来自宏模板，开发者不直接控制。

**代表**：
- `analysis/identifier/rules/library-export.sls` 的 `match-clause`。
- `analysis/identifier/rules/r7rs/define-library-import.sls` 的 `match-clause`。
- `analysis/identifier/rules/r7rs/define-library-export.sls` 的 `match-clause`。

**处理建议**：
- 这些不应由开发者手动修复。
- 应让诊断器识别宏生成的代码并跳过，或调整 `ufo-match` 宏模板减少无意义参数。

### 5.4 私有辅助函数

**特征**：函数本身私有，参数只是没被当前实现使用。

**代表**（已处理）：
- `virtual-file-system/index-node.sls` 的 `private` 中的 `document`。
- `analysis/tokenizer.sls` 的 `private:r7rs-fix-u8` 中的 `irritants`。
- `analysis/workspace.sls` 的 `private:init-workspace-from-scratch` 中的 `identifier`。
- `analysis/identifier/reference.sls` 的 `private-export-transform` 中的 `location-document`。

**处理建议**：
- 若函数稳定且调用点少，可删除未使用参数。
- 若保留，建议重命名为 `_document` 或加注释。

---

## 6. 完整列表

去重后的 124 个 unique 条目已保存到：

```text
/tmp/unused-with-calls-relative.log
```

格式：

```text
文件路径: 参数名 in 函数名 (called ~N times)
```

---

## 7. 待办清单（一个个解决）

- [x] **T1** - `protocol/analysis/request-queue.sls`：`request-queue-push` 的 `potential-request-processor`
  - **决定**：删除。
  - **原因**：`potential-request-processor` 在 `request-queue-push` 中完全未使用；实际处理请求的是 `request-queue-pop` 的 `request-processor`。
  - **已修改文件**：
    - `protocol/analysis/request-queue.sls`：删除参数
    - `scheme-langserver.sls`：3 处调用同步删除 `request-processor` 实参
    - `tests/protocol/analysis/test-request-queue.sps`：所有调用同步删除 `processor` 实参
    - `doc/protocol/diagnostic.md`：代码示例同步删除 `request-processor` 实参
  - **验证**：`test-request-queue.sps` 48 passes；`test-file-lifecycle.sps`、`test-document-symbol.sps`、`test-workspace-cache.sps`、`test-workspace.sps` 均通过；`(import (scheme-langserver))` 无错误。
  - **未 commit**。
- [x] **T1.5** - 清理 `virtual-file-system/index-node.sls` 中判断函数的 `document` 参数
  - **决定**：删除 `quote?` / `quasiquote?` / `unquote?` / `unquote-splicing?` / `syntax?` / `quasisyntax?` / `unsyntax?` / `unsyntax-splicing?` 以及内部 `private` 的 `document` 参数。
  - **原因**：这些判断函数只检查 `index-node` 的表达式头，不需要 `document`。
  - **已修改文件**：
    - `virtual-file-system/index-node.sls`：8 个判断函数 + `private` 删除 `document` 参数
    - `analysis/abstract-interpreter.sls`：同步 8 处调用
    - `analysis/type/substitutions/generator.sls`：同步 8 处调用
  - **验证**：`test-workspace.sps` 58 passes；`test-tokenizer.sps` 37 passes；`test-abstract-interpreter.sps` 2 passes；`test-generator.sps` 3 passes；`test-vfs.sps` 5 passes；相关 protocol/api 测试通过。
- [x] **T1.6** - 清理私有函数中的未使用参数
  - **决定**：删除。
  - **已修改文件**：
    - `analysis/tokenizer.sls`：`private:r7rs-fix-u8` 删除 `irritants` 参数
    - `analysis/workspace.sls`：`private:init-workspace-from-scratch` 删除 `identifier` 参数
    - `analysis/identifier/reference.sls`：`private-export-transform` 删除 `location-document` 参数
  - **验证**：`test-workspace.sps`、`test-tokenizer.sps`、`test-reference.sps`、`test-completion.sps` 均通过。
- [ ] **T2** - 评估 `ufo-match` 生成的 `match-clause` 参数
  - 是否应在诊断器中跳过宏生成代码？
  - 或修改 `ufo-match` 模板？
- [x] **T3** - `analysis/type/substitutions/rules/*.sls` 统一接口 `document` 参数
  - **决定**：保持现状，不清理。
  - **原因**：这些 `*-process` 函数被 `generator.sls` / `abstract-interpreter.sls` 统一调用，必须保持相同签名。删除个别未用参数会破坏调用一致性。
- [x] **T1.7** - 清理 `protocol/apis/completion.sls`：`sort-with-type-inferences` 的 `target-document`
  - **决定**：删除。
  - **原因**：函数内部完全未使用 `target-document`。
  - **已修改文件**：
    - `protocol/apis/completion.sls`：删除 `target-document` 参数，同步调用点
  - **验证**：`test-completion.sps` 通过。
- [x] **T1.8** - 清理 `analysis/type/substitutions/generator.sls`：`establish-available-rules-from` 的 `current-document`、`expanded+callee-list`
  - **决定**：删除。
  - **原因**：函数内部只使用 `identifier-list`，`current-document` 和 `expanded+callee-list` 是早期接口残留。
  - **已修改文件**：
    - `analysis/type/substitutions/generator.sls`：删除两个参数，同步调用点
  - **验证**：`test-generator.sps`、`test-abstract-interpreter.sps`、`test-workspace.sps` 均通过。
- [x] **T4** - `analysis/identifier/rules/*.sls` 统一接口 `root-file-node` / `root-library-node` 参数
  - **决定**：保持现状，不清理。
  - **原因**：与 T3 相同。这些 `*-process` 函数被 `abstract-interpreter.sls` 的 `step` 统一回调调用，签名必须一致。
- [ ] **T5** - 其余低频调用函数中的未使用参数
  - `analysis/identifier/reference.sls` 的 `library-identifier?` 中的 `document`：独立工具函数，可安全删除。
  - 其他逐个评估。
- [ ] **T5** - 其余低频调用函数中的未使用参数
  - 逐个评估是否安全删除。

---

## 8. 注意事项

- **不要批量改代码**。某些参数虽然当前未使用，但可能是为了接口统一或未来扩展。
- **优先处理高频调用函数**（如 `request-queue-push`），影响面最大。
- **宏生成的代码**应由宏或诊断器处理，不要手动修改。
- 每次修改后运行 `bash test.sh` 确保不破坏测试。
