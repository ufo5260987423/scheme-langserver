# 任务：用 `output-type-analysis.ss` 验证类型推断正确性

## 1. 目标

使用 `bin/output-type-analysis.ss` 的单库模式，对 `scheme-langserver` 自身的若干 library 运行类型推断，检查输出结果是否符合源码实际类型，从而评估当前类型推断子系统的正确性、精确度和常见失效模式。

## 2. 测试库选取

按**复杂度递增**和**类型系统特性覆盖**两个维度，挑选 6 个 library：

| 序号 | Library | 选择理由 | 预期特点 |
|------|---------|----------|----------|
| 1 | `(scheme-langserver util contain)` | 纯函数、无递归、无 record | 类型应较准确，适合作为基线 |
| 2 | `(scheme-langserver util json)` | 字符串/列表操作、少量分支 | 测试 list/vector/string 推断 |
| 3 | `(scheme-langserver virtual-file-system file-node)` | 含 `define-record-type`、getter/setter | 测试 record 类型间接表示 |
| 4 | `(scheme-langserver analysis identifier reference)` | 含 records、多字段、导出过程 | 测试复杂 record + 过程签名 |
| 5 | `(scheme-langserver analysis type domain-specific-language interpreter)` | 类型系统自身 | 自举测试，观察递归/宏相关函数 |
| 6 | `(scheme-langserver util binary-search)` | 已知在类型推断中表现差（文档 §10） | 作为压力/边界案例 |

## 3. 执行步骤

### 3.1 环境准备

```bash
source .akku/bin/activate
rm -rf .akku/libobj/scheme-langserver
mkdir -p /tmp/type-analysis-results
```

### 3.2 单库运行

对每个选定的 library 执行：

```bash
scheme --script bin/output-type-analysis.ss \
  . \
  "(<library-name>)" \
  /tmp/type-analysis-results/<sanitized-library-name>.txt
```

例如：

```bash
scheme --script bin/output-type-analysis.ss \
  . \
  "(scheme-langserver util contain)" \
  /tmp/type-analysis-results/util-contain.txt
```

### 3.3 输出收集

所有结果保存到 `/tmp/type-analysis-results/`。同时记录：

- 每次运行的 wall-clock 时间
- 输出文件大小/行数
- 是否超时或报错

## 4. 审查标准

对每个 library 的每个 export 标识符，按以下维度判断：

| 维度 | 合理（✅） | 部分合理（⚠️） | 不合理（❌） | 无输出（➖） |
|------|------------|----------------|-------------|-------------|
| 是否有类型输出 | 有具体类型 | 有但含 `something?` | 有但明显错误 | 完全无输出 |
| 过程参数数量 | 与源码一致 | 部分一致 | 明显不一致 | — |
| 返回类型 | 与源码一致 | 过于保守 | 明显错误 | — |
| record 相关 | predicate/constructor/accessor 类型清晰 | 只有 `something?` | signature 明显错误 | — |

### 明显错误的例子

- 一个已知返回 `boolean?` 的 predicate 被推断为 `something?`
- 一个接受 2 个参数的函数被推断为 `(something? <- (inner:list? something?))`（1 个参数）
- 一个 record constructor 被推断为普通 `procedure` 而非返回 record predicate

## 5. 报告结构

最终报告将包含：

1. **执行摘要** — 运行时间、成功/失败的 library、总体结论。
2. **逐库结果表** — 每个 library 的 export 数量、有具体类型的数量、`something?` 占比、明显错误数量。
3. **典型案例分析** — 挑选 3–5 个代表性标识符，贴出源码与推断类型，逐条点评。
4. **发现的问题清单** — 按严重程度分类：
   - 严重：推断结果与源码语义冲突
   - 中等：结果过于保守但可用
   - 轻微：输出格式或细节问题
5. **改进建议** — 针对发现的问题，提出可落地的修复方向。

## 6. 风险与应对

| 风险 | 应对 |
|------|------|
| `binary-search` 等递归库运行极慢 | 设置 300 秒超时；若超时则记录并跳过后续递归压力库 |
| 某些 library 无 export（脚本文件） | 记录为 N/A，不纳入统计 |
| 输出量过大难以人工审查 | 先按 "有输出 / 全 something? / 无输出" 做粗筛，再重点抽查 |
| 缓存导致结果不反映最新代码 | 每次运行前清 `.akku/libobj/scheme-langserver` |

## 7. 时间预估

| 阶段 | 预估时间 |
|------|----------|
| 环境准备 + 试运行 1 个库 | 5–10 分钟 |
| 运行全部 6 个库 | 10–30 分钟（取决于 `binary-search` 等递归库） |
| 审查输出 + 整理报告 | 30–60 分钟 |
| **总计** | **约 1–1.5 小时** |

## 8. 交付物

- `/tmp/type-analysis-results/*.txt` — 原始推断输出
- `type-inference-evaluation-report.md` — 本次验证的总结报告

---

## 9. 进展更新与结论

> 更新日期：2026-06-18

### 9.1 已完成的修复

在正式执行 Step 3 前，已先修复了若干会严重影响输出正确性的基础 bug：

| 修复项 | 涉及文件 | 说明 |
|--------|----------|------|
| Bug 3 方案 A：Akku 文件过滤器 | `analysis/package-manager/akku.sls` | 移除错误的 `private:percent-decode`，使 percent-encoded 的 included `.scm` 文件（如 `%3a13/srfi-13.scm`）能正确进入 VFS。 |
| Bug 2：rest/dotted 参数 | `analysis/identifier/rules/define.sls`<br>`analysis/type/substitutions/rules/define.sls` | `define-process` 与类型规则均支持 `(define (f x . rest) ...)` 形式的函数。 |
| Bug 3：include 引用挂点 | `analysis/identifier/self-defined-rules/srfi/include-resolve.sls` | 被 include 文件的 references 现在挂到最近的 `(library ...)` / `(define-library ...)` 祖先节点，而不是 `SRFI-23-error->R6RS` 宏调用节点。 |
| Bug 1：输出工具 union 合并 | `bin/output-type-analysis.ss` | 当结果中存在 `something?` 时，合并更具体的 top-type 子类型，避免 `assq-ref` 等被错误显示为返回 `boolean?`。 |

相关测试已全部通过：

- `tests/analysis/identifier/rules/srfi/test-include-resolve.sps`
- `tests/analysis/type/substitutions/rules/test-define.sps`
- `tests/analysis/package-manager/test-akku.sps`
- `tests/virtual-file-system/test-vfs.sps`
- `tests/analysis/identifier/test-reference.sps`

### 9.2 关于 `uri-is-path?` 返回 `something?` 的结论

`uri-is-path?` 目前仍被推断为：

```text
(something? <- (inner:list? something? ) )
```

而不是 `boolean?`。经确认，这是**合理且可接受的当前限制**：

1. `uri-is-path?` 依赖 `(srfi :13 strings)` 的 `string-prefix?`；
2. `string-prefix?` 在 `srfi-13.scm` 中通过 `let-string-start+end2` 等宏实现；
3. **当前类型推断子系统未将宏展开引入类型推断**，因此无法从宏实现的 predicate 自动推导出 `boolean?`；
4. 这不同于 task.md 第 4 节列举的 "明显错误"：它不是推断器对普通源码分支的误推，而是宏抽象层造成的固有信息损失。

因此，**不会为 SRFI-13 添加显式类型签名**；`uri-is-path?` 返回 `something?` 视为当前架构下的已知限制，不在本次任务的可修复范围内。

### 9.3 Step 3 执行结果

已于 2026-06-18 逐个完成 6 个 library 的类型推断：

- 初始运行（无结果数量预算）时：
  - `util/json`、`virtual-file-system/file-node`、`analysis/identifier/reference`、`analysis/type/domain-specific-language/interpreter` 4 个库成功输出；
  - `util/contain`、`util/binary-search` 因递归函数类型推断结果无限膨胀而 300s 超时。
- 调试后，在 `analysis/type/domain-specific-language/interpreter.sls` 中增加**结果数量预算**：
  - `PRIVATE-MAX-DEPTH` 保持原值 `10`；
  - 新增 `PRIVATE-MAX-RESULTS`，在 `type:interpret` 单步结果去重后超过阈值时保留前若干项；
  - 经对比 100/200/500/1000 四个阈值（见 `type-inference-evaluation-report.md` 第 6 节），最终选定 **`PRIVATE-MAX-RESULTS = 500`**；
  - 6 个 library 全部能在合理时间内完成；
  - `util/contain`、`util/binary-search` 不再超时；
  - 简单 predicate（如 `meta?` → `boolean?`）的精确推断得以保留。
- 完整评估报告见 `type-inference-evaluation-report.md`。

### 9.4 下一步

根据 `type-inference-evaluation-report.md` 中的建议，优先处理：

1. **record accessor/setter 的字段类型精确化**：让 `record-accessor` / `record-mutator` 能从 `define-record-type` 字段定义中获取类型。
2. **类型推断阶段的 union 合并/简化**：减少复杂函数的签名数量。


---

## 10. 历史问题记录附录（原 bug.md）

> 以下内容来自原 bug.md，作为本次类型推断评估任务的背景参考。其中已修复的 bug 在第 9 节中有对应说明。

# 类型推断子系统问题记录（P0 阶段）

> 记录时间：2026-06-16
> 验证工具：`bin/output-type-analysis.ss` 单库模式
> 验证对象：`scheme-langserver` 自身 library

---

## Bug 1：`assq-ref` / `assoc-ref` / `assv-ref` 返回类型被错误推断为 `boolean?`

### 现象

对 `(scheme-langserver util association)` 运行类型推断导出：

```text
identifier:	assq-ref
type:		([identifier-reference boolean?] <- (inner:list? something? something? ) ) 
```

期望：返回类型应为 `something?`（因为 `cdr` 可能返回任意值，`#f` 也是 `something?` 的子类型）。

### 源码

```scheme
(define (assq-ref alist key)
  (let ((key-value-pair (assq key alist)))
    (if key-value-pair
        (if (pair? key-value-pair)
            (cdr key-value-pair)
            #f)
        #f)))
```

### 根因分析

1. **`if-process` 生成 union type**：
   - 内层 `if` 的 true 分支是 `(cdr key-value-pair)` → 类型 `something?`
   - 内层 `if` 的 false 分支是 `#f` → 类型 `boolean?`
   - 外层 `if` 的 false 分支也是 `#f` → 类型 `boolean?`

2. **`type:recursive-interpret-result-list` 过度展开**：
   - `output-type-analysis.ss` 调用 `type:recursive-interpret-result-list` 对函数体 index-node 进行迭代解释。
   - 展开后产生 7 个结果：6 个 `something?` + 1 个 `boolean?`。

3. **union 合并不足 + 工具过滤不当**：
   - 理论上 `something?` 与 `boolean?` 的并集应为 `something?`（top type）。
   - 但类型系统未做这个合并，保留了所有分支。
   - `output-type-analysis.ss` 过滤掉单个 `"something? "` 后，错误地保留了 `(boolean? <- ...)` 这个函数类型。

### 验证证据

直接解释 `assq-ref` body 节点的 `substitution-list`：

```text
body sub count: 1
body interpreted strings: [something?]
```

但 `type:interpret-result-list` / `type:recursive-interpret-result-list` 展开函数体后：

```text
count: 7
[(something? )]
[(something? )]
[(something? )]
[(something? )]
[(something? )]
[(something? )]
[([identifier-reference boolean?] )]
```

### 修改方案

**方案 A（推荐）：修复工具的结果过滤/合并策略**

修改 `bin/output-type-analysis.ss` 中的 `write-identifier-types!`：

- 对 `type:recursive-interpret-result-list` 的结果，先做一次 union 合并：如果结果集合中包含 `something?`，则丢弃其他更具体的 top-type 子类型（如 `boolean?`、`number?` 等），因为 `something?` 已经覆盖它们。
- 或者改为优先使用 `type:interpret-result-list` 直接解释 substitution-list，而不是 `type:recursive-interpret-result-list` 迭代展开。

**方案 B：修复类型系统的 union 合并**

在 `analysis/type/domain-specific-language/interpreter.sls` 中，当 `type:interpret` 处理 `inner:list?` / union 时，检查结果集合：

- 如果结果中包含 `something?`，则把结果归并为 `(something?)`。
- 这会影响更广泛的类型推断行为，需要谨慎测试。

**方案 C：修复 `if-process` 的分支处理**

让 `if-process` 在 attach 分支时，对已知 top-type 分支进行剪枝：

- 如果一个分支是 `something?`，另一个分支是更具体的类型，只保留 `something?`。
- 这会减少 union 爆炸，但也可能丢失信息。

### 建议

优先采用**方案 A**，因为：
- 风险最低，只影响 `output-type-analysis.ss` 这个调试/报告工具。
- 不改变核心类型推断语义，不会引入回归。
- 可以快速验证效果。

---

## Bug 2：`make-alist` 被推断为 `(inner:list? something? ...)` 而不是函数类型

### 现象

对 `(scheme-langserver util association)` 运行类型推断导出：

```text
identifier:	make-alist
type:		(inner:list? something? ... ) 
```

期望：`make-alist` 是一个函数，类型应形如 `((inner:list? (inner:pair? something? something?) ...) <- (inner:list? something? ...))`。

### 源码

```scheme
(define (make-alist . args)
  (let loop ([index 0])
    (if (< index (- (length args) 2))
        `(,(cons (list-ref args index) (list-ref args (+ 1 index))) . ,(loop (+ 2 index)))
        (list (cons (list-ref args index) (list-ref args (+ 1 index)))))))
```

### 根因分析

`analysis/type/substitutions/rules/define.sls` 中的 `define-process` 只匹配普通参数列表：

```scheme
[(_ ((? symbol? identifiers) (? symbol? parameters) ... ) tail) ...]
```

该模式要求 `(cadr expression)` 是 `(identifiers parameters ...)` 形式的**正规列表**。

但 `(define (make-alist . args) ...)` 的 signature 是 `(make-alist . args)`，这是一个 **dotted pair**，不匹配上述模式。因此 `define-process` 没有为 `make-alist` 标识符 attach lambda 函数类型。

验证（`make-alist` index-node）：

```text
substitution-list length: 7
substitutions mentioning <- or boolean: (none)
direct interpret-result-list length: 3
results:
  <index-node cycle>
  (with ((a b c)) ((with ((x d0)) d0) . c))
  (inner:list? something? ...)
```

没有任何 `<-` 形式的函数签名。

### 修改方案

**方案 A（推荐）：扩展 `define-process` 支持 rest 参数**

在 `analysis/type/substitutions/rules/define.sls` 中增加一个匹配 dotted parameter list 的分支：

```scheme
[(_ ((? symbol? identifier) . (? symbol? rest-parameter)) tail)
  (let* ([identifier-index-node (car (index-node-children (cadr (index-node-children index-node))))]
         [tail-index-node (car (reverse (index-node-children index-node)))]
         ; rest parameter 应表示为 (inner:list? something? ...)
         [rest-param-type `(inner:list? something? ...)]
         [lambda-details (construct-lambdas-with (list tail-index-node) rest-param-type)])
    (for-each
      (lambda (t)
        (extend-index-node-substitution-list identifier-index-node t))
      lambda-details))]
```

需要确认 `construct-lambdas-with` 是否能接受非 index-node 的 param 类型。如果不能，需要先构造一个虚拟 index-node 或使用现有工具函数。

**方案 B：在 `lambda-process` 中统一处理 rest 参数**

如果 `lambda-process` 已经支持 `(lambda (x . rest) ...)`，可以让 `define-process` 检测到 dotted pair 时，把函数重写为等价的 lambda 形式，然后复用 `lambda-process` 的逻辑。

**方案 C：保守处理**

对 dotted parameter list 的函数，直接赋予 `(something? <- (inner:list? something? ...))` 类型。这会丢失返回类型精度，但至少能正确识别为函数。

### 建议

优先采用**方案 A**，因为：
- `make-alist` 这种变长参数函数在 Scheme 中很常见。
- 修复后返回类型可以是 `(inner:list? (inner:pair? something? something?) ...) <- (inner:list? something? ...)`，精度较高。
- 需要测试 `construct-lambdas-with` 对 rest 参数的支持。

---

## Bug 3：`uri-is-path?` 返回 `something?` 而不是 `boolean?`

### 现象

对 `(scheme-langserver util path)` 运行类型推断导出：

```text
identifier:	uri-is-path?
type:		(something? <- (inner:list? something? ) ) 
```

期望：`(boolean? <- (inner:list? string?))`。

### 源码

```scheme
(define (uri-is-path? str)
  (string-prefix? str "file://"))
```

### 根因分析（已更新）

#### 3.1 直接原因：`string-prefix?` 没有任何类型签名

`string-prefix?` 来自 `(only (srfi :13 strings) string-prefix?)`。在 `util/path.sls` 的 `document-ordered-reference-list` 中**找不到** `string-prefix?` 的 reference；在 `(srfi :13 strings)` 文件的 `document-ordered-reference-list` 中也**找不到** `string-prefix?`。因此：

- `trivial-process` 无法通过 `identifier-reference-type` 判断它是 `predicator`。
- `string-prefix?` 被当作无签名的外部标识符处理，调用点返回 `something?`。

#### 3.2 根本原因：Akku 文件过滤器误排除 percent-encoded 的 included `.scm` 文件

`(srfi :13 strings)` 本身只 export 了一串标识符，实际实现通过 `include/resolve` 引入：

```scheme
(SRFI-23-error->R6RS "(library (srfi :13 strings))"
 (include/resolve ("srfi" "%3a13") "srfi-13.scm"))
```

被 include 的文件是 `.akku/lib/srfi/%3a13/srfi-13.scm`，其目录名是 **percent-encoded**（`%3a13`）。

`analysis/package-manager/akku.sls` 的 `generate-akku-acceptable-file-filter` 读取 `.akku/list` 时，对路径做了 `private:percent-decode`：

```scheme
(hashtable-set! path->library (string-append root (private:percent-decode target-path)) target-library)
```

`.akku/list` 中对应行是：

```text
.akku/lib/srfi/%3a13/srfi-13.scm	chez-srfi	included-file	
```

decode 后 hashtable key 变成 `.akku/lib/srfi/:13/srfi-13.scm`，而文件系统真实路径是 `.akku/lib/srfi/%3a13/srfi-13.scm`。两者不匹配，filter 对该文件返回 `#f`，`srfi-13.scm` 被排除在 VFS 之外。

#### 3.3 历史根源：`8aaab25` 是一次过度修复

`git log` 显示 `analysis/package-manager/akku.sls` 最近一次修改是提交 `8aaab25`：

```text
8aaab25 fix(analysis): decode percent-encoded paths in akku package filter
```

该提交添加了 `private:percent-decode`，commit message 认为：

> `.akku/list encodes characters like ':' as '%3a', but the actual filesystem directories use ':' (e.g. srfi/:152/...). The filter's hashtable therefore failed to match many akku dependency files...`

这个判断基于一个**错误假设**：所有 `.akku/list` 路径都是 percent-encoded，而文件系统都是 decoded。

实际上，当前 Akku（1.1.0-unstable-2025-11-08）的行为是**混合**的：

- **R6RS library wrapper**（如 `:13/strings.chezscheme.sls`）使用 **decoded** 路径，因为 `library-name->file-name/chezscheme` 不对 `:` 编码。
- **included files**（如 `srfi-13.scm`）使用 **percent-encoded** 路径 `%3a13/`，因为它们来自 `include/resolve ("srfi" "%3a13") ...` 的字符串参数。
- 其他实现（ikarus、ypsilon 等）的 wrapper 也使用 percent-encoded 路径。

因此 `.akku/lib/srfi/` 下同时存在：

```text
:13/strings.chezscheme.sls          <- decoded，library wrapper
%3a13/srfi-13.scm                   <- encoded，included implementation
%3a13/strings.ikarus.sls -> ../:13/strings.chezscheme.sls   <- encoded symlink
```

`.akku/list` 也如实反映了这种混合：

```text
.akku/lib/srfi/:13/strings.chezscheme.sls	chez-srfi	r6rs-library
.akku/lib/srfi/%3a13/srfi-13.scm	chez-srfi	included-file
```

所以 `.akku/list` 中的路径**已经是文件系统的真实路径**，不需要 decode。`8aaab25` 的 percent-decode 把 `%3a13/srfi-13.scm` 错误地映射到不存在的 `:13/srfi-13.scm`，导致所有 percent-encoded included files 被排除。

在 `8aaab25` 之前，`akku.sls` 直接使用 `target-path` 作为 hashtable key（无 decode），那才是正确行为。

#### 3.4 验证

用原始 filter 扫描 `.akku/lib/srfi/%3a13`：

```text
filter on scm file: rejected
walk-file result null? #t
```

用“不对 `.akku/list` 路径做 percent-decode”的 filter：

```text
fixed filter on scm file: accepted
walk-file result null? #f
document index-node-list null? #f
```

#### 3.5 现有测试的误导

`tests/analysis/package-manager/test-akku.sps` 中 `8aaab25` 添加的测试：

```scheme
(test-equal #t (checker (string-append (current-directory) "/.akku/lib/srfi/:152/r7rs-shim.scm")))
```

这个测试检查 decoded 路径 `:152/r7rs-shim.scm` 被接受。但文件系统上真实文件是 `%3a152/r7rs-shim.scm`，`:152/r7rs-shim.scm` **不存在**。该测试通过只是因为 percent-decode 把 `.akku/list` 中的 `%3a152/r7rs-shim.scm` 映射到了 `:152/r7rs-shim.scm`。

同样，`tests/virtual-file-system/test-vfs.sps` 中有针对 `srfi-13.scm` 的测试：

```scheme
(let* ([target-path (string-append (current-directory) "/.akku/lib/srfi/%3a13")]
    [root-file-node (init-virtual-file-system target-path '() (generate-akku-acceptable-file-filter ...))]
    [target-file-node (walk-file root-file-node (string-append target-path "/srfi-13.scm"))])
  (test-equal #f (null? (document-index-node-list (file-node-document target-file-node)))))
```

该测试目前显示 pass，但属于**假阳性**：

- `target-file-node` 实际是 `'()`（walk-file 找不到文件）。
- `(file-node-document '())` 会抛出异常。
- SRFI-64 的 `test-equal` 内部用 `(guard (ex (else #F)) expr)` 捕获异常，使 actual-value 变为 `#f`。
- 期望也是 `#f`，因此巧合通过。

### 修改方案

**方案 A（推荐，根本原因）：恢复 `generate-akku-acceptable-file-filter` 直接使用 `.akku/list` 原始路径**

移除 `8aaab25` 引入的 `private:percent-decode`，直接用 `.akku/list` 中的原始路径作为 hashtable key：

```scheme
(hashtable-set! path->library (string-append root target-path) target-library)
```

理由：
- Akku 生成的 `.akku/list` 已经正确反映了文件系统实际路径。
- 混合 encoded/decoded 是 Akku 为不同实现安装文件的**正常行为**，不是错误。
- percent-decode 会把真实存在的 `%3a13/srfi-13.scm` 错误映射到不存在的 `:13/srfi-13.scm`。

修复后 `srfi-13.scm` 会进入 VFS，`include-resolve-process` 会把它里面的 references 合并到 `(srfi :13 strings)` 的 ordered references 中。

**方案 A'（兼容性折中）：同时维护 encoded / decoded 两套 key**

如果担心某些旧版 Akku 或特定平台生成全部 encoded 的 `.akku/list`，可以同时设置两个 key：

```scheme
(let ((full-path (string-append root target-path)))
  (hashtable-set! path->library full-path target-library)
  (hashtable-set! path->library (private:percent-decode full-path) target-library))
```

但这可能把真实文件和 ghost 路径混在一起，不如方案 A 清晰。优先尝试方案 A，如果测试发现兼容问题再退回 A'。

**方案 B：扩展 `define-process` 支持 rest 参数（与 Bug 2 共用）**

`srfi-13.scm` 里的 `string-prefix?` 定义也是 rest 参数：

```scheme
(define (string-prefix? s1 s2 . maybe-starts+ends) ...)
```

即使文件被正确包含，如果 Bug 2 没修，`string-prefix?` 仍无法得到精确的函数类型签名。因此 Bug 2 的修复对 Bug 3 有直接影响。

**方案 C：为 `(srfi :13 strings)` 添加显式类型签名**

在 `analysis/type/substitutions/rnrs-meta-rules.sls` 或新建 `srfi-meta-rules.sls` 中添加：

```scheme
(string-prefix? (boolean? <- (inner:list? string? string?)))
```

然后在 `analysis/identifier/meta.sls` 的 `init-type-expressions` 中加载这些签名。

**方案 D：谓词函数默认推断启发式**

当遇到没有类型签名的 `procedure` 时，如果其 identifier name 以 `?` 结尾，默认推断为 `(boolean? <- (inner:list? something? ...))`。

### 建议

优先采用**方案 A + 方案 B + 方案 C**：
- **方案 A** 是根本修复，解决所有通过 `include/resolve` 引入的 SRFI 文件被误排除的问题。
- **方案 B** 让 rest 参数函数（包括 `string-prefix?`、`make-alist` 等）得到正确函数类型。
- **方案 C** 为常用 SRFI-13 函数提供精确签名，避免依赖源码推导的不确定性。

方案 D 可作为兜底启发式，但不精确。

---

## 修复优先级建议（更新）

| 优先级 | Bug | 理由 |
|--------|-----|------|
| P0 | Bug 3 方案 A：修复 Akku filter 路径解码 | 根本原因，影响所有 percent-encoded include 文件，修复后 Bug 3 大部分自动解决 |
| P0 | Bug 2：`define-process` 支持 rest 参数 | 明确的模式匹配 bug，同时影响 `make-alist` 和 `string-prefix?` 等函数 |
| P1 | Bug 1：`assq-ref` union 合并 / 工具过滤 | 影响多个函数，但优先修复工具过滤策略风险更低 |
| P2 | Bug 3 方案 C：补充 SRFI-13 精确签名 | 工作量较大但收益广，可作为兜底精度提升 |

---

## 下一步行动建议

1. 修复 **Bug 3 方案 A**（`analysis/package-manager/akku.sls` 移除 `private:percent-decode`），并修正相关测试：
   - `tests/analysis/package-manager/test-akku.sps`：把 `:152/r7rs-shim.scm` 改为 `%3a152/r7rs-shim.scm`（真实文件路径）。
   - `tests/virtual-file-system/test-vfs.sps`：把 `test-equal #f (null? (document-index-node-list ...))` 改为先 `test-assert` 确保 `target-file-node` 非空，再检查 index-node-list。
2. 修复 **Bug 2**（`analysis/type/substitutions/rules/define.sls` 支持 dotted/rest 参数）。
3. 验证 `(srfi :13 strings)` 中 `string-prefix?` 是否被正确分析为函数，再验证 `util/path` 中 `uri-is-path?` 是否返回 `boolean?`。
4. 修复 **Bug 1 方案 A**（`bin/output-type-analysis.ss` 的结果过滤/合并策略），验证 `assq-ref` 输出恢复正常。
5. 可选：补充 **SRFI-13 精确签名**（方案 C），提升签名精度。
