# Task：修复 `init-index-node` 对 cyclic literal 的无限递归

## 问题描述

在分析 swish 项目（`/home/ufo/Documents/workspace/swish`）时，scheme-langserver 会出现内存持续增长、CPU 占满并最终挂起的现象。

根本原因是 swish 的 `src/swish/db.ss` 第 410 行包含一个自引用的 cyclic list literal：

```scheme
(define sleep-times '(2 3 6 11 16 21 26 26 26 51 51 . #0=(101 . #0#)))
```

`#0=(101 . #0#)` 创建了一个 `cdr` 指向自身的 pair。

## 根因定位

问题出在 `virtual-file-system/index-node.sls` 的 `init-index-node` 函数。

提交 `d9597c3` 为了保留 dotted pair 的 AST 结构，在 `init-index-node` 中新增了对 pair 的递归处理：

```scheme
[(pair? annotation-list)
  (map 
    (lambda (e) 
      (init-index-node node (if (annotation? e) e (private:pair->synthetic-annotation e))))
    `(,(car annotation-list) ,(cdr annotation-list)))]
```

当 `cdr annotation-list` 是一个自引用 pair 时，`init-index-node` 会不断把同一个 pair 包装成 synthetic annotation 并再次递归，形成无限递归，导致内存持续增长。

旧代码（`d3c22a9` 之前）对 non-annotation 的 pair 会 `filter annotation?` 过滤掉，因此不会触发这个问题。

## 最小复现

```scheme
(library (cyclic-test lib)
  (export sleep-times)
  (import (chezscheme))
  (define sleep-times '(1 2 3 . #0=(4 . #0#))))
```

执行：

```scheme
(let ([annotations (source-file->annotations path)])
  (for-each (lambda (ann) (init-index-node '() ann)) annotations))
```

- `d3c22a9`（旧 index-node）：正常结束
- `d9597c3` 及之后：挂起，内存持续增长

## 修复方案

### 核心原则

`index-node` 是 AST，AST 不应该表示 cyclic 结构。循环只存在于 Chez reader 输出的底层对象图中；建成后的 index-node 图必须是无环的树/DAG。

对于 `#0=(x . #0#)`：

- `#0=(x . #0#)` 作为一个源码出现位置，对应**定义节点**。
- `#0#` 作为另一个源码出现位置，对应**引用节点**。
- 引用节点是**叶子节点**，children 为空。
- 通过新增属性 `shared-reference` 标记引用关系。

### 目标

对于 `#0=(x . #0#)`：

- 生成 **2 个 index-node**：
  1. `#0=(x . #0#)` 对应的定义节点。
  2. `#0#` 对应的引用节点。
- 定义节点的 children 是 `[x-node, reference-node]`。
- 引用节点的 children 为空。
- 为 `index-node` 记录新增**不可变**属性 `shared-reference`：
  - 普通节点：`#f`
  - 引用节点：指向它所引用的定义节点

### 设计：在 `init-index-node` 内部检测 cycle 并复用 node

使用一个 `eq-hashtable`（记为 `compound->node`）跟踪“当前顶层 `init-index-node` 调用过程中，每个 compound 对象已经创建出的定义节点”。

流程：

1. 顶层 `init-index-node` 调用时创建一个新的 `eq-hashtable`。
2. 当 `init-index-node` 遇到一个 annotation `A` 时：
   - 令 `C = (annotation-expression A)`。
   - 如果 `C` 是 compound（pair/vector）且已在 `compound->node` 中：
     - 说明这是 `#0#` 这样的 cyclic reference。
     - 创建一个新的 index-node `R` 作为引用节点，datum = `A`，children = `'()`，`shared-reference` = `compound->node` 中对应的定义节点。
     - 返回 `R`。
   - 否则：
     - 创建定义节点 `D`，datum = `A`，`shared-reference` = `#f`。
     - 若 `C` 是 compound，把 `C -> D` 存入 `compound->node`。
     - 递归处理 children（pair 的 car/cdr、vector 的元素等）。
     - 设置 `D` 的 children。
     - 返回 `D`。
3. 对于 plain pair（如 Chez 为 `#0=(1 #0#)` 生成的 wrapper pair `Q`），同样用 `compound->node` 检测：若 `Q` 已存在则创建引用节点，否则创建定义节点并存入 `Q -> D_Q`。

### 结果示例

对于 swish 里的 cyclic literal：

```scheme
'(2 3 6 ... 51 51 . #0=(101 . #0#))
```

生成的 index-node 图如下：

```
'(2 3 6 ... 51 51 . #0=...)        [2 children]
├── 2                                [0 children]
└── synthetic-node for (3 6 ... )    [2 children]
    ├── 3                            [0 children]
    └── synthetic-node for (6 ... )  [2 children]
        ├── 6                        [0 children]
        └── ...                      [2 children]
            └── definition-node for #0=(101 . #0=)    [2 children]
                ├── 101                              [0 children]
                └── reference-node for #0#           [0 children, shared-reference -> definition-node]
```

关键点：

- `#0=(101 . #0#)` 对应一个**定义节点**，有两个 children：`101` 和 `#0#` 的引用节点。
- `#0#` 对应一个**引用节点**，children 为空，但 `shared-reference` 属性指向定义节点。
- 之前的 dotted pair 结构（非 cyclic 部分）保持不变。
- index-node 图整体无环。

### 引用节点的 datum

Chez reader 对 `#0#` 会生成独立的 annotation：

- 向量 `#0=#(1 #0#)`：第二个元素就是 `#0#` 的 annotation，source range 精确覆盖 `#0#`。
- 点对 `#0=(1 #0#)`：Chez 会生成 wrapper pair `Q = (A_ref . ())`，其中 `A_ref` 是 `#0#` 的 annotation。

因此引用节点可以直接用 `#0#` 对应的 annotation 作为 datum，source 信息是准确的，不需要 synthetic annotation 或特殊标记。

### 为什么 AST 可以无环

`annotation-expression` 和 `annotation-stripped` 的对象图是循环的（例如 `V[1] = V`），但 AST 节点按**源码出现位置**区分：

- `#0=#(1 #0#)` 是一个出现位置 → 定义节点
- `#0#` 是另一个出现位置 → 引用节点

同一底层对象的不同出现位置变成不同节点，引用位置用叶子表示，因此 AST 天然无环。

### 为什么不用 annotation-level 的 cycle detection？

Chez 没有暴露 `cyclic?` 之类的 predicate。要在 annotation 层面提前检测 cycle，必须自己完整遍历一遍 annotation tree，这和在 `init-index-node` 里一边建树一边检测做的是同一遍工作。因此把 cycle detection 放在 `init-index-node` 内部更自然、开销更小。

## `annotation-expression` / `annotation-stripped` 使用审计

引用节点的 `annotation-stripped` 返回的是它所引用的 compound 对象（如 pair/vector），而不是“当前节点自身是一个 form”。因此，凡是把 `annotation-stripped` 直接当“当前节点是什么语法形式”来判断的地方，都需要先排除引用节点。

需要调整的位置：

| 文件 | 行号/函数 | 问题 | 调整 |
|---|---|---|---|
| `virtual-file-system/index-node.sls` | `private`（被 `quote?` 等使用） | 引用节点若引用 `(quote ...)` 会被误判为 quote form | 先检查 `shared-reference`，非 `#f` 返回 `#f` |
| `analysis/workspace.sls` | import 子句检测 | 引用节点若引用以 `import` 开头的对象会被误判 | 先检查 `shared-reference` |
| `analysis/identifier/rules/s7/lambda*.sls` | `parameter*-process` | 引用节点若引用 pair 会被当成参数名 | 先检查 `shared-reference` |
| `analysis/identifier/self-defined-rules/srfi/include-resolve.sls` | library/include 检测 | 引用节点若引用 library form 会被误判 | 先检查 `shared-reference` |

绝大多数只通过 `index-node-children` 递归、或 children 为空就退出的函数不需要改动。

## 修改范围

### 主要修改

- `virtual-file-system/index-node.sls`
  - 给 `index-node` 记录新增 `shared-reference` 字段（immutable）。
  - 导出 `index-node-shared-reference`。
  - 更新 `make-index-node` 的 protocol，把 `shared-reference` 作为构造参数传入，并同步更新所有调用点。
  - 给 `init-index-node` 增加内部辅助函数，携带 `compound->node` hashtable。
  - 在 pair/vector 处理分支中：
    - 如果 compound 对象已存在 `compound->node` 中，创建引用节点（children 为空，`shared-reference` 作为构造参数传入定义节点）并返回。
    - 否则创建定义节点并存入 hashtable，然后递归 children。

### 次要修改

因为 index-node 图无环，**不需要**给 `find-leaves`、`clear-references-for`、`step` 等遍历函数加 visited set。

#### `analysis/type` 相关影响

`analysis/type/substitutions/generator.sls` 的 `step` 只对两类节点调规则：
1. 有 children 的节点（按 head 找对应 type rule）。
2. 叶子节点（调用 `trivial-process`）。

引用节点是叶子，因此只会进入 `trivial-process`。

`analysis/type/substitutions/rules/trivial.sls` 的 `trivial-process` 在叶子分支会递归处理 `annotation-stripped` 的 pair/vector 结构。如果不对引用节点做特殊处理，遇到 `#0=(1 . #0#)` 的引用节点时会因为 `cdr` 仍是同一个 pair 而无限递归。

因此需要：
- 在 2-arg `trivial-process` 开头检查 `(index-node-shared-reference index-node)`。
  - 若非 `#f`，表示这是一个引用节点。不递归展开 expression，而是生成类型约束：把 `(index-node-shared-reference index-node)` 加入当前 index-node 的 substitution list，表示“该引用节点的类型与它所引用的定义节点相同”。
  - 对于非循环的共享引用（如 `#0=(a b) #0#`），这个约束能正确复用定义节点的类型。
  - 对于循环引用（如 `#0=(1 . #0#)`），这个约束会形成递归类型方程；如果后续类型求解器无法处理递归类型，可以再退化为近似类型（如 `(inner:pair? <car-type> something?)`）。
- 同步更新该文件中 `make-index-node` 的调用点，传入 `#f` 作为 `shared-reference`。

其他 type rule（如 `lambda-process`、`define-process` 等）只处理有 children 的节点，不会收到引用节点，无需改动。`private:collect-param-types` 在引用节点上因 children 为空会自然返回 `'()`，也无需改动。

### 测试

- 新增 fixture：`tests/resources/workspace-fixtures/cyclic-literal/lib.scm.txt`，内容为 r6rs library，其中包含 `#0=(x . #0#)`。
- 新增测试 `tests/virtual-file-system/test-cyclic-literal.sps`，验证：
  - `init-index-node` 不挂起。
  - `#0=(x . #0#)` 生成一个定义节点和一个引用节点，定义节点 children 为 `[x-node, reference-node]`，引用节点 children 为空且 `shared-reference` 指向定义节点。
  - 引用节点的 source range 落在定义节点的 source range 之内。
- 回归测试：
  - `tests/analysis/test-workspace.sps` 仍然通过。
  - `rest-param-type` fixture 的测试仍然通过。
  - swish 项目完整 `bin/parallel-log-debug.sps` 不挂起。

## 验证结果

1. ✅ 最小复现文件验证 `init-index-node` 不挂起，生成的 node 结构符合预期（定义节点 2 children，引用节点 0 children，`shared-reference` 指向定义节点）。
2. ✅ `bash test.sh` 通过，无回归（exit code 0，无 unexpected failures）。
3. ✅ `bin/parallel-log-debug.sps` 在 120 秒内正常结束（exit code 0），未出现内存增长或挂起；`db.ss` 的 cyclic literal 被正确处理并产出 diagnostics。
4. ✅ `find-leaves`、`clear-references-for`、`debug:recursive-print-expression&uuid` 均通过 `index-node-children` 递归，引用节点无 children，不会死循环。

## 备注

此前分析中把问题归因于 `private:collect-param-types` 和 Chez 编译器优化是错误的。实际根因是 `init-index-node` 对 cyclic pair 的无限递归。`private:collect-param-types` 与此问题无关。
