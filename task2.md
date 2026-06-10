# Task 2: 预处理坏括号删除方案

## 核心思路

在 tokenizer 运行前，对源码做一次预处理扫描，一次性算出"哪些括号是坏的"。**删掉这些坏括号，`get-datum` 就不会报括号相关的 condition。**

然后：
1. 把坏括号替换为空格（保留字符偏移）
2. 在清洗后的源码上运行 `get-datum`
3. 每个被替换的坏括号位置生成一条 diagnose

统一的 diagnose 语义：**"这个括号不该出现在这里，删掉它就能消除一个错误。"**

## 算法：栈 + 最近同类型匹配

从左到右扫描源码，跳过字符串、注释、字符字面量。只处理裸括号 `(` `)` `[` `]`。

维护一个 opener 栈，记录 `(position . char-type)`。

### 遇到 opener（`(` 或 `[`）

入栈。

### 遇到 closer（`)` 或 `]`）

从栈顶往下，找最近的同类型 opener。

**情况 A：找到同类型 opener**

- 弹出该 opener 和它上面的所有 opener
- **它上面的异型 opener 全部标记为"坏"**
  - 原因：这些 opener 挡在同型配对中间，在清洗后的源码中仍会导致 `get-datum` 报错

**情况 B：栈非空，但找不到同类型 opener（全是异型）**

- 当前 closer 标记为"坏"
  - 原因：这个 closer 找不到能匹配它的 opener，删掉它最省事

**情况 C：栈为空**

- 当前 closer 标记为"坏"
  - 原因：unexpected close，前面没有任何 opener

### EOF（扫描结束）

栈中剩余的 opener 全部标记为"坏"。

### 输出

返回"坏括号位置集合"——一个需要删除的位置列表。

## 与当前方案的区别

| 维度 | 当前方案 | Task 2 方案 |
|------|---------|-------------|
| **时机** | `get-datum` 报错后 patch，逐步修复 | **预处理一次性计算**，清洗后再跑 `get-datum` |
| **状态** | 原始源码和 patch 后源码交替，位置易漂移 | **只在清洗后源码上运行 `get-datum`** |
| **unclosed 来源** | `private:find-unclosed-parens-human-view` 在原始源码上独立扫描 | **扫描过程本身决定哪些 opener 是坏括号** |
| **diagnose 语义** | cross-mismatch + unclosed 两种类型 | **统一为"坏括号"** |
| **效率** | `get-datum` 可能反复 inner 递归 | **`get-datum` 只需一次**（或极少数 fallback） |

## 相关算法参考

### LeetCode 1249: Minimum Remove to Make Valid Parentheses

这是经典的"最小删除使括号有效"问题。给定一个只包含 `(` `)` 和小写字母的字符串，删除最少数量的括号使其合法。

标准解法是 **Two-Pass Greedy**：
1. **第一遍（左到右）**：用栈或计数器跟踪未匹配的 `(`。遇到 `)` 时如果计数器为 0，说明没有匹配的 `(`，删掉这个 `)`
2. **第二遍（右到左）**：处理多余的 `(`。从末尾往前扫，删掉没有 `)` 匹配的 `(`

时间复杂度 O(n)，空间复杂度 O(n)。

**我们的区别**：LeetCode 1249 只处理单类型括号 `()`。我们的场景有 `()` 和 `[]` 两种类型，且需要处理 cross-mismatch（`]` 遇到栈顶 `(` 等）。经典 two-pass 算法在遇到异型括号时会直接判定当前 closer 无效并删除；而我们的算法会**跳过异型 opener 继续找同型**，这是核心差异。

### StackOverflow: Modified Valid Parentheses (Multi-type)

StackOverflow 上有人讨论过多类型括号（`()`, `[]`, `{}`）的最小删除问题。精确解法使用 **DFS + Memoization**：

```python
# 对每个字符枚举"删除"或"保留"两种选择
# 用递归 + lru_cache 找最小删除数
# 时间复杂度较高，但能求出精确最优解
```

例如 `"([)]"` 的最小删除是 2（删掉 `[` 和 `]` 得到 `"()"`）。

**我们的区别**：DFS 精确解法时间复杂度高（指数级），不适合大文件实时处理。我们的贪心策略 O(n) 线性扫描，牺牲部分"全局最优性"换取速度和简单性。对于 LSP tokenizer 的场景，O(n) 的近似最优解已经足够实用。

### 为什么是"跳过异型找同型"而不是严格 LIFO？

严格 LIFO（LeetCode 1249 风格）在遇到 `)` 而栈顶是 `[` 时，会直接删掉 `)`。这样 `"([)]"` 会变成 `"([)"`，然后第二遍再删掉 `[`，最终得到 `"()"`——结果是对的，但中间过程不同。

而我们的算法在遇到 `)` 而栈顶是 `[` 时，会**跳过 `[` 去找下面的 `(`**。这样 `"([)]"` 中 `)` 找到 `(`，`[` 被标记为坏，最终也是删掉 `[` 和 `)` 得到 `"()"`。

两种策略在这个 case 上结果相同，但在更复杂的嵌套中（如 `(a [b (c x] y)`），严格 LIFO 会先删掉 `]`（因为栈顶是 `(`），而我们的算法会让 `]` 跳过 `(` 去找 `[`。这会导致不同的清洗结果，也对应不同的 diagnose 集合。

## 实现步骤

### Step 1: 新增 `private:compute-bad-brackets`

```scheme
(define (private:compute-bad-brackets source)
  ;; 返回坏括号位置列表
  ...)
```

实现要点：
- 扫描时跳过 `"..."` 字符串、`#|...|#` 块注释、`;...` 行注释、`#\x` 字符字面量
- 对 `#(...)` 等语法中的括号**不做处理**（`get-datum` 会正确解析）
- 只处理裸括号

### Step 2: 修改 `source-file->annotations`

在 tolerant 分支中：

```scheme
(let ([bad-positions (private:compute-bad-brackets source)])
  ;; 1. 清洗源码
  (let ([cleaned-source (private:replace-positions-with-spaces source bad-positions)])
    ;; 2. 在清洗后的源码上运行 get-datum
    (private:tolerant-parse->patch cleaned-source maybe-document fallback)))
```

### Step 3: 修改 `private:tolerant-parse->patch`

- 每次遇到 condition 时，除了 patch 错误字符，还要**在该位置追加 diagnose**
- 清洗后的源码理论上不应再遇到括号相关的 condition；如果仍遇到，按现有逻辑 patch 并 retry（作为安全网）
- 去掉 `private:find-unclosed-parens-human-view` 调用——unclosed opener 已在预处理阶段被标记为坏括号
- 保留 EOF、dot、sharp-sign 等非括号 condition 的处理

### Step 4: 更新测试

测试策略调整：
- 不再区分"cross-mismatch"和"unclosed"两种 diagnose 类型
- 统一验证：坏括号位置是否被正确识别、清洗后 `get-datum` 是否不再报括号 condition

## 算法调研结论

在确定方案前，我们调研了业界已知的多类型括号最小删除算法，结论如下：

### 已知算法的现状

| 算法 | 时间复杂度 | 支持类型 | 来源 | 适用性 |
|------|-----------|---------|------|--------|
| **Two-Pass Greedy** | O(n) | 单类型 `()` | LeetCode 1249 | 不能直接用于两种类型 |
| **DFS + Memoization** | 指数级 | 多类型 `()`, `[]`, `{}` | StackOverflow | 太慢，不适合大文件 |
| **区间 DP** | O(n³) | 多类型 | StackOverflow / GeeksforGeeks | 太慢，n=1000 就要 10⁹ 操作 |
| **跳过异型找同型** | O(n) | 多类型 | VS Code Bracket Commands 插件 | **唯一实用的贪心策略** |

### VS Code 也在用同样的策略

VS Code 的 **Bracket Commands** 插件（MIT 开源）明确描述了相同的扫描逻辑：

> *"The scanning algorithm searches left and right from the cursor for the **nearest matching open and close characters while respecting nesting of the same bracket type**. **Other bracket types are skipped** so that commands for specific types only operate on the requested pair."*

这说明"跳过异型找同型"不是凭空想出来的，而是业界在编辑器括号匹配工具中实际采用的策略。

### 两种贪心策略的对比

我们对比了**严格 LIFO**（遇到异型直接删 closer）和**跳过异型找同型**：

| 案例 | 严格 LIFO 删除数 | 跳过异型找同型 删除数 | 最优解 |
|------|-----------------|---------------------|--------|
| `(]([)` | 5 | **3** | 3 |
| `([)]` | 2 | **2** | 2 |
| `[(])` | 2 | **2** | 2 |
| `(a [b (c x] y)` | 3 | **1** | 1 |

在复杂嵌套中，跳过异型找同型明显更优。

### 最终结论

多类型括号最小删除的精确解法是 O(n³) 的区间 DP，对于 LSP tokenizer 这种需要实时处理大文件的场景完全不实用。**跳过异型找同型的 O(n) 贪心策略已经是目前最好的选择**。

## 待确认事项

1. **字符串/注释扫描**：是否复用现有的 `consume-block-comment` / `consume-sps-auxiliary`？还是写一套轻量级的预扫描逻辑？
2. **`#(...)` 等语法中的括号**：是否需要识别 `#` 前缀来跳过这些括号？
3. **Fallback 策略**：如果预处理有 bug 导致遗漏，是否保留当前的 `inner` 递归作为安全网？
