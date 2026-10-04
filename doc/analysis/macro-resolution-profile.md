# 自动宏解析（路线二）性能 Profile 实测与推测

> 范围：仅"宏解析机器本身"（generator → 模板实例化 → 重解析 → 配对 → shallow-copy 回传），**不含**抽象解释器对生成代码的一般分析。
> 方法：以 `test-match-cascade-auto-resolve.sps` 的测试路径为基准，用 Chez `(time ...)` / `current-time` + `time-difference` 对真实 `ufo-match` 宏家族的 generator / rule / pattern.sls 逐函数做迭代实测，再按代码结构外推。
> 日期：2026-10-03（2026-10-03 第二次修订：逐函数 profile 推翻了首版"pattern 机器是大头"的判断，真实大头是 eval 的子句编译，见 §1）。
> 所有"实测"行可复现：项目根目录 `source .akku/bin/activate` 后运行 `scheme --script`（脚本见附录）。

---

## 1. generator 本体分解（930 µs，5-clause `match` 第一层，N=200）

| 组件 | 耗时 | 占比 | 性质 |
|---|---|---|---|
| **`eval` clause 确认**（`private:confirm-clause` 把含全部子句的 syntax-case 表单交宿主 eval） | **668 µs** | **72%** | 线性于宏定义的**子句数**（实测 ~180 µs/子句的编译成本），与调用点大小无关；编译产物 ~600KB/次，是单次 920KB 分配的主要来源 |
| 输出配对 `private:expansion+index-node->pairs` + quasiquote 构造 eval 表单 + `private:signature-match?` 循环 + wrapper 杂项 | ~186 µs | 20% | 常数级，子句多时配对线性增长 |
| pretty-print + `source-file->annotations` 重解析（148 字符实际展开） | 59 µs | 6% | 线性于展开尺寸，常数小 |
| **pattern.sls 全部函数**（见 §2） | **~17 µs** | **2%** | 常数极小，**不是热点** |

**修正记录**：首版本文据单次成本外推猜测"pattern 机器（建树/对齐/协程取值/实例化）占 50–65%"，逐函数实测后证伪。真实图景：**每次宏调用都把宏定义的全部子句重新编译一遍**——这是路线二最大的结构性浪费，也是最明确的优化靶子（定义期一次性编译 clause 选择器、调用期复用，可把 72% 砍掉）。OPT-1 的签名过滤（identifier.md 记录 1.75×）只是减少了 eval 前的尝试次数，并未消除"每次调用全量子句编译"。

其他实测数字：generator 本体 ~930 µs；端到端 rule（+ 递归 step + shallow-copy）~1.79 ms（1-clause ~1.45 ms）；单次分配 ~920KB（eval 编译产物为主）；第一层展开尺寸 ×1.2（148/120 字符；×16–71 的膨胀发生在 L2–L6，Chez 完全展开实测）。

---

## 2. pattern.sls 逐函数 profile（真实 match 主子句，N=500）

| 函数 | 单次 | 说明 |
|---|---|---|
| `make-pattern(pattern)` | 1 µs | 建树，纯分配（~1.8KB/树） |
| `make-pattern(template)` | 3 µs | 模板树略大 |
| `gather-context` | ~0 µs | 树上收集变量名 |
| `pattern+index-node->pair-list` | 1 / 2 / 6 µs（1/5/20 子句） | 线性于调用点 AST 大小；ellipsed 从右对齐不增加量级 |
| `pattern+context->pairs->iterator` ×3 | ~0 µs | ellipsed 协程只是闭包分配，创建工作在消费时 |
| `generate-binding` ×3（含协程流消费、栈式重组） | 10 µs | 全文件最贵；ellipsed 变量（pat/body）走协程路径 |
| `expand->index-node-compound-list` | 1 µs | 模板实例化本身极快（bindings 预计算前提下） |
| **合计** | **~17 µs** | **占 generator 本体 2%** |

结论：`generate-binding` 的协程消费协议（dive/escape 指令 + 祖先栈重组）是全文件唯一值得注意的算法，但常数在 10 µs 级；`pattern+index-node->pair-list` 线性于调用点规模。对真实场景 profile 而言，pattern.sls **无实质影响**——优化 effort 不应投这里。

---

## 3. 真实场景推测：宏解析时间构成（修正版）

| 排名 | 组件 | 占比预测 | 性质 |
|---|---|---|---|
| 1 | **eval 子句编译**（`private:confirm-clause`） | **~70%**（调用点密、子句多时更高） | 线性于宏定义子句数 × 调用点数；可通过"定义期编译一次"消除 |
| 2 | 输出配对 + 杂项 | ~15–20% | 线性 |
| 3 | shallow-copy 回传 + ordered-reference-list 插入 | ~5–15%，**大文件升级为第一** | **唯一超线性组件**（MEM-1 标注 O(n² log n)）；生成代码引用密度数倍于手写代码 |
| 4 | pretty-print + 重解析 | ~5% | 线性于展开尺寸，常数小 |
| 5 | pattern.sls | ~2% | 可忽略 |
| 6 | GC | 微观 1–3% → 真实 10–20% | eval 编译产物（~600KB/次）× 数千调用点是分配主源 |

---

## 4. 真实场景实测（2026-10-03，替换首版外推）

方法：对全项目所有 `match` 顶层调用点跑**完整级联**（6 个 ufo-match 宏、memory cap 15、递归驱动模拟 router 启用后的真实递归；不含 step/shallow-copy）。

| 指标 | 实测值 |
|---|---|
| 规模 | 823 文档，57 个顶层 `match` 调用点（含 tests/ 与 .akku/lib） |
| 总展开次数 | 4224 次（1233 次失败返回：无匹配子句/调用点含 `...`/guard 拒绝） |
| 生成代码总量 | 1.57 MB |
| 墙钟（纯宏解析，generator 本体） | **1.17 s** |
| 单次展开平均 | **0.28 ms**（内层宏子句少，比孤立测的 0.93 ms 便宜） |
| 每层展开数 | 16/16/16/29/44/61/99/139/171/240/336/460/621/845/1131 |

**三个结论**：

1. **"分钟级"外推证伪**：全项目 match 家族完整级联只加 ~1.2 s（叠加 step+shallow-copy ~0.7 ms/层估 +3 s），对 22.5 s 基线 +10–20%。identifier.md 记录的 "severe degradation" 来自**叠加成本**（解释器对 71 倍膨胀代码的分析 + 回传插入的文档级超线性），不是宏解析机器本身。
2. **危险信号是层分布的指数尾**：13–15 层占全部展开的 61%（2597/4224）——生成代码中重复的 failure thunk 与内层宏调用被逐个重复展开，DAG 共享完全未利用，大量调用点顶着 cap 15 仍未收敛。这是"展开缓存 keyed by (宏定义, 调用点形状)"（框架纪要 §6-Q2c）的量化依据：memoize 后尾部 2597 次可坍缩到几十次。
3. **eval 占比在宏观尺度依然成立**：1.17 s × 72% ≈ 0.85 s 为 eval 子句编译，与 §1 微观分解互相印证。

其他场景参考：只开本项目自身 35 个 match 点（不含 tests/.akku）≈ 0.5–0.8 s；稳态编辑下若复用展开缓存，didChange 增量重展开可压到百 ms 级；`.akku/lib` 全部 395 宏不筛选 = 本实测的数倍（更多宏家族、更浅级联），叠加成本后才会逼近分钟级。

---

## 5. 优化靶子优先级（按 profile 证据排序，2026-10-03 修订）

1. **clause 选择器定义期编译**（微观 72% / 宏观 ~72%，预期收益最大）：把 `(syntax-case <input> <literals> <clauses...>)` 在 `define-syntax:attach-generator` 时编译一次（input 用占位符），调用期只做匹配，消除每调用成本。
2. **展开缓存（memoize）**（宏观层分布实测的 61% 指数尾）：keyed by (宏定义, 调用点形状/内容)，DAG 共享重复展开坍缩；兼解决稳态编辑的重展开延迟。这是 Q2c 的直接量化支撑。
3. **shallow-copy 的 ordered-reference-list 插入**（大文件上的超线性）：落实 MEM-1（当前 O(n² log n)）。
4. **级联调度**（乘数控制）：按需展开 + 宏家族白名单（框架纪要 §6-Q2）；与 1/2/3 正交。
5. ~~优化 pattern.sls 的协程/重组算法~~：实测 10 µs 级（§2），不值得投。

---

## 6. 测试盲区、bug 与总结

**测试盲区**：9 个自动宏解析测试覆盖正确性（generator 存在性、单步展开形状、pairs 映射、shallow-copy 闭环），盲区在乘数与规模效应（调用点密度、级联树规模、文档级引用表增长）——§4 的实测弥补了这一盲区。

**实测中修复的 bug**：`pattern.sls` 的 `make-pattern` 在 **vector 分支（L86-93）与 equal?-datum 分支（L97）漏传第 6 个字段（cdr?）**，遇向量字面量或模板中的字符串/数字等原子项即抛 arity 异常。已修复（补 `#f`）；此前未暴露是因为已写测试从未让含字符串模板（如 match 的报错子句）的子句被实例化。

**总结**：真实场景级联实测表明，纯宏解析机器即使全量开启也仅是秒级增量（本项目 +1.2 s），其内部成本结构（eval 子句编译 ~72%）与微观分解一致；真正的规模风险在指数尾（DAG 共享未利用，61% 展开集中在 cap 边界）与叠加成本（解释器分析膨胀代码 + 回传插入）。优化顺序：clause 选择器定义期编译 > 展开缓存 > 回传插入 > 调度控制。

---

## 附录：复现脚本

### A. generator / rule 端到端（§1）

```scheme
;; 项目根目录：source .akku/bin/activate && scheme --script bench.ss
(import (chezscheme) (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis tokenizer)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver util test))

(define workspace (init-workspace (current-directory)))
(define root (workspace-file-node workspace))
(define ufo-doc (file-node-document (walk-file root (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls"))))

(define (get-gen name)
  (let loop ([nodes (document-index-node-list ufo-doc)])
    (if (null? nodes) #f
      (let ([r (find-index-node-recursive
                 (lambda (n) (let ([e (annotation-stripped-expression n)])
                               (and (list? e) (not (null? e)) (eq? 'define-syntax (car e))
                                    (list? (cdr e)) (not (null? (cdr e))) (eq? name (cadr e)))))
                 (car nodes))])
        (if r (index-node-expansion-generator (car (reverse (index-node-children r))))
            (loop (cdr nodes)))))))

(define match-gen (get-gen 'match))
(define m5 '(match x ((? string? path) path) ((? number? n) n) ((a b) (list a b)) ((s rest) (list s rest)) (_ 'fallback)))
(define (mknode expr) (init-index-node '() (car (source-file->annotations (with-output-to-string (lambda () (pretty-print expr))) "/dev/null"))))
(define node (mknode m5))
(collect 3)
(time (let loop ([i 0]) (when (< i 50) (match-gen #f #f ufo-doc node) (loop (+ 1 i)))))
(define rule (expansion-generator->rule match-gen step (workspace-file-linkage workspace) '() '()))
(collect 3)
(time (let loop ([i 0]) (when (< i 50) (rule root (workspace-library-node workspace) ufo-doc (mknode m5)) (loop (+ 1 i)))))
```

### B. pattern.sls 逐函数（§2）

```scheme
;; 计时骨架（bench 定义 + 输入构造同 §A；pattern/template datum 从 define-syntax match
;; 的子句节点取 annotation-stripped），对每个函数 (collect 3) 后 N=500 次循环，
;; 用 (current-time) + (time-difference) + (time-nanosecond) 计平均。
(bench "make-pattern(pattern)" (lambda () (make-pattern pattern-datum)))
(bench "pair-list 5-clause"  (lambda () (pattern+index-node->pair-list p-tree call5)))
(bench "generate-binding x3" (lambda () ... ))  ; 见正文 §2
(bench "expand->compound"    (lambda () (expand->index-node-compound-list t-tree bindings context)))
```

### C. eval 子句编译 scaling（§1 关键证据）

```scheme
;; 4 子句全编译：668 µs；1 子句对照：115 µs；~180 µs/子句
(collect 3)
(let* ([t0 (current-time)]
       [_ (let loop ([i 0]) (when (< i 200)
           (syntax->datum (eval `(syntax-case ',m5 ()
             ,@(map (lambda (c) `(,(car c) #'(,(car c) . ,(cadr c)))) clauses)
             (else #f))))
           (loop (+ 1 i))))]
       [dt (time-difference (current-time) t0)])
  (printf "eval 4-clause: ~a us~%" (round (/ (time-nanosecond dt) 200 1000))))
```
