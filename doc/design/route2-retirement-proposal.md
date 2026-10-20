# 路线二（ufo-match 自动宏展开管线）退役提案

> 状态：**提案 — Phase 1 已完成**（2026-10-20）。
> 依据文档：`doc/analysis/chez-sc-expand-records-switch.md`（机制与全部落地记录）、
> `doc/analysis/macro-resolution-profile.md`（route 2 成本结构）、
> `doc/analysis/macro-analysis-framework.md`（E1–E4 理论框架）。
> 关键模块：`analysis/identifier/chez-ir.sls`（IR 提取）、
> `analysis/identifier/ir-auto-route.sls`（并行路径）、
> `analysis/identifier/expanders/`（route 2 管线，退役对象）、
> `analysis/identifier/self-defined-rules/router.sls`（分发点）。

---

## 1. 现状

路线二 = 对 ufo-match 六宏族（match / match-next / match-one /
match-check-ellipsis / match-two / match-check-identifier）的急切全级联自动展开：
router 分发 → generator（syntax-rules 机器：`dispatch-matcher` 的 Chez
`$syntax-dispatch` 逐行转写 + `pattern.sls` 模板实例化 + pretty-print/reparse）→
step 分析展开树 → `expansion-wrap.sls` 双向回传（E4）。

**一个关键事实**：调查证实生产代码中 generator 的唯一调用方就是 router 的
ufo-match 臂（`expansion-generator->rule` 仅此一处生产调用；手写 `match-process`
回退已于 2026-10-05 移除；expander-doc 旧通道无调用者）。即**整条 route 2 管线
只为这六个宏服务**——它不是通用设施，是一个宏家族的专用引擎。

路线三 = 用 Chez 自己的 expander（未文档化的 `(sc-expand x env #t)` 开关，
返回带逐节点源位置的编译器 IR）替代上述管线：`chez-ir` 提取
`(name ref-bias binding-bias)` 三元组 → `ir-auto-route` 按位置合成引用。

## 2. 证据汇总（2026-10-20，全部可复现）

| 证据 | 结果 |
|---|---|
| 差分测试（route 2 vs Chez IR 地面真值） | route 2 修复后 15/15；IR 模式 15/15 |
| Tier-3 spike（单调用点替代管线） | 15/15；11 ms vs route 2 冷调用 228 ms（≈20x） |
| 全仓金丝雀（`bin/ir-route-canary.ss`，本仓库 835 文档自分析） | IR 命中 45 真实调用点；**诊断单向优**——消除 14 条 "Unused local variable" 误报（route 2 重复引用损坏 usage-count 的首次用户可见暴露），零新增；引用 -0.03%；墙钟持平 |
| 多线程浸泡（`tests/robustness/robustness-ir-route-threaded.sps`） | 6 次 threaded init，25/25，并发 sc-expand 干净 |
| 既有 match 家族全量回归（IR 默认开启下） | 全部与基线逐组一致 |

### IR 模式相对 route 2 的确定优势

1. **语义 = Chez 原生**，无需自证（route 2 的正确性依赖 dispatch-matcher
   转写保真 + datum 级对比测试）。
2. **不产生重复引用**（TODO-15 P2 多重性是 route 2 usage-count 损坏的根源，
   误报即由此而来）。
3. **拒绝对宏模板内的伪调用点做过分析**（金丝雀中 256 个回退全部是
   ufo-match 自身模板内的 match 调用——按 Chez 语义它们不是调用点；
   route 2 的急切展开白分析它们，正是 profile 中 +231 ms 的组成部分）。
4. **对未保存缓冲区天然正确**（从 document-text 直接解析，带缓存上限）；
   磁盘不一致这一类回退已消除。
5. 性能持平或更好；且不再对整个展开树跑 step（历史 profile 中最大单项）。

### 已知残余与限制（诚实清单）

- 宏库自身模板内的调用点：IR 模式不做分析（语义上正确；若 IDE 需要在
  宏定义文件内跳转模板中的模式变量，route 2 也只做近似——接受现状）。
- 宏派生绑定的可见性粒度 = 整个宏调用（route 2 修复后同样如此；更细的
  子句级作用域需 Chez IR 的 letrec 结构信息，留作增强）。
- 重复 speculative 引用在 route 2 模式下仍存在（IR 模式无此问题）。
- 退役后 route 2 模式的唯一保留理由 = `SLSCHEME_IR_ROUTE=0` 回归二分。

## 3. 退役阶段

### Phase 1 — 受控并行（✅ 已完成）

IR 路径默认开启，opt-out：`SLSCHEME_IR_ROUTE=0`。
`run.ss` 已 import `(ufo-match)`（IR 探测前提）。
任意失败静默回退 route 2（`REPLAY_TRACE=1` 可见 miss 原因）。

### Phase 2 — router 简化（建议下个迭代）

router 的 ufo-match 臂改为 IR-only；route 2 规则保留在
`SLSCHEME_IR_ROUTE=0` 分支后（二分能力不丢）。条件：

- [ ] 本仓库 dogfood 一个发布周期无 IR 相关回归（每日用 `.scheme-langserver.toml`
      配置跑 MCP 桥即天然覆盖）；
- [ ] 外部至少一个非 ufo-match 重度用户项目验证（IR 路径对调用点含跨库
      自由变量的 define 目前会回退——见 §4，需量化回退率可接受）。

### Phase 3 — 管线退役（条件满足后）

删除对象（约 2,000 行 + 相应测试）：
`expanders/syntax-rules.sls`（含 TODO-16 骨架）、`expanders/dispatch-matcher.sls`、
`expanders/pattern.sls`（模板实例化部分）、模板 replay 缓存
（`expansion-wrap.sls` 的 TODO-10 机制）、router 中 route-2 臂。

**保留**（仍被非宏路径使用）：`expanders/expansion-wrap.sls` 的
`expansion-generator->rule` 外壳（Phase 2 前仍承载 route-2 回退；
Phase 3 后随回退一并移除）、`expanders/syntax-case.sls`（30 行，若
syntax-case 宏的 generator 也无调用者则一并清理——需先确认）。

前置条件：
- [ ] Phase 2 稳定一个发布周期；
- [ ] `usage-count` / unused-imports / unused-local-variables 在更多工作区上
      与 route 2 模式 diff 无回归（金丝雀方法已工具化，换根目录重跑即可）；
- [ ] AGENTS.md 宏分析章节重写（route 2 退役、route 3 为主路径）。

## 4. 风险与回退

| 风险 | 缓解 |
|---|---|
| Chez 升级破坏未文档化 API（`sc-expand` 3 参数、`#%$record-type-descriptor`、IR 文法） | 全部隔离在 `chez-ir.sls`；`chez-ir-available?` 启动自检，失败自动回退 route 2——**升级破坏 = 静默降级，不是崩溃** |
| IR 路径在特定工作区大面积回退（性能劣化） | `(ir-auto-route:stats)` 暴露命中/回退/耗时；金丝雀脚本可对新仓库复测 |
| 多线程 Chez expander 的未知竞争 | 已浸泡 6 迭代干净；加长浸泡（CI nightly）后进 Phase 2 |
| 回退需求（IR 语义争议） | `SLSCHEME_IR_ROUTE=0` 逐进程 opt-out；Phase 2 前 route 2 代码原样保留 |

## 5. 决策点

本提案不需要立即决策 Phase 3。当前默认（IR 开 + route 2 回退）已被证据支持
直接维持；Phase 2 的触发条件是 §3 中的 soak 清单，建议在下一个版本发布时
评审一次。
