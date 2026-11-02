# 类型推断改进方案调研（2026-11-02）

> 起因：`bin/output-type-analysis.ss` 全量导出本仓库（543 个 export 标识符）后的
> 质量分析——precise 38% / imprecise 49% / no-info 13%，无错误签名。
> 本文档记录针对四个问题类的外部调研结论与落地路线图。
> 前置阅读：[`type-inference-precedents.md`](type-inference-precedents.md)
> （success typing / occurrence typing / conditional types / semantic subtyping
> 四条路线的深度综述）。**本文档未提交（uncommitted），内容为调研记录。**

---

## 0. 问题清单（来自全量导出分析，2026-11-02 实测）

| # | 问题 | 规模 | 示例 |
|---|------|------|------|
| 1 | 递归函数返回类型丢失（named-let / 自递归值流断链） | imprecise 的 49% 中最大成因 | `binary-search` 17 条签名全返回 `something?` |
| 2 | record 谓词无签名 | no-info 的 13% 主因 | `file-node?` / `index-node?` / `document?` 裸 `something?` |
| 3 | record accessor 返回 top（字段类型未进 DSL） | 每个 record 库都有 | `file-node-path` → `(something? <- (inner:list? [file-node?]))` |
| 4 | 数值塔参数 cross product 爆炸 | 单标识符最多 73 条签名 | `natural-order-compare`（string?/integer?/real?/number? 逐位组合） |
| 5 | dotted-rest 定义无签名 | protocol API 一批 | `did-open` → 裸 `something?` |

## 1. 问题一（递归返回值）：不动点迭代

**文献共识**：无成功系统用 HM unification 处理递归。两条主路：
(a) lattice 上 Kleene 不动点迭代（Dialyzer/TypEr，SCC 分组，超时给 top，
success-typing 语义）；(b) 单趟 + 重入守卫 + 占位符传播（Pyright/TypeScript/orly）。
共同工程细节：**memo 表"占位结果不缓存、具体结果才缓存"**，使第二阶段迭代增量极小。

| 方案 | 机制 | 精度 | 实现量 | 出处 |
|------|------|------|--------|------|
| A 重入守卫 | 守卫命中→`something?`，单趟 | 递归结果参与返回值时精度为零（= 现状成因） | ~30 行 | orly `TFunction::GetReturnType` |
| **B 两阶段重解释** | Phase1 占位算 body 得 R1；Phase2 递归调用重绑 R1 再算，R2==R1 收敛 | binary-search 17→1 条，返回 `(number?\|#f)` | ~150 行，generator 外层 | orly issue #128 Option B |
| C Dialyzer SCC | 调用图 + SCC 自底向上 + 组内 fixpoint | 优势仅在互递归 | ~300 行 | Lindahl & Sagonas PPDP'06 |
| D cap widening | union 原子数 > K 或深度 > D → widen top | B 的 safety net | +30 行 | Dialyzer "upped bound type"；Cousot ∇ |

**推荐：B（主体）+ D（cap 兜底）**，落地形态：

> 递归调用绑定 running union 迭代解释 body；`R_{n+1} == R_n` 或 n≥N(4–8) 或
> 原子数>K 时停；占位结果永不缓存使第 2 轮增量极小；超时返回 union∪`something?`。
> soundness = post-fixpoint（`F(R)⊆R` 即停）；termination 由有限原子集 + cap 保证。

**关键陷阱**（orly 实证）：任何以占位 `something?` 为操作数构造出的"具体"复合类型
**不可缓存**，否则 Phase 2 永远收不到重算，fixpoint 静默失败。

- TypeScript 是反面教材：递归箭头函数直接 `any`（error 7022/7024），
  说明"单趟 eager inference"路线的天花板。
- Pyright 的 call-site 实参类型可作迭代初值 X⁰，通常 1–2 轮收敛且更精确。
- 互递归：按 identifier 侧已有 root-ancestor 分组做 SCC，组内同步迭代（C）。

来源：
- orly #128: https://github.com/orlyatomics/orly/issues/128
- Lindahl & Sagonas PPDP'06: https://user.it.uu.se/~kostis/Papers/war_story.pdf
- success typing 深度截断示例: https://www.cs.ubbcluj.ro/~studia-i/contents/2014-macs/16Olah.pdf
- Cousot widening 教程: https://cs.nyu.edu/~pmc309/publications.www/slides/Cousot-WG2-3-Talk-4-Biarritz-4-1.pdf
- post-fixpoint 定理: https://inria.hal.science/hal-02916996/document
- Pyright 返回类型推断: https://docs.basedpyright.com/v1.40.1/usage/type-inference/
- TS 递归 any: https://github.com/microsoft/TypeScript/issues/58100

## 2. 问题二/三（record 谓词 / 字段类型）：声明表驱动 + 命题槽位

五系统对照（谓词类型 / accessor 类型 / 字段类型来源 / 收窄机制）：

| 系统 | 谓词 | accessor | 字段来源 | 收窄 | 适配度 |
|------|------|----------|----------|------|--------|
| Typed Racket | arrow 带 propositions 槽 `(-> Any Boolean : String)` | 精确字段类型 + Path object | 显式声明 | occurrence typing | ★★★★★ |
| Dialyzer | guard BIF，record = tagged tuple | 查 record 声明表 | `-record` 声明；无声明则全程序 union 累积 | guard 信息流 | ★★★☆ |
| Pyright/mypy | TypeGuard/TypeIs（PEP 742） | dataclass 字段=声明类型 | 声明优先，缺则 RHS 推断 | CFG narrowing | ★★★★ |
| TypeScript | type predicate `x is T` | interface 属性 | 纯声明 | 判别 union | ★★★ |
| CDuce | record = 标签→类型映射（准常量函数） | record pattern 解构 | 类型声明 | 模式流敏感细化 | ★★★★（中期） |

**结论（按优先级）**：

1. **声明表驱动合成**（Dialyzer 声明表 + mypy constructor 合成）：
   R6RS `define-record-type` 字段封闭，从 `(fields ...)` 提取字段↔getter/setter
   映射，合成谓词签名 `(boolean? <- (inner:list? something?))`，消灭 no-info。
   现有 `record.sls` 已有半成品（谓词 type-expression 被注释，注释称 trivial.sls
   硬编码处理，但 trivial.sls 只覆盖内置谓词——这正是 `file-node?` 裸奔的根因）。
2. **谓词命题槽位**（Typed Racket ICFP'10 / PEP 742 TypeIs）：
   arrow 挂 `(正, 负)` 命题对；if/cond 分支 then→`arg∩T`、else→`arg−T`。
   只处理一阶位置（谓词直接应用裸标识符），不碰 higher-order 命题擦除
   不 sound 的坑（arXiv:2609.16299）。union 求差复用已落地的 semtype 引擎。
3. **字段类型**：短期 constructor 调用点累积 union（Dialyzer 局部版）；
   中期对齐 CDuce record 模型（arXiv 2404.00338），与 semtype 引擎衔接。
4. **排除 row polymorphism**（OCaml/Elm）——需要 unification，与 substitution
   DSL 不兼容；R6RS record 封闭语义 → 选 Hack shape 式封闭记录。

来源：
- ICFP'10 Logical Types: https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf
- TS Guide propositions: https://docs.racket-lang.org/ts-guide/types.html
- PEP 742 TypeIs: https://peps.python.org/pep-0742/
- PEP 647 TypeGuard: https://peps.python.org/pep-0647/
- Dialyzer records: https://www.erlang.org/eeps/eep-0008.html
- Python NamedTuple 合成: https://typing.python.org/en/latest/spec/namedtuples.html
- TS narrowing: https://www.typescriptlang.org/docs/handbook/2/narrowing.html
- CDuce records: https://arxiv.org/html/2404.00338v2
- higher-order 命题不 sound: https://arxiv.org/abs/2609.16299

## 3. 问题四（数值塔爆炸）：压缩三选项

- **1B 显示层语义折叠**（先做，无损）：hover 已有 `sem:compact-display`，
  复用到候选列表层——CDuce 打印同款"语义最小规范形"。
- **1A 支配消除**：O(n²) 两两判定，参数位超集 + 返回相同即删被支配项；
  tower cross product 预期 73→4–8 条；纯删语义冗余。
- **1C IR 级 tower collapse**（可选档位）：模仿 Pyright int→float promotion /
  TS literal widening，有损；hover 显示用、诊断关断。
- 保险丝常数抄 **Pyright 的 64**（union 成员上限 + overload 展开上限）。

来源：
- CDuce compact representations: https://arxiv.org/html/2603.25306v4
- FCB JACM'08: https://www.cduce.org/papers.html
- arrow union normal form (ICFP'21): https://i.cs.hku.hk/~bruno/papers/icfp2021.pdf
- Pyright union/overload 上限: https://docs.basedpyright.com/v1.39.0/usage/type-concepts-advanced/
- Pyright numeric promotion: https://docs.basedpyright.com/v1.28.0/usage/type-concepts-advanced/
- Dialyzer t_limit_k: https://www.erlang.org/blog/my-otp-24-highlights/

## 4. 问题五（rest 参数）：arrow rest 槽

- **2A arrow 加 rest 槽**（推荐）：`(T * -> R)` 均匀语义（Typed Racket `#:rest` /
  Pyright `ParamCategory::ArgsList` 同款）；`define`/`lambda*` 规则对 dotted-rest
  生成带 rest 槽的 arrow，arity 检查 `≥ required`。直接修复 `did-open` 类无签名。
- 2B rest 用 `inner:list? of T` 表示更贴近运行时语义，为后续 car/null? 分支
  精化铺路，工作量中等。
- 可选参数：展开成多条定长 arrow（Dialyzer/TS 先例，case-lambda 已同构）。

来源：
- TR variable arity: https://docs.racket-lang.org/ts-guide/variable-arity.html
- TR ts-reference: https://plt.cs.northwestern.edu/snapshots/current/pdf-doc/ts-reference.pdf

## 5. 路线图

| 优先级 | 事项 | 预期收益 | 实现量 | 状态 |
|--------|------|----------|--------|------|
| P0 | record 谓词签名（恢复+扩展 record.sls） | no-info 13%→~5% | 极小 | **已完成**（2026-11-02，谓词定义节点携带 arrow，导出/悬停验证通过） |
| P1 | 递归 fixpoint B+D | imprecise 49%→~25% | ~200 行 | **已完成**（2026-11-02，`fixpoint.sls`；binary-search 返回类型全 something? → 20/21 条具体 list 签名；互递归联合求解；性能中性） |
| P2 | 1B/1A 签名压缩 | 显示签名数 -80%+，无损 | 小 | 未开始 |
| P3 | 2A rest 槽 | protocol API 一批签名复活 | 小 | 未开始 |
| P4 | 谓词命题 + semtype narrowing | 根治 something? 泛滥 | 中 | 未开始 |
| P5 | record 字段类型 / CDuce record / SCC 互递归 | 长期 | 大 | 未开始 |

方向间收敛性：POPL'22 已把 occurrence typing 归约到集合论框架——P4 的
restrict/remove 与 semtype 的 meet/diff 是同一套原语；P2 的压缩也在给
semtype 引擎喂数据。无架构冲突。

## 附录：全量导出质量基线（2026-11-02，复现方式见下）

- 命令：`scheme --script bin/output-type-analysis.ss . /tmp/sls-types-all.txt`
  （需 `source .akku/bin/activate`；全量含依赖 22.5 分钟）
- 自身代码 543 标识符：precise 204 (38%) / imprecise 266 (49%) / no-info 73 (13%)
- 6 评估库耗时：contain 37s / json 14s / file-node 13s / reference 13s /
  interpreter 17s / binary-search 15s（历史上 interpreter 与 binary-search
  在 600s 超时阈值下失败，现快 35 倍以上）
- 复核确认：271 条"空 `(inner:list?)` 返回"全部为 `'()` 基例分支的 sound 签名
  （如 `search-end-with` 的 else 分支），非错误签名
