# Chez Scheme `read` / `get-datum` 异常说明

本文档基于 Chez Scheme 源码（`s/read.ss` 与 `s/exceptions.ss`）分析 `read` 和 `get-datum` 两个过程在读取数据时可能抛出的异常，重点说明**词法错误**与**语法错误**的区别、触发代码示例、报错信息字段与源代码的对应关系，以及 `condition-irritants` 的详细结构。

---

## 一、错误分类：词法错误 vs 语法错误

在 `s/read.ss` 中，读取过程分为两个阶段：

1. **词法分析（Tokenization）**：由 `rd-token` 及其子状态（`rd-token-*`）负责，把字符流拆成 token。这一阶段抛出的错误称为**词法错误**（lexical errors）。
2. **语法分析（Parsing）**：由 `rd-help`、`rd-paren-list`、`rd-vector` 等负责，把 token 组合成 S-expression。这一阶段抛出的错误称为**语法错误**（syntax/structural errors）。

两类错误最终都通过 `$lexical-error` 抛出，condition 对象里都会包含 `&lexical-violation` 和 `&i/o-read-error`。

---

## 二、异常的数据结构详解

### 2.1 底层 condition 构造

所有读取异常最终都走 `error-help`（`s/exceptions.ss:618`），构造的 condition 至少包含：

| 组件 | 说明 |
|------|------|
| `&lexical-violation` | 词法/语法层面的错误 |
| `&i/o-read-error` | I/O 读取操作失败 |
| `&format-condition` | 说明 message 是格式字符串 |
| `&i/o-port-error` | 携带导致错误的输入 port |
| `&who` | 过程名：`read` 或 `get-datum` |
| `&message` | 人类可读的错误描述（已格式化） |
| `&irritants` | 格式化时使用的参数列表（见第 2.4 节） |
| `&continuation` | 包含一个 continuation `k` |

### 2.2 额外可选组件

| 组件 | 触发条件 | 说明 |
|------|----------|------|
| `&implementation-restriction-violation` | 当 `rd-error` 的 `ir?` 参数为 `#t` | 实现限制违反（如数字无法表示） |
| `$&src` | 当读取带 source-file-descriptor 的端口时 | 包含源文件位置信息（bfp/fp） |

### 2.3 错误消息字段与代码的对应关系

`rd-error` 函数（`s/read.ss:278`）的签名逻辑如下：

```scheme
(xdefine (rd-error ir? start? msg . args)
  ...)
```

- **`ir?`**（implementation restriction?）：若为 `#t`，condition 中额外加入 `&implementation-restriction-violation`。对应代码场景如：`"cannot represent ~a"`（数字太大无法表示）。
- **`start?`**：控制错误位置显示。若 `#t`，使用 `bfp`（beginning file position）；若 `#f`，使用 `fp`（current file position）。对应代码中 `"invalid number syntax ~a"` 等通常传 `#t`，表示错误从 token 起始位置算起。
- **`msg`**：格式字符串模板，最终出现在 `&message` 中。
- **`args`**：格式参数列表，最终出现在 `&irritants` 中（见下节详细说明）。
- **`port`**：总是当前输入端口，出现在 `&i/o-port-error` 里。
- **`bfp` / `fp`**：若提供了 `source-file-descriptor`，会包装成 `$&src` 条件；否则只在 message 里以字符位置出现。

### 2.4 `condition-irritants` 的详细结构

`condition-irritants` 提取的是 `&irritants` 条件中的值。它的内容取决于 `rd-error` 调用时传入的 `. args`，以及 `rd-error` 所处的端口上下文（是否有 `fp`、是否有 `sfd`、是否是控制台输入）。

#### 2.4.1 核心规则

1. **无 args 时无 irritants**：如果 `rd-error` 没有传入任何 `args`（即空列表），`error-help` 内部判定为 "无 irritants"，`condition-irritants` 将返回 `#f`，且 condition 中**不附带** `&irritants` 组件。
2. **控制台输入**（`eq? ip (console-input-port)`）：`$lexical-error` 直接接收原始 `msg` 和原始 `args`，`condition-irritants` = 原始 `args`。
3. **有 `source-file-descriptor` 的端口**：`$lexical-error` 直接接收原始 `msg` 和原始 `args`，`condition-irritants` = 原始 `args`。
4. **普通文本端口（最常见）**：`rd-error` 会把错误消息重写为 `"~? at char ~a of ~s"`，并把 **原始 `msg`、原始 `args`、位置（`bfp` 或 `fp`）、port 对象** 一起打包成新的 `args` 传给 `$lexical-error`。
5. **`fp` 为 `#f` 时**：类似地，原始 `msg` 和原始 `args` 会与 `pos`（或仅有 `ip`）一起打包成新的 `args`。

#### 2.4.2 四种上下文下的 `condition-irritants` 结构

假设触发词法错误的原始调用是：

```scheme
(xcall rd-error #f #t "invalid sharp-sign prefix #~c" #\z)
```

则不同上下文下 `condition-irritants` 的结构如下：

| 上下文 | `condition-irritants` 返回值 | 结构说明 |
|--------|------------------------------|----------|
| **控制台输入** | `(#\z)` | 原始 args 列表 |
| **普通文本端口，有 `fp`** | `("invalid sharp-sign prefix #~c" (#\z) 0 #<input-port ...>)` | `[原始msg, 原始args列表, 位置数字, port对象]` |
| **`fp` 为 `#f`，但 port 有 `port-position`** | `("invalid sharp-sign prefix #~c" (#\z) 0 #<input-port ...>)` | `[原始msg, 原始args列表, pos数字, port对象]` |
| **`fp` 为 `#f`，port 也无 `port-position`** | `("invalid sharp-sign prefix #~c" (#\z) #<input-port ...>)` | `[原始msg, 原始args列表, port对象]` |
| **有 `sfd` 的端口** | `(#\z)` | 原始 args 列表（位置信息已提取到 `$&src` 条件中） |

> **注意**：对于无 `args` 的情况（如 `"unexpected close parenthesis"`），上述所有上下文都返回 `#f`（无 `&irritants` 组件）。

#### 2.4.3 为什么位置信息在 irritants 里？

在普通文本端口场景下，`rd-error` 的 `else` 分支（`s/read.ss:288`）是这样的：

```scheme
($lexical-error (rcb-who rcb)
  "~? at char ~a of ~s"
  (list msg args (if start? bfp fp) ip)
  ip ir?)
```

这里的 `~?` 是 Chez Scheme 的格式扩展，它消费**两个参数**：格式字符串和该字符串的参数列表。因此：
- `~?` 消费 `"invalid sharp-sign prefix #~c"` 和 `'(#\z)`，生成 `"invalid sharp-sign prefix #z"`
- `" at char "` 是字面量
- `"~a"` 消费 `0`
- `" of "` 是字面量
- `"~s"` 消费 `#<input-port ...>`

最终生成的 message 形如：
```
"invalid sharp-sign prefix #z at char 0 of #<input-port string>"
```

而传给 `error-help` 的 `args`（即 `condition-irritants`）就是 `(list msg args position ip)`。

#### 2.4.4 各类错误对应的 `condition-irritants` 实例（普通文本端口场景）

以下假设使用 `open-input-string` 读取，即最常见的**普通文本端口、有 `fp`** 的场景。

| 错误类别 | 触发代码 | `condition-irritants` 结构 | 元素含义拆解 |
|----------|----------|----------------------------|--------------|
| **EOF 错误** | `(read (open-input-string "(1 2"))` | `("unexpected end-of-file reading ~a" ("list") 4 #<input-port ...>)` | `[原始msg, ("list"), fp位置, port]` |
| **Sharp-sign 前缀** | `(read (open-input-string "#z"))` | `("invalid sharp-sign prefix #~c" (#\z) 0 #<input-port ...>)` | `[原始msg, (#\z), bfp位置, port]` |
| **Sharp-sign 前缀（带数字）** | `(read (open-input-string "#1z"))` | `("invalid sharp-sign prefix ~a~a" ("1" #\z) 0 #<input-port ...>)` | `[原始msg, ("1" #\z), bfp位置, port]` |
| **Boolean 分隔符** | `(read (open-input-string "#t1"))` | `("invalid delimiter ~a for ~a" (1 "boolean") 2 #<input-port ...>)` | `[原始msg, (1 "boolean"), fp位置, port]` |
| **Boolean 拼写** | `(read (open-input-string "#trux"))` | `("invalid boolean #~a~c" ("tru" #\x) 0 #<input-port ...>)` | `[原始msg, ("tru" #\x), bfp位置, port]` |
| **Character 名称** | `(read (open-input-string "#\\unknown"))` | `("invalid character name #\\~a" ("unknown") 0 #<input-port ...>)` | `[原始msg, ("unknown"), bfp位置, port]` |
| **Character hex** | `(read (open-input-string "#\\x110000"))` | `("invalid hex character escape ~a" ("x110000") 0 #<input-port ...>)` | `[原始msg, ("x110000"), bfp位置, port]` |
| **String 非法转义** | `(read (open-input-string "\"\\q\""))` | `("invalid string character \\~c" (#\q) 2 #<input-port ...>)` | `[原始msg, (#\q), 字符位置, port]` |
| **String hex 值** | `(read (open-input-string "\"\\x110000;\""))` | `("invalid code point value ~s in string hex escape" (1114112) 2 #<input-port ...>)` | `[原始msg, (1114112), 字符位置, port]` |
| **Number 非法语法** | `(read (open-input-string "1.2.3"))` | `("invalid number syntax ~a" ("1.2.3") 0 #<input-port ...>)` | `[原始msg, ("1.2.3"), bfp位置, port]` |
| **Number 不可表示** | `(read (open-input-string "1e10000"))` | `("cannot represent ~a" ("1e10000") 0 #<input-port ...>)` | `[原始msg, ("1e10000"), bfp位置, port]` |
| **Gensym 未闭合** | `(read (open-input-string "#{abc"))` | `#f`（无 irritants） | — |
| **Hash-bang 非法** | `(read (open-input-string "#!unknown"))` | `("invalid syntax #!~a" ("unknown") 0 #<input-port ...>)` | `[原始msg, ("unknown"), bfp位置, port]` |
| **括号不匹配** | `(read (open-input-string ")"))` | `#f`（无 irritants） | — |
| **Dot 错误** | `(read (open-input-string "(1 . 2 3)"))` | `#f`（无 irritants） | — |
| **Vector 长度非法** | `(read (open-input-string "#-1(1)"))` | `("invalid vector length ~s" (-1) 0 #<input-port ...>)` | `[原始msg, (-1), bfp位置, port]` |
| **FXVector 类型错** | `(read (open-input-string "#vfx(1 2.0)"))` | `#f`（无 irritants） | — |
| **Bytevector 符号值** | `(read (open-input-string "#vu8(1 x)"))` | `("invalid value ~:[~s~;~a~] found in bytevector" (#t x) 5 #<input-port ...>)` | `[原始msg, (#t x), 元素位置, port]` |
| **Record 名称** | `(read (open-input-string "#[unknown)"))` | `("unrecognized record name ~s" (unknown) 2 #<input-port ...>)` | `[原始msg, (unknown), record名位置, port]` |
| **Graph 重复标记** | `(read (open-input-string "#1=(1) #1=(2)"))` | `("duplicate mark #~s= seen" (1) 0 #<input-port ...>)` | `[原始msg, (1), mark位置, port]` |
| **Graph 缺失标记** | `(read (open-input-string "#1#"))` | `("mark #~s= missing" (1) 0 #<input-port ...>)` | `[原始msg, (1), 引用位置, port]` |

#### 2.4.5 如何从中提取有用信息

对于一个通过 `with-exception-handler` 捕获到的 condition `c`：

```scheme
(let ([irritants (condition-irritants c)])
  (cond
    [(and (list? irritants) (= (length irritants) 4))
     (let ([original-msg  (list-ref irritants 0)]
           [original-args (list-ref irritants 1)]
           [position      (list-ref irritants 2)]
           [port          (list-ref irritants 3)])
       (format #t "Error at position ~a of ~a: ~?~%"
               position port original-msg original-args))]
    [(list? irritants)
     (format #t "Irritants: ~s~%" irritants)]
    [else
     (format #t "No irritants~%")]))
```

这段代码可以可靠地从绝大多数普通文本端口的读取错误中提取：原始错误模板、原始参数、字符位置和 port 对象。

---

## 三、词法错误（Lexical Errors）

词法错误发生在**单个 token 的扫描过程中**，即字符序列无法被识别为合法的 Scheme token。

### 3.1 EOF 错误（Unexpected EOF while reading token）

**源码函数**：`rd-eof-error`（`s/read.ss:290`）

**触发逻辑**：读到某个 token 的一半时端口到达 EOF。

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#"))` | `unexpected end-of-file reading # prefix` | `("unexpected end-of-file reading ~a" ("# prefix") 0 #<input-port ...>)` |
| `(read (open-input-string "\"hello"))` | `unexpected end-of-file reading string` | `("unexpected end-of-file reading ~a" ("string") 1 #<input-port ...>)` |
| `(read (open-input-string "#\\"))` | `unexpected end-of-file reading character` | `("unexpected end-of-file reading ~a" ("character") 0 #<input-port ...>)` |
| `(read (open-input-string "#| comment"))` | `unexpected end-of-file reading block comment` | `("unexpected end-of-file reading ~a" ("block comment") 0 #<input-port ...>)` |

### 3.2 Sharp-sign 前缀错误

**源码位置**：`rd-token-hash`（`s/read.ss:438`）、`rd-token-hash-num`（`s/read.ss:556`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#z"))` | `invalid sharp-sign prefix #z` | `("invalid sharp-sign prefix #~c" (#\z) 0 #<input-port ...>)` |
| `(read (open-input-string "#1z"))` | `invalid sharp-sign prefix 1z` | `("invalid sharp-sign prefix ~a~a" ("1" #\z) 0 #<input-port ...>)` |
| `(read (open-input-string "#1q"))` | `outdated object file format` | `#f`（无 irritants） |

### 3.3 Boolean 错误

**源码位置**：`rd-token-boolean-rest`（`s/read.ss:490`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#trux"))` | `invalid boolean #trux` | `("invalid boolean #~a~c" ("tru" #\x) 0 #<input-port ...>)` |
| `(read (open-input-string "#t1"))` | `invalid delimiter 1 for boolean` | `("invalid delimiter ~a for ~a" (1 "boolean") 2 #<input-port ...>)` |

### 3.4 Character 错误

**源码位置**：`rd-token-char`（`s/read.ss:594`）、`rd-token-char-hex`（`s/read.ss:634`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#\\unknown"))` | `invalid character name #\unknown` | `("invalid character name #\\~a" ("unknown") 0 #<input-port ...>)` |
| `(read (open-input-string "#\\x110000"))` | `invalid hex character escape x110000` | `("invalid hex character escape ~a" ("x110000") 0 #<input-port ...>)` |
| `(read (open-input-string "#\\777"))` | `invalid character #\777` | `("invalid character #\\~a~a~a" (#\7 #\7 #\7) 0 #<input-port ...>)` |

### 3.5 String 错误

**源码位置**：`rd-token-string`（`s/read.ss:804`）、`rd-token-string-hex-char`（`s/read.ss:903`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "\"\\q\""))` | `invalid string character \q` | `("invalid string character \\~c" (#\q) 2 #<input-port ...>)` |
| `(read (open-input-string "\"\\x110000;\""))` | `invalid code point value 1114112 in string hex escape` | `("invalid code point value ~s in string hex escape" (1114112) 2 #<input-port ...>)` |
| `(read (open-input-string "\"\\\nq\""))` | `unexpected character q after \<intraline whitespace> in string` | `("unexpected character ~c after \\<intraline whitespace> in string" (#\q) 3 #<input-port ...>)` |

### 3.6 Number 错误

**源码位置**：`rd-make-number`（`s/read.ss:953`）、`rd-make-number-or-symbol`（`s/read.ss:920`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） | 额外组件 |
|----------|----------|-----------------------------------|----------|
| `(read (open-input-string "1.2.3"))` | `invalid number syntax 1.2.3` | `("invalid number syntax ~a" ("1.2.3") 0 #<input-port ...>)` | 无 |
| `(read (open-input-string "1e10000"))` | `cannot represent 1e10000` | `("cannot represent ~a" ("1e10000") 0 #<input-port ...>)` | `&implementation-restriction-violation` |

### 3.7 Gensym 错误

**源码位置**：`rd-token-gensym`（`s/read.ss:513`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#{abc"))` | `expected close brace terminating gensym syntax` | `#f`（无 irritants） |

### 3.8 Hash-bang 错误

**源码位置**：`rd-token-hash-bang2`（`s/read.ss:703`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#!unknown"))` | `invalid syntax #!unknown` | `("invalid syntax #!~a" ("unknown") 0 #<input-port ...>)` |

### 3.9 R6RS 模式下的词法限制

**源码位置**：`rd-nonstandard-error`（`s/read.ss:296`）

当端口带有 `port-flag-r6rs` 时，很多 Chez 扩展语法会触发此错误。

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#&box"))`（在 `#!r6rs` 之后） | `#& box syntax is not allowed in #!r6rs mode` | `("~a syntax is not allowed in #!r6rs mode" ("#& box") 0 #<input-port ...>)` |
| `(read (open-input-string ".."))`（在 `#!r6rs` 之后） | `.. symbol syntax is not allowed in #!r6rs mode` | `("~a syntax is not allowed in #!r6rs mode" (".. symbol") 0 #<input-port ...>)` |

---

## 四、语法错误（Syntax / Structural Errors）

语法错误发生在 token 已经正确识别之后，但它们的组合不符合 Scheme 的 S-expression 结构。

### 4.1 括号不匹配错误

**源码位置**：`rd-help`（`s/read.ss:1200`）、`rd-paren-list`（`s/read.ss:1230`）、`rd-brack-list`（`s/read.ss:1271`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string ")"))` | `unexpected close parenthesis` | `#f`（无 irritants） |
| `(read (open-input-string "]"))` | `unexpected close bracket` | `#f`（无 irritants） |
| `(read (open-input-string "(1 2]"))` | `parenthesized list terminated by bracket` | `#f`（无 irritants） |
| `(read (open-input-string "[1 2)"))` | `bracketed list terminated by parenthesis` | `#f`（无 irritants） |

### 4.2 Dot (`.`) 错误

**源码位置**：`rd-help`（`s/read.ss:1225`）、`rd-paren-tail`（`s/read.ss:1244`）、`rd-brack-tail`（`s/read.ss:1285`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "."))` | `unexpected dot (.)` | `#f`（无 irritants） |
| `(read (open-input-string "(1 .)"))` | `expected one item after dot (.)` | `#f`（无 irritants） |
| `(read (open-input-string "(1 . 2 3)"))` | `more than one item found after dot (.)` | `#f`（无 irritants） |
| `(read (open-input-string "(1 . . 2)"))` | `unexpected dot` | `#f`（无 irritants） |

### 4.3 EOF 在结构内部

**源码位置**：`rd-paren-list`、`rd-vector`、`rd-quote` 等

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "(1 2"))` | `unexpected end-of-file reading list` | `("unexpected end-of-file reading ~a" ("list") 4 #<input-port ...>)` |
| `(read (open-input-string "#("))` | `unexpected end-of-file reading vector` | `("unexpected end-of-file reading ~a" ("vector") 2 #<input-port ...>)` |
| `(read (open-input-string "'"))` | `unexpected end-of-file reading quote` | `("unexpected end-of-file reading ~a" ("quote") 1 #<input-port ...>)` |
| `(read (open-input-string "#1="))` | `unexpected end-of-file reading graph mark` | `("unexpected end-of-file reading ~a" ("graph mark") 3 #<input-port ...>)` |

### 4.4 Vector 错误

**源码位置**：`rd-sized-vector`（`s/read.ss:1397`）、`rd-fill-vector`（`s/read.ss:1403`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#3(1 2 3 4)"))` | `too many vector elements supplied` | `#f`（无 irritants） |
| `(read (open-input-string "#-1(1)"))` | `invalid vector length -1` | `("invalid vector length ~s" (-1) 0 #<input-port ...>)` |

### 4.5 FXVector 错误

**源码位置**：`rd-fxvector`（`s/read.ss:1455`）、`rd-fill-fxvector`（`s/read.ss:1473`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#vfx(1 2.0)"))` | `non-fixnum found in fxvector` | `#f`（无 irritants） |
| `(read (open-input-string "#vfx(1 2 3)"))` 后再加一个元素 | `too many fxvector elements supplied` | `#f`（无 irritants） |
| `(read (open-input-string "#-1vfx(1)"))` | `invalid fxvector length -1` | `("invalid fxvector length ~s" (-1) 0 #<input-port ...>)` |

### 4.6 FLVector 错误

**源码位置**：`rd-flvector`（`s/read.ss:1497`）、`rd-fill-flvector`（`s/read.ss:1515`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#vfl(1)"))` | `non-flonum found in flvector` | `#f`（无 irritants） |
| `(read (open-input-string "#vfl(1.0 2.0 3.0)"))` 再加一个 | `too many flvector elements supplied` | `#f`（无 irritants） |

### 4.7 Bytevector 错误

**源码位置**：`rd-bytevector`（`s/read.ss:1539`）、`rd-bytevector-check`（`s/read.ss:1550`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#vu8(1 256)"))` | `non-octet found in bytevector` | `#f`（无 irritants） |
| `(read (open-input-string "#vu8(1 x)"))` | `invalid value x found in bytevector` | `("invalid value ~:[~s~;~a~] found in bytevector" (#t x) 5 #<input-port ...>)` |

> 解释：`(symbol? value)` 返回 `#t`，所以 `~:[~s~;~a~]` 使用 `~a` 分支显示符号名 `x`。

### 4.8 Stencil Vector 错误

**源码位置**：`rd-stencil-vector`（`s/read.ss:1428`）、`rd-fill-stencil-vector`（`s/read.ss:1434`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#vs()"))` | `mask required for stencil vector` | `#f`（无 irritants） |
| `(read (open-input-string "#3vs(1)"))` 元素不足 | `not enough stencil vector elements supplied` | `#f`（无 irritants） |
| `(read (open-input-string "#3vs(1 2 3 4)"))` | `too many stencil vector elements supplied` | `#f`（无 irritants） |
| `(read (open-input-string "#99999vs(1)"))` | `invalid stencil vector mask 99999` | `("invalid stencil vector mask ~s" (99999) 0 #<input-port ...>)` |

### 4.9 Record 错误

**源码位置**：`rd-record`（`s/read.ss:1322`）、`rd-record-tail`（`s/read.ss:1366`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#[123)"))` | `non-symbol found after #[` | `#f`（无 irritants） |
| `(read (open-input-string "#[unknown)"))` | `unrecognized record name unknown` | `("unrecognized record name ~s" (unknown) 2 #<input-port ...>)` |
| `(read (open-input-string "#[date 1)"))`（假设 date 需要更多字段） | `too few fields supplied for record date` | `("too few fields supplied for record ~s" (date) 0 #<input-port ...>)` |
| `(read (open-input-string "#[date 1 2 3 4)"))`（字段过多） | `too many fields supplied for record date` | `("too many fields supplied for record ~s" (date) 0 #<input-port ...>)` |

### 4.10 Graph 标记错误

**源码位置**：`rd-mark`（`s/read.ss:1590`）、`rd-fix-graph`（`s/read.ss:1078`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| `(read (open-input-string "#1=(1) #1=(2)"))` | `duplicate mark #1= seen` | `("duplicate mark #~s= seen" (1) 0 #<input-port ...>)` |
| `(read (open-input-string "#1#"))` | `mark #1= missing` | `("mark #~s= missing" (1) 0 #<input-port ...>)` |

### 4.11 旧的 FASL 格式错误

**源码位置**：`rd-help`（`s/read.ss:1217`）

| 触发代码 | 错误消息 | `condition-irritants`（普通端口） |
|----------|----------|-----------------------------------|
| 读取旧版二进制 fasl 对象 | `unsupported old fasl format detected---use new format with binary i/o` | `#f`（无 irritants） |

---

## 五、入口参数错误（`$oops`）

在进入词法/语法分析之前，参数校验失败会触发 `$oops`，构造的 condition 与词法错误不同：

| 触发代码 | 错误消息 | condition 差异 | `condition-irritants` |
|----------|----------|----------------|-----------------------|
| `(read 123)` | `123 is not a textual input port` | 基础条件是 `&assertion-violation`，不是 `&lexical-violation` | `(123)` |
| `(get-datum (current-output-port))` | `... is not a textual input port` | 同上 | `(#<output-port ...>)` |
| `(let ([p (open-input-string "1")]) (close-port p) (read p))` | `not permitted on closed port ...` | 同上 | `(#<input-port ...>)` |

`$oops` 的 `condition-irritants` 总是包含**导致失败的实际参数值**。

---

## 六、错误位置与 Source 信息的映射

`rd-error` 根据当前读取上下文决定错误消息中位置的呈现方式：

### 6.1 控制台输入

如果端口是 `console-input-port`，错误消息只包含 message 和 port，不显示文件位置。`condition-irritants` 中也不包含位置信息。

### 6.2 普通文本端口（无 source-file-descriptor）

错误消息形如：
- `invalid number syntax 1.2.3 at char 0 of #<input port ...>`

`condition-irritants` 形如：
- `("invalid number syntax ~a" ("1.2.3") 0 #<input-port ...>)`

这里的 `char 0` 对应 `bfp`（当 `start?` = `#t`）或 `fp`（当 `start?` = `#f`）。 irritants 中的第三个元素就是这个位置数字。

### 6.3 带 source-file-descriptor 的端口

如果通过 `get-datum/annotations` 或 `$make-read` 读取，且提供了 `sfd`，则异常中会额外携带 `$&src` 条件：

```scheme
($&src
  src: <source-object>
  start?: #t/#f)
```

- `src` 是通过 `($make-source-object sfd bfp fp)` 构造的，包含了文件名、起始位置、结束位置。
- `start?` = `#t` 表示错误指向 token 的起始位置 `bfp`；`#f` 表示指向当前位置 `fp`。
- 此时 `condition-irritants` **不**包含位置数字（ irritants 只是原始 args 列表）。位置信息应到 `$&src` 条件中去取。

例如：
- `(get-datum/annotations ip sfd 0)` 读取 `#\x110000` 时，`bfp` = 0，`fp` = 8，`$&src` 中的 `start?` = `#t` 表示从 `#` 开始算错误位置。`condition-irritants` = `("x110000")`（原始 args）。

---

## 七、总结速查表

| 错误类型 | 典型触发代码 | 核心 message | `condition-irritants`（普通端口） | 是否有 `&implementation-restriction` | 是否有 `$&src` |
|----------|--------------|--------------|-----------------------------------|--------------------------------------|----------------|
| 非法参数 | `(read 123)` | `~s is not a textual input port` | `(123)` | 否（且基础条件不同） | 否 |
| EOF 中断 | `(read (open-input-string "(1 2"))` | `unexpected end-of-file reading list` | `("unexpected end-of-file reading ~a" ("list") 4 port)` | 否 | 取决于调用方式 |
| 非法 token | `(read (open-input-string "#z"))` | `invalid sharp-sign prefix #z` | `("invalid sharp-sign prefix #~c" (#\z) 0 port)` | 否 | 否 |
| 数字不可表示 | `(read (open-input-string "1e10000"))` | `cannot represent 1e10000` | `("cannot represent ~a" ("1e10000") 0 port)` | **是** | 否 |
| 括号不匹配 | `(read (open-input-string "(1 2]"))` | `parenthesized list terminated by bracket` | `#f` | 否 | 否 |
| Dot 错误 | `(read (open-input-string "(1 . 2 3)"))` | `more than one item found after dot (.)` | `#f` | 否 | 否 |
| 向量长度 | `(read (open-input-string "#3(1 2 3 4)"))` | `too many vector elements supplied` | `#f` | 否 | 否 |
| 类型不匹配 | `(read (open-input-string "#vfx(1.0)"))` | `non-fixnum found in fxvector` | `#f` | 否 | 否 |
| 图标记错误 | `(read (open-input-string "#1#"))` | `mark #1= missing` | `("mark #~s= missing" (1) 0 port)` | 否 | 否 |
| Bytevector 符号值 | `(read (open-input-string "#vu8(1 x)"))` | `invalid value x found in bytevector` | `("invalid value ~:[~s~;~a~] found in bytevector" (#t x) 5 port)` | 否 | 否 |
| Record 名称 | `(read (open-input-string "#[unknown)"))` | `unrecognized record name unknown` | `("unrecognized record name ~s" (unknown) 2 port)` | 否 | 否 |
