# Scheme-langserver 开发指南

本手册旨在向开发者说明如何为 Scheme-langserver 添加新语言支持，通过 `top-environment` 支持特定的语言环境（如 `R6/7RS`、`S7`）。其中 `R6/7RS` 是两种标准，`S7`则是一种`R7RS`实现。这些语言环境决定了词法、句法、语法层面的若干解析规则，也决定了什么样的标准库将被加载。特别的，Scheme 语言环境往往通过宏确定用户声明变量、引用库的方式，因此开发者扩展对 `top-environment` 的支持，将有相当大的自由度添加新的标准、方言、实现。这对熟悉 REPL（Read-Evaluate-Print Loop）的用户来说，可以类比为一个配置当前 REPL 环境的开关，它的作用是告诉 Scheme-langserver 当前项目应该使用哪种 Scheme 语言环境来解析代码、加载库和匹配规则。在 REPL 中，用户输入的表达式会根据当前环境加载对应的库和规则进行解析和执行。同样，`top-environment` 就是 Scheme-langserver 的“环境配置”，它确保语言服务器在处理代码时，能够正确地模拟出一个符合指定标准的上下文。

阅读本手册，您可以对这些新语言支持实现 `goto-defninition`、`auto-complete` 等功能，并掌握对整个生命周期的测试方法。这些内容设计的代码结构如下：

- `run.ss`: 解析启动 Scheme-langserver 的命令行参数，开发者可以从这里入手了解整个流程；
- `scheme-langserver.sls`: 面向 LSP Client（一般是编辑器）建立一个 Client/Server 结构，把服务器输入输出等琐碎工作隔绝在外；
- `analysis/workspace.sls`: 建立解析代码的工作区，本文档所有内容从这里开始；
- `analysis/tokenizer.sls`: 利用 Chez Scheme 稍加修改，得到 Scheme-langserver 的 lexer 和 parser，这里对语言环境的支持存在少量限制；
- `analysis/package-manager`: 为不同标准提供文件过滤器，使得 Scheme-langserver 可以动态生成可接受文件的路径规则。
- `analysis/identifier/meta.sls`: 注册 `top-environment` 需要的所有核心语法等；
- `analysis/identifier/reference.sls`: 整个 Scheme-langserver 最核心的库，它确定了一个函数、宏究竟对哪一段代码有效；
- `analysis/dependency`: 用于解析代码文件之间的依赖关系，支持不同 Scheme 标准的库加载和依赖管理。
- `analysis/abstract-intepreter.sls`: 使用抽象解释方法解析代码；
- `analysis/identifier/rules/`和`analysis/identifier/self-defined-rules/`: 通过 `match` 宏对代码建立索引，这里主要假设代码是不完整——也就是说对代码进行部分解释，以获得对编辑有用的信息；
- `analysis/type/`: 类型推断相关代码，属于较为高级的内容，本文档仅有限涉及；
- `virtual-file-system/`: 对代码建立索引所需要的全部数据结构，可参考[论文](https://github.com/ufo5260987423/scheme-langserver/blob/main/doc/paper.pdf)，本文也将适当介绍；

> 在`util/`、`protocol/` 文件夹下的代码与本文核心内容关系较远，仅在有必要时略加引用及介绍。

## `top-environment` 初始化 

本阶段将从`workspace.sls`文件的`init-workspace`函数开始，它整个的目的是解析代码（`analysis/tokenizer.sls`）并建立一个索引，然后为所有的代码文件设定一个类似初始化 REPL 的环境。这其中有一些关窍，即LSP协议的核心在于客户端将随时更新这些代码，而索引不得不随之发生变动，并且关联工作区的变动。因此，开发者将看到相关代码冗长的分成了若干个分支函数而并不能立刻在本手册中找到对应的章节。作者在这里请求诸位保持一些耐心，并且把注意力集中在一些重点的段落。

### 建立索引：`init-virtual-file-system` 和 `tokenizer.sls`

开发者第一个需要对`workspace.sls`搜索的函数是`init-virtual-file-system`，它是`init-workspace`对文件系统的抽象函数。[论文](https://github.com/ufo5260987423/scheme-langserver/blob/main/doc/paper.pdf) 对抽象的过程进行了详细的介绍，不过本手册提醒开发者注意其参数`top-environment`，这个参数传递将在`init-document`函数中结束：在此之前，`virtual-file-system/file-node.sls`相关的代码将文件系统的细节隔离在外，本手册后文基本不会再见到它；而`top-environment`最终成为了`virtual-file-system/document.sls`中的数据结构的一个参数，并且决定一些事情。不过在此之前，请读者先注意到函数`init-index-node`：它并没有`top-environment`，它调用了`tokenizer.sls`中的`source-file->annotations`获得了Chez Scheme内置的代码索引结构`annotation`，并调用`virtual-file-system/index-node.sls`在外面包裹了一层`index-node`数据结构。可以说，`index-node`就是代码索引的本体，但它相当程度上屈从于客观的 Chez Scheme 的一些限制，即 Chez Scheme 服从R6RS标准，这导致R7RS标准的词法、句法定义存在相当程度的冲突。Scheme-langserver 不得不服从这些限制的动因是性能，一些可能的替代品可能导致2-3倍的性能开销。

为了处理这些冲突，`tokenizer.sls`采用“降级”的方法进行了处理，Chez Scheme抛出的内置异常将导致一些字符在`document.sls`层面被替换为空格。因为替换的核心原则是不改变代码中其他结构的位置，这样LSP层面的交互将不受到影响。绕是如此，Scheme-langserver维护者们目前仅对对Chez Scheme经常抛出的异常进行了处理，用户仍然在各种奇怪的情况下引发奇怪的异常，继而导致Scheme-langserver崩溃。如果出现了这样的问题，请向开发者提交issue。

### `top-environment` 初始化: `meta.sls`

在`init-document`执行的末段，`find-meta`函数将处理`top-environment`的未竟事宜，也就是为每个`document`初始化 REPL 类似的环境。开发者可以看到 `meta.sls` 中 `find-meta` 函数是如何工作的：
1. `find-meta` 函数内部通过 `initialized?` 标志检查是否已初始化类型表达式。如果未初始化，调用 `init-type-expressions` 函数完成初始化，并将 `initialized?` 设置为 `#t`。
2. `find-meta` 函数根据当前的 `top-environment` 和对应的 `meta-lib` 标识符选择不同的元库集合
3. 最后，`meta.sls`中的信息将通过`make-document`进入`document-ordered-reference-list`结构。

要为新的 Scheme 标准添加支持，开发者需要进行如下修改：

- 创建全面的标识符映射：每个库定义都必须包含所有可用的标识符及其类型（过程、语法等），遵循现有标准（如 S7）建立的模式。这一步非常重要，因为在 Core Stage 中，`analysis\abstract-interpreter.sls` 根据标示符进行分析，如果标示符定义不全，将导致某些语句无法被分析。
- 更新 `init-type-expressions`：将新的库定义加入类型表达式初始化所用的库列表。
- 为新库定义标识符列表：参照现有定义的模式，使用 `private-process` 创建新的库定义。
- 在 `find-meta` 中新增条件分支：在 `cond` 表达式中添加一个新的分支，以处理新的标准标识符。

通过以上步骤，可以在 `meta.sls` 中添加对新标准的支持，确保语言服务器能够正确解析新标准的库和语法规则。

### `document-ordered-reference-list` 和 `reference.sls`

请开发者在`reference.sls`中搜索`find-available-references-for`代码，其中有这么一段：

```scheme
(if (null? (index-node-parent current-index-node))
  (private-binary-search (document-ordered-reference-list document) identifier current-exclude)
  (find-available-references-for document (index-node-parent current-index-node) identifier current-exclude))
```

这段的意思是，当 `index-node` 中的信息不够充分，函数将从 `document-ordered-reference-list` 获得信息，这也是“初始化”的真正含义。而`find-available-references-for`将处理这些初始化以后的信息的可见性，也就是该函数的其他部分。这些内容本手册将在后文一一介绍。

### `txt-filter.sls`: 目标文件筛选器

在开发新标准支持时，往往需要针对特定标准筛选和处理相关代码文件，以便测试和功能实现更具针对性。例如，在 `r7rs` 标准兼容性开发过程中，`txt-filter.sls` 文件充当了专用的文件过滤器。该模块通过检查文件路径的后缀名和目录属性，灵活决定哪些文件或目录应被加载和解析。为便于管理和测试，所有与 `r7rs` 相关的测试代码文件统一采用 `.scm.txt` 后缀。这样，在测试（如 `tests/analysis/test-workspace.sps`）中指定该文件过滤器后，Scheme-langserver 只会处理目标标准的相关文件，避免了无关文件的加载，提高了测试效率和开发灵活性。

通过灵活定义文件筛选规则，目标文件筛选器能够高效地识别和加载与当前标准相关的代码文件，避免无关文件的干扰，从而提升语言服务器的性能和解析效率。在开发新标准时，开发者可以根据实际需求自定义筛选器规则，例如指定特定的文件后缀、目录结构或命名约定，使测试和功能扩展更加便捷和有针对性。这种机制不仅优化了工作区的资源管理，也为后续标准的集成和维护提供了便利。

## 依赖解析

现在回到`workspace.sls`，`init-virtual-file-system`的部分已经做完了，要做的是`init-library-node`和`init-file-linkage`。它的作用是调用`virtual-file-system/library-node.sls`和`analysis/dependency/file-linkage.sls`初步建立代码文件之间的依赖关系，并保存在`file-linkage.sls`中的`file-linkage-matrix`中。

### `init-library-linkage`识别代码文件的库声明
Scheme-langserver假设Scheme代码文件有两种，即一种是有库声明的，而另一种是没有的。它们都需要在`init-library-node`调用的`get-library-identifier-list`中处理。可以看到`top-environment`经传递到达了`analysis/util.sls`：
1. `get-library-identifier-list` 通过不同的 `top-environment` 选择不同的 `match` 语句。这一分支同时也对应不同标准定义库的语法不同。
2. `match` 宏根据不同标准对库声明方法进行匹配并提取。如 `r6rs` 以 `library` 作为库声明的关键字，因此使用 `[('library (name **1) _ ... ) name]` 对 `library` 关键字后的标示符进行捕捉；

也就是说，开发者遵循本手册支持新的语言环境，需要首先修改这一部分。

### `init-file-linkage`识别依赖关系
`library-node`经过识别完毕，`init-file-linkage.sls`将其之间的依赖关系识别为一个图并存储在`file-linkage-matrix`中。识别这种依赖关系的核心函数是`get-imported-libraries-from-index-node`，开发者可以在此看到`top-environment`的作用：
1. `get-imported-libraries-from-index-node` 根据 `top-environment` 选择对应的 `import` 处理函数。
2. `import` 处理函数在匹配到库声明关键字后，将关键字后续的节点传递给 `match-import` 函数进行处理。
3. `match-import` 函数使用 `('import dummy **1 )` 形式的 `match` 语句，过滤所有 `import` 节点传递给 `match-clause` 函数处理。
4. `match-clause` 函数最终使用一系列 `match` 宏来匹配诸如 `only`, `rename` 等 `import` 关键字，并最终返回 `import` 所对应的库声明。

当然这里要额外注意一点，就是这些代码之处理了各个代码文件中*与最外层库声明嵌套一层的的`import`等结构*，对于更深层次的嵌套需要等到`abstract-interpreter.sls`来处理。这再次实现了本手册关于“初始化”的解释，因为对于嵌套层次更深的代码，即便具有与`import`、`load`等相同的宏名称或者函数名称，其语义可能受到上下文影响发生了变化，与`top-environment`的定义有所区别——这些区别恰恰是程序员开发的意义。`find-available-references-for`恰恰就是用来处理这些区别的。

### `file-linkage`的真正作用

再次回到`workspace.sls`，在`init-file-linkage`后`get-init-reference-batches`揭示了`file-linkage`的真正作用，即文件之间的依赖关系将决定后文抽象解释`abstract-interpreter.sls`的处理顺序：为了性能的因素，没有依赖关系的文件也采用并行化方法进行了处理。如果开发者和用户发现 Scheme-langserver 在自己的项目上表现不佳且没有很好的利用全部CPU性能，问题可能在于你没有良好的划分代码之间的依赖关系。

## 抽象解释

`workspace.sls`中真正的重头戏在于`init-references`函数，这里的`references`特指`find-available-references-for`中的含义，即`references.sls`中的`identifier-reference`结构，它支撑了 LSP 协议的全部功能：当本手册在说 `goto-definition` 功能的时候，本手册意思是通过`find-available-references-for`找到`index-node`对应的`identifier-reference`，并且通过其`identifier-reference-initialization-index-node`找到跳转目标；当本手册再说 `auto-complete` 功能的时候，本手册意思是通过`find-available-references-for`找到`index-node`可见的`identifier-reference`。而这些工作的基础是`init-references`。

开发者同时可以注意到，`top-environment`到这里似乎已经结束了，它不再出现在`init-references`的参数列表里面。但是，`top-environment`的影响将无处不在。

### `step`:开始
请开发者在 `init-references` 函数执行及它调用的函数的执行过程中搜索 `step` 函数，这个函数来自 `abstract-interpreter.sls`。它的全部逻辑是遍历`index-node`形成的树，并判断何种规则适用于处理当前的`index-node`。而“规则”直接由`establish-available-rules-from`生成：
1. 它的输入参数有：`root-file-node`，表示当前正在分析的文件的根节点；`root-library-node`，表示当前文件所属的库节点；`file-linkage`，表示文件之间的依赖关系；`current-document`，表示当前正在处理的文档对象；`current-index-node`，表示当前正在处理的 AST 节点。。
2. 通过一系列条件，如果它判断一个宏确实来自 `top-environment`，表达 `top-environment`确定的语义，比如`define`，的确是 `R6/7RS` 定义的 `define`，不是用户自己定义的，那么它将调用 `analysis/identifier/rules/define.sls` 进行处理，处理过程见下文；
3. 它的规则的执行过程为：首先该函数检查当前节点的类型，并根据类型设置排除引用或调用子节点的递归分析；对于具有子节点的表达式，`step` 会通过 `establish-available-rules-from` 函数动态匹配规则，为语法解析、标识符解析和规则应用提供支持。

### 例:`analysis\identifier\rules\define.sls` 的处理过程
1. 函数通过 `match` 宏对节点表达式进行模式匹配，例如 `(_ ((? symbol? identifier) dummy0 ... ) dummy1 ... )`，可用于识别如 `(define (f a b c) ... )` 这样的函数定义形式。
2. 匹配到目标元素后，函数会为 `identifier` 新建一个 `identifier-reference`，并标记其类型为 `procedure`。这正是 Scheme-langserver 实现 `goto-definition` 跳转目标的依据。
3. 随后，函数利用 `map` 对参数名逐一新建 `identifier-reference`，类型标记为 `parameter`。在函数体内跳转时，参数的 `goto-definition` 操作将定位到对应的形参位置。

### 力有不逮：用户自定义宏
为了满足 Scheme-langserver 对用户自定义语法规则和宏扩展的支持需求，Scheme-langserver 引入了 `self-defined-rules` 模块。`self-defined-rules` 模块用于支持用户自定义的语法规则和宏扩展。它的主要作用是通过动态规则匹配和处理机制，为用户定义的 Scheme 语法提供解析、索引和语义分析支持，这使得开发者可以根据项目需求定义新的语法形式或扩展现有的语言特性。

### 另一个例子：`self-defined-rules/goldfish/typed-lambda.sls`
`typed-lambda` 是 Goldfish Scheme 独有的语法扩展。它允许在使用 `lambda` 创建过程时，将形参指定为一个列表，其中第二个元素为断言函数。这样，定义的过程在调用时会自动对传入参数进行断言检查，确保参数符合预期条件，从而提升代码的类型安全性和健壮性。该自定义宏的处理过程为：
1. 函数通过 `match` 宏对节点表达式进行模式匹配，匹配模式为 `(_ (identifier **1) fuzzy ... )`，与 `define` 的处理方式类似。
2. 对于每一个形参，使用 `match` 宏进行匹配：若形参为单个符号，则按常规处理；若为列表，则提取列表中的第一个元素作为实际的 `identifier-reference`。处理规则与 `define` 参数一致：为对应节点新建 `identifier-reference`，并将类型标记为 `parameter`。

### 如何加入新标准

为了在 Scheme-langserver 中支持新的 Scheme 标准，需要对多个模块进行扩展和修改。首先，需要在 `rules` 目录中添加新的规则模块，以支持新标准的特定语法构造。现有的规则模块为不同的 Scheme 标准（如 `R6RS`、`R7RS`、`S7`）提供了独立的规则集，新标准也需要类似的模块化设计，以确保规则的清晰性和可维护性。

其次，需要扩展 `analysis/abstract-interpreter.sls` 中的 `establish-available-rules-from` 函数，为新标准的特定语法形式添加规则映射。这通常通过在函数中添加新的条件分支来实现，这些分支会检查当前的 `top-environment` 是否匹配新标准，并根据匹配结果加载对应的规则处理器。此外，还需要更新 `private:top-env=?` 函数及相关工具，使其能够识别新标准的标识符，从而在顶层环境检测系统中正确区分不同的语言标准。

## 类型推断
`analysis/type` 是 Scheme-langserver 中负责类型推断的模块，主要用于为 Scheme 代码提供静态类型分析支持。由于 Scheme 是动态类型语言，类型信息通常不会显式地出现在代码中，因此类型推断模块的作用是通过静态分析推导出代码中的类型信息，从而为代码补全、跳转定义和错误检查等功能提供支持。

类型推断模块的实现基于类型表达式和规则匹配，支持基本类型（如 `number?`、`boolean?`）、复合类型（如 `list?`、`vector?`）以及用户自定义类型。它通过遍历抽象语法树（AST），结合上下文信息和类型规则，推导出变量、函数参数和返回值的类型。

类型推断是 Scheme-langserver 的高级功能，本文档对其不作深入解释。开发者如需深入了解类型推断的实现细节，可参考 `type` 目录下的代码文件，或结合实际需求扩展类型推断规则，以支持新的 Scheme 标准或项目特定的类型需求。

## 测试

Scheme-langserver 使用 `SRFI :64 testing` 作为标准化测试框架，所有测试文件遵循统一的结构，并与主代码库的层级组织保持一致。通过 `test.sh` 脚本，测试框架能够自动发现和执行测试，同时部分测试因调试或性能原因被排除自动化运行。

在 Scheme-langserver 的测试框架中，所有测试文件都遵循严格的代码规范，以确保测试代码的可读性、一致性和可维护性。这些规范涵盖了文件头部标准、导入模式、命名约定以及测试组织方式。

每个测试文件都包含一个标准化的文件头部，所有测试文件统一导入 `SRFI :64` 测试库，这是 Scheme 的标准化测试框架。此外，测试文件还会根据需要导入项目的相关模块，例如 `analysis/`、`protocol/` 和 `virtual-file-system/` 等模块。同时，测试文件和测试函数的命名遵循以下约定：测试文件的名称与被测试模块的名称保持一致。这种命名约定使得测试文件和测试函数的作用一目了然，便于开发者快速定位问题。

### 测试运行方法

Scheme-langserver 项目已集成自动化测试脚本 `test.sh`。开发者只需将自定义测试文件放入 `tests` 目录，`test.sh` 会自动遍历该目录，发现所有测试文件并依次执行。测试过程中，脚本会收集并汇总测试结果，报告每个测试文件的执行情况，包括通过、失败及错误信息。这样，开发者能够快速定位问题，确保新增功能或标准的兼容性和稳定性，无需手动管理测试流程。但注意运行测试脚本之前需要首先激活 `akku` 环境。

```bash
akku install
bash .akku/env
bash test.sh
```

### 日志调试工具

在 langserver 实际运行过程中，难免会遇到一些意外错误。为此，Scheme-langserver 提供了日志调试工具，帮助开发者读取日志文件并重现操作场景，便于调试和分析服务器行为。该工具能够解析日志内容，生成符合 LSP 协议的输入流，模拟客户端与服务器的交互过程，从而有效定位和解决问题。本小节简单介绍如何使用日志调试工具对日志进行调试，详细的内容请参考 `doc/how-to-debug.md`。

开发者可以通过运行测试脚本（`log-debug.sps`）来使用日志调试工具。脚本会自动读取日志文件，生成输入流并初始化服务器实例。测试完成后，工具会输出测试结果，包括服务器的响应状态和日志重现的详细信息。

针对错误日志进行调试时，开发者首先需要确定自己的日志输出路径（可参考 VSCode 插件的相关选项配置），确保日志文件能够正确生成并保存于指定位置。当遇到错误或异常行为时，应将对应时间段的日志内容复制到 `ready-for-analyse.log` 文件中。随后，开发者可利用 Scheme-langserver 提供的日志调试工具，对该日志文件进行分析和重放。该工具会自动解析日志内容，模拟客户端与服务器的交互过程，帮助开发者反复重现错误场景。通过多次重现和分析，开发者能够精准定位问题根源，验证修复效果，并确保相关功能的稳定性和兼容性。这一流程极大提升了调试效率，降低了排查复杂问题的难度，是 Scheme-langserver 项目中推荐的标准调试方法。

通过以上测试框架和方法，Scheme-langserver 能够全面验证代码库的功能和稳定性，为开发者提供可靠的测试支持。开发者在开发新功能或扩展新标准时，应参考现有测试文件，及时补充针对新增模块、语法规则和边界情况的测试用例。完善的测试体系不仅有助于发现潜在问题，还能保障项目的长期可维护性和兼容性。

### 针对`R7RS`、`S7`、`Goldfish`的测试
在开发 `R7RS`、`S7`、`Goldfish` 标准兼容的过程中，我补充了大量测试文件，覆盖了新标准的初始化、库加载、依赖管理、宏扩展等核心功能。测试文件主要分布在 `tests` 目录下，针对各标准的特性分别编写，重点验证了新增模块和规则的正确性及稳定性。这些测试为新标准的集成和后续维护提供了可靠保障。

- `tests/resources/r7rs/`：测试资源目录。由于 `R7RS/S7` 标准与 Scheme-langserver 默认的 `R6RS` 标准存在兼容性差异，测试过程中引入了新的包管理机制，并补充了专门的测试文件。后续大部分测试均依赖该目录下的资源文件进行验证。
- `tests/analysis/test-workspace.sps`：用于验证工作区初始化流程。针对新增的标准，补充了相关测试用例，确保在不同 `top-environment` 下工作区能够正确初始化。
- `tests/analysis/dependency/rules/test-library-import.sps`：验证 Scheme-langserver 在不同 `top-environment` 下对库导入语句的解析处理是否正确。测试主要判断对于给定的 `top-environment` 和文件的情况下，能否正确解析 `import` 语句所引入的标示符。
- `tests/analysis/dependency/test-file-linkage.sps`：测试新标准下 Scheme-langserver 对依赖关系的解析处理是否正确。由于依赖关系使用图结构存储，本测试通过判断依赖文件之间是否存在路径来判断解析处理是否正确。
- `tests/analysis/test-abstract-interpreter.sps`：验证抽象解释器在处理库导入、标识符解析和语法规则应用时的正确性。该测试非常重要，因为 `abstract-interpreter.sls` 是整个分析引擎的核心，是 Scheme-langserver 提供智能编辑功能（如代码补全、跳转定义）的基础。
- `tests/analysis/identifier/rules/test-define*.sps`：测试 `define*` 语法解析是否正确。`define*` 是 `s7` 标准对 `define` 的一个扩展版本，它使得定义带有可选参数。
- `tests/analysis/identifier/rules/s7/test-define-macro.sps`：测试 `define-macro` 语法解析是否正确。`define-macro` 是 `s7` 标准用于定义宏的结构，它允许程序员创建在编译时转换代码的规则。
- `tests/analysis/identifier/self-defined-rules/goldfish/test-typed-lambda.sps`：测试 `typed-lambda` 语法解析是否正确。`typed-lambda` 是 Goldfish Scheme 中的一个宏，用于定义带类型检查的 `lambda` 函数。它允许你在参数列表中指定每个参数的类型谓词，调用时会自动检查类型，不符则报错。
- `tests/analysis/identifier/self-defined-rules/goldfish/test-let1.sps`：测试 `let1` 语法解析是否正确。`let1` 是 Goldfish Scheme 中的一个宏，用于简化 Scheme 中的 `let` 绑定语法。
- `tests/analysis/identifier/self-defined-rules/goldfish/test-define-case-class.sps`：测试 `define-case-class` 语法解析是否正确。`define-case-class` 是 Goldfish Scheme 中的一个宏，用于在 Scheme 中定义类似“类”的数据结构，支持字段、类型检查、实例方法、静态方法等面向对象特性。
- `tests/analysis/identifier/self-defined-rules/goldfish/test-define-class.sps`：测试 `define-class` 语法解析是否正确。`define-case-class` 的语法糖扩展，自动为每个字段生成变量、`getter`、`setter`，并支持类型检查和默认值。

通过大量的测试补充，我验证了新增功能的正确性，确保了功能的稳定性和兼容性。这不仅验证了功能的实现效果，也为 Scheme-langserver 的持续开发和扩展打下了坚实的基础，为后续的维护和新标准支持提供了可靠保障。

## 总结

本手册详细介绍了 Scheme-langserver 的架构和扩展方法，帮助开发者理解如何添加新标准、优化现有功能，并确保代码库的稳定性。我们欢迎所有对 Scheme-langserver 感兴趣的开发者加入，共同完善这一项目，为 Scheme 社区贡献力量！

## 参考链接

- Scheme-langserver 代码库：https://github.com/ufo5260987423/scheme-langserver
- chez scheme 用户手册：https://cisco.github.io/ChezScheme/csug/csug.html
- LSP 官方文档：https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/
