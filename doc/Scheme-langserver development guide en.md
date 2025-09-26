# Scheme-langserver Development Guide

This manual aims to guide developers on how to add new language support for Scheme-langserver through `top-environment`, which supports specific language environments (e.g., `R6/7RS`, `S7`). Among these, `R6/7RS` are two standards, while `S7` is an implementation of `R7RS`. These language environments determine various parsing rules at the lexical, syntactic, and grammatical levels, as well as which standard libraries will be loaded. Notably, Scheme language environments often use macros to define how users declare variables and reference libraries, which provides developers with considerable flexibility to add new standards, dialects, and implementations. For users familiar with REPL (Read-Evaluate-Print Loop), this can be likened to a switch that configures the current REPL environment. Its purpose is to inform Scheme-langserver which Scheme language environment should be used to parse code, load libraries, and match rules in the current project. In the REPL, user-entered expressions are parsed and executed based on the libraries and rules of the current environment. Similarly, `top-environment` serves as Scheme-langserver's "environment configuration," ensuring that the language server can accurately simulate a context conforming to the specified standard when processing code.

By reading this manual, you can learn how to implement features such as `goto-definition` and `auto-complete` for new language support and master testing methods for the entire lifecycle. The code structure involved in these implementations is as follows:

- `run.ss`: Parses the command-line arguments that start Scheme-langserver. Developers can start here to understand the overall process.
- `scheme-langserver.sls`: Establishes a Client/Server structure for the LSP Client (usually an editor), isolating trivial tasks like server input and output.
- `analysis/workspace.sls`: Sets up the workspace for parsing code. All contents of this document begin here.
- `analysis/tokenizer.sls`: Utilizes Chez Scheme with slight modifications to create Scheme-langserver's lexer and parser. This implementation has certain limitations in supporting language environments.
- `analysis/package-manager`: Provides file filters for different standards, enabling Scheme-langserver to dynamically generate path rules for acceptable files.
- `analysis/identifier/meta.sls`: Registers all core syntax required by `top-environment`.
- `analysis/identifier/reference.sls`: The core library of Scheme-langserver, determining which code segments a function or macro is valid for.
- `analysis/dependency`: Parses dependencies between code files, supporting library loading and dependency management for different Scheme standards.
- `analysis/abstract-interpreter.sls`: Parses code using abstract interpretation methods.
- `analysis/identifier/rules/` and `analysis/identifier/self-defined-rules/`: Builds indices for code using the `match` macro, primarily assuming that the code is incomplete—partially interpreting the code to gain editing-relevant information.
- `analysis/type/`: Handles type inference, which is an advanced topic and only briefly touched upon in this document.
- `virtual-file-system/`: Contains all data structures needed to build indices for code. Developers can refer to this [paper](https://github.com/ufo5260987423/scheme-langserver/blob/main/doc/paper.pdf) for details. This document also provides an appropriate introduction.

> The code in the `util/` and `protocol/` folders is less relevant to the core content of this document and will be briefly mentioned only when necessary.

## `top-environment` Initialization

This section begins with the `init-workspace` function in the `workspace.sls` file. Its main purpose is to parse the code (`analysis/tokenizer.sls`) and build an index, then set up an environment similar to initializing a REPL for all code files. There are some key aspects to this process. For example, the core of the LSP protocol lies in the fact that the client continuously updates the code, and the index must change accordingly, which is associated with changes in the workspace. Therefore, developers will see the related code split into several branching functions and may not immediately find the corresponding sections in this manual. The author requests readers to maintain some patience and focus on the essential paragraphs.

### Building the Index: `init-virtual-file-system` and `tokenizer.sls`

The first function developers need to search for in `workspace.sls` is `init-virtual-file-system`. It is an abstraction function for the file system in `init-workspace`. The [paper](https://github.com/ufo5260987423/scheme-langserver/blob/main/doc/paper.pdf) provides a detailed explanation of this abstraction. However, this manual draws developers' attention to its parameter `top-environment`. This parameter is passed forward until it ends in the `init-document` function. Before that, the code in `virtual-file-system/file-node.sls` isolates the details of the file system, and this manual will rarely mention it afterward. However, `top-environment` ultimately becomes a parameter in the data structure in `virtual-file-system/document.sls`, determining several aspects. Before proceeding, readers should note the `init-index-node` function. Unlike `top-environment`, it calls `source-file->annotations` in `tokenizer.sls` to obtain Chez Scheme's built-in code indexing structure, `annotation`, and wraps it with an `index-node` data structure in `virtual-file-system/index-node.sls`. Essentially, `index-node` is the essence of the code index. However, it is significantly constrained by Chez Scheme's adherence to the R6RS standard, leading to conflicts with the R7RS standard's lexical and syntactic definitions. Scheme-langserver is bound by these constraints due to performance considerations. Some alternatives might result in 2-3 times the performance overhead.

To address these conflicts, `tokenizer.sls` uses a "degradation" method. Built-in exceptions thrown by Chez Scheme result in some characters being replaced with spaces at the `document.sls` level. The core principle of replacement is not to change the positions of other structures in the code, ensuring that LSP-level interactions are unaffected. Even so, Scheme-langserver maintainers have currently only handled exceptions frequently thrown by Chez Scheme. Users can still encounter strange exceptions under unusual circumstances, leading to Scheme-langserver crashes. If such issues occur, please submit an issue to the developers.

### `top-environment` Initialization: `meta.sls`

At the end of `init-document` execution, the `find-meta` function handles the unfinished business of `top-environment`, initializing a REPL-like environment for each `document`. Developers can observe how the `find-meta` function in `meta.sls` works:
1. The `find-meta` function checks whether type expressions have been initialized through the `initialized?` flag. If not initialized, it calls the `init-type-expressions` function to complete the initialization and sets `initialized?` to `#t`.
2. The `find-meta` function selects different meta-library collections based on the current `top-environment` and corresponding `meta-lib` identifiers.
3. Finally, the information in `meta.sls` is passed to the `document-ordered-reference-list` structure through `make-document`.

To add support for new Scheme standards, developers need to make the following changes:

- Create a comprehensive identifier mapping: Each library definition must include all available identifiers and their types (procedures, syntax, etc.), following the patterns established by existing standards (e.g., `S7`). This step is critical because, in the Core Stage, `analysis\abstract-interpreter.sls` performs analyses based on identifiers. Incomplete identifier definitions will result in some statements not being analyzable.
- Update `init-type-expressions`: Add new library definitions to the library list used for initializing type expressions.
- Define identifier lists for new libraries: Follow the patterns of existing definitions and use `private-process` to create new library definitions.
- Add conditional branches in `find-meta`: Add a new branch in the `cond` expression to handle identifiers of the new standard.

By following these steps, you can add support for new standards in `meta.sls`, ensuring that the language server can correctly parse libraries and syntax rules of the new standards.

### `document-ordered-reference-list` and `reference.sls`

Developers should search for the `find-available-references-for` code in `reference.sls`. It contains the following snippet:

```scheme
(if (null? (index-node-parent current-index-node))
  (private-binary-search (document-ordered-reference-list document) identifier current-exclude)
  (find-available-references-for document (index-node-parent current-index-node) identifier current-exclude))
```

This snippet means that when the information in the `index-node` is insufficient, the function retrieves information from the `document-ordered-reference-list`. This is the true meaning of "initialization." The `find-available-references-for` function processes the visibility of these initialized pieces of information, which is covered in other parts of the function. This content will be introduced one by one later in this document.

### `txt-filter.sls`: Target File Filter

When developing support for new standards, you often need to filter and process related code files for specific standards to make testing and implementation more targeted. For example, during the development of `r7rs` standard compatibility, the `txt-filter.sls` file functions as a dedicated file filter. This module flexibly decides which files or directories should be loaded and parsed by checking file path suffixes and directory attributes. To facilitate management and testing, all test code files related to `r7rs` adopt the `.scm.txt` suffix. Thus, when specifying this file filter in tests (e.g., `tests/analysis/test-workspace.sps`), Scheme-langserver processes only files related to the target standard, avoiding the loading of irrelevant files. This improves testing efficiency and development flexibility.

By flexibly defining file filter rules, target file filters can efficiently identify and load code files related to the current standard, avoiding interference from irrelevant files. This enhances the language server's performance and parsing efficiency. When developing new standards, developers can customize filter rules based on actual needs, such as specifying specific file suffixes, directory structures, or naming conventions, making testing and functional extensions more convenient and targeted. This mechanism not only optimizes workspace resource management but also facilitates the integration and maintenance of subsequent standards.

## Dependency Analysis

Returning to `workspace.sls`, after completing the `init-virtual-file-system` part, the next steps are `init-library-node` and `init-file-linkage`. These functions call `virtual-file-system/library-node.sls` and `analysis/dependency/file-linkage.sls` to preliminarily establish the dependency relationships between code files, which are stored in the `file-linkage-matrix` in `file-linkage.sls`.

### `init-library-linkage` for Recognizing Library Declarations in Code Files

Scheme-langserver assumes Scheme code files fall into two categories: those with library declarations and those without. Both are processed in the `get-library-identifier-list` function called by `init-library-node`. The `top-environment` is passed to `analysis/util.sls` for this purpose:
1. `get-library-identifier-list` selects different `match` statements based on the `top-environment`. These branches correspond to the differences in library declaration syntax for different standards.
2. The `match` macro is used to match and extract library declaration methods based on the standard. For example, in `r6rs`, `library` serves as the keyword for library declarations, so `[('library (name **1) _ ... ) name]` is used to capture the identifiers following the `library` keyword.

In summary, developers following this manual to support new language environments should modify this part first.

### `init-file-linkage` for Recognizing Dependency Relationships

After processing the `library-node`, `init-file-linkage.sls` identifies the dependency relationships between them, representing these as a graph and storing them in the `file-linkage-matrix`. The core function for identifying such relationships is `get-imported-libraries-from-index-node`. Developers can observe the role of `top-environment` in this function:
1. `get-imported-libraries-from-index-node` selects the appropriate `import` handler function based on the `top-environment`.
2. The `import` handler function matches the library declaration keyword and passes the subsequent nodes to the `match-import` function for processing.
3. The `match-import` function uses a `match` statement like `('import dummy **1 )` to filter all `import` nodes and passes them to the `match-clause` function.
4. The `match-clause` function employs a series of `match` macros to match keywords like `only` and `rename` in the `import` statement, ultimately returning the library declarations corresponding to the `import` statements.

It is worth noting that this code only processes `import` structures nested one level within the outermost library declaration of each code file. Deeper nesting levels are handled by `abstract-interpreter.sls`. This further exemplifies the concept of "initialization" explained in this manual. For deeply nested code, even if macros or functions share the same name as `import` or `load`, their semantics may vary depending on the context, diverging from the definitions in `top-environment`. These differences are precisely what developers need to address. The `find-available-references-for` function is specifically designed to handle such distinctions.

### The True Role of `file-linkage`

Returning to `workspace.sls`, after `init-file-linkage`, `get-init-reference-batches` reveals the true role of `file-linkage`. The dependency relationships between files determine the processing order of the abstract interpreter in `abstract-interpreter.sls`. For performance reasons, files without dependency relationships are also processed in parallel. If developers or users find that Scheme-langserver performs poorly on their projects and fails to fully utilize CPU resources, the issue may lie in suboptimal partitioning of code dependencies.

## Abstract Interpretation

The core function in `workspace.sls` is `init-references`. Here, `references` refers to the concept in `find-available-references-for`, which is defined in `references.sls` as the `identifier-reference` structure. This structure underpins all LSP protocol features. For example, when this manual discusses the `goto-definition` feature, it means using `find-available-references-for` to locate the `identifier-reference` corresponding to the `index-node`, and then finding the target via its `identifier-reference-initialization-index-node`. Similarly, when discussing the `auto-complete` feature, it means using `find-available-references-for` to locate the `identifier-reference` visible to the `index-node`. The foundation of these operations is `init-references`.

Developers may also notice that `top-environment` seems to conclude its role here, as it no longer appears in the `init-references` parameter list. However, the influence of `top-environment` permeates the entire process.

### `step`: The Beginning

Developers should search for the `step` function, originating from `abstract-interpreter.sls`, in the execution of `init-references` and the functions it calls. The entire logic of `step` is to traverse the tree formed by `index-node` and determine which rules apply to processing the current `index-node`. These "rules" are directly generated by `establish-available-rules-from`:
1. Its input parameters include `root-file-node` (the root node of the file being analyzed), `root-library-node` (the library node the file belongs to), `file-linkage` (representing file dependencies), `current-document` (the document object being processed), and `current-index-node` (the AST node being processed).
2. Based on a series of conditions, if it determines that a macro indeed comes from the `top-environment`—for instance, the `define` macro defined by `R6/7RS` instead of a user-defined one—it will invoke `analysis/identifier/rules/define.sls` for processing (explained further below).
3. The rules' execution process involves examining the type of the current node, setting exclusions for references or recursive analysis of child nodes based on the type, and using the `establish-available-rules-from` function to dynamically match rules for syntax parsing, identifier resolution, and rule application.

### Example: Processing in `analysis\identifier\rules\define.sls`

1. The function uses the `match` macro to pattern-match the node expression, such as `(_ ((? symbol? identifier) dummy0 ... ) dummy1 ... )`, which can recognize function definitions like `(define (f a b c) ... )`.
2. Once the target element is matched, the function creates a new `identifier-reference` for the `identifier` and marks its type as `procedure`. This serves as the basis for Scheme-langserver's `goto-definition` feature.
3. Subsequently, the function uses `map` to create `identifier-references` for each parameter, marking their type as `parameter`. In function bodies, the `goto-definition` operation for parameters will locate the corresponding formal parameter positions.

### Challenges: User-Defined Macros

To meet the demand for supporting user-defined syntax rules and macro extensions, Scheme-langserver introduces the `self-defined-rules` module. This module provides dynamic rule matching and processing mechanisms to enable parsing, indexing, and semantic analysis for user-defined Scheme syntax. This allows developers to define new syntax forms or extend existing language features based on project requirements.

### Another Example: `self-defined-rules/goldfish/typed-lambda.sls`

`typed-lambda` is a syntax extension unique to Goldfish Scheme. It allows parameters to be specified as lists (with the second element being an assertion function) when creating procedures with `lambda`. This ensures that parameters are automatically checked during invocation, enhancing type safety and robustness. The processing steps for this custom macro are:
1. The function uses the `match` macro to pattern-match the node expression, with a pattern like `(_ (identifier **1) fuzzy ... )`, similar to `define`.
2. For each parameter, the `match` macro is used to process it. If the parameter is a single symbol, it is processed conventionally. If it is a list, the first element is extracted as the actual `identifier-reference`. The processing rules are consistent with those for `define` parameters: creating `identifier-references` and marking their type as `parameter`.

### Adding Support for New Standards

To support new Scheme standards in Scheme-langserver, multiple modules need to be extended and modified. First, new rule modules should be added to the `rules` directory to support the specific syntax constructs of the new standard. Existing rule modules provide independent rule sets for various Scheme standards (e.g., `R6RS`, `R7RS`, `S7`), and new standards should follow a similar modular design to ensure clarity and maintainability.

Next, the `establish-available-rules-from` function in `analysis/abstract-interpreter.sls` must be extended to add rule mappings for the specific syntax of the new standard. This is typically achieved by adding new conditional branches to the function, which check whether the current `top-environment` matches the new standard and load the corresponding rule handlers based on the result. Additionally, the `private:top-env=?` function and related utilities must be updated to recognize the identifiers of the new standard, ensuring that the top-level environment detection system can correctly distinguish between different language standards.

## Type Inference

The `analysis/type` module in Scheme-langserver is responsible for type inference, which provides static type analysis support for Scheme code. Since Scheme is a dynamically typed language, type information is typically not explicitly present in the code. The type inference module deduces the types of code elements through static analysis, enabling features like code completion, definition navigation, and error checking.

The type inference module is implemented using type expressions and rule matching. It supports basic types (e.g., `number?`, `boolean?`), composite types (e.g., `list?`, `vector?`), and user-defined types. By traversing the abstract syntax tree (AST) and combining contextual information with type rules, the module deduces the types of variables, function parameters, and return values.

Type inference is an advanced feature of Scheme-langserver. This document does not delve deeply into its implementation. Developers seeking a deeper understanding of type inference should refer to the code files in the `type` directory or extend type inference rules as needed to support new Scheme standards or project-specific type requirements.

## Testing

Scheme-langserver uses `SRFI :64 testing` as the standardized testing framework. All test files follow a uniform structure and align with the hierarchical organization of the main codebase. Through the `test.sh` script, the testing framework can automatically discover and execute tests, while some tests may be excluded from automated execution due to debugging or performance reasons.

In the Scheme-langserver testing framework, all test files adhere to strict coding standards to ensure readability, consistency, and maintainability. These standards cover file header conventions, import patterns, naming conventions, and the organization of tests.

Each test file includes a standardized file header. All test files uniformly import the `SRFI :64` testing library, which serves as the standardized testing framework for Scheme. Additionally, test files import relevant project modules as needed, such as `analysis/`, `protocol/`, and `virtual-file-system/`. The naming of test files and test functions follows these conventions: the name of the test file matches the name of the module being tested. This naming convention makes the purpose of the test files and functions immediately apparent, facilitating quick problem identification for developers.

### Test Execution Method

The Scheme-langserver project integrates an automated testing script, `test.sh`. Developers simply need to place custom test files in the `tests` directory, and `test.sh` will automatically traverse the directory, discover all test files, and execute them sequentially. During the test process, the script collects and summarizes test results, reporting the execution status of each test file, including passes, failures, and error messages. This allows developers to quickly identify issues, ensuring the compatibility and stability of new features or standards, without manually managing the testing process. Note that the testing script requires the `akku` environment to be activated beforehand.

```bash
akku install
bash .akku/env
bash test.sh
```

### Log Debugging Tool

During the actual operation of the langserver, unexpected errors may occur. To address this, Scheme-langserver provides a log debugging tool that helps developers read log files and reproduce operational scenarios, facilitating debugging and analysis of server behavior. This tool can parse log content, generate input streams compliant with the LSP protocol, and simulate the interaction between the client and server, thereby effectively identifying and resolving issues. This section briefly introduces how to use the log debugging tool for log debugging. For detailed information, refer to `doc/how-to-debug.md`.

Developers can use the log debugging tool by running the test script (`log-debug.sps`). The script automatically reads the log file, generates an input stream, and initializes a server instance. Upon test completion, the tool outputs the test results, including the server's response status and details of the log reproduction.

When debugging error logs, developers first need to confirm the path of their log output (refer to relevant options in the VSCode plugin configuration) to ensure that log files are correctly generated and saved to the specified location. When encountering errors or abnormal behavior, the corresponding log content for the time period should be copied into the `ready-for-analyse.log` file. Developers can then use the log debugging tool provided by Scheme-langserver to analyze and replay the log file. This tool automatically parses the log content, simulates the interaction between the client and server, and enables developers to repeatedly reproduce error scenarios. Through multiple reproductions and analyses, developers can precisely locate the root cause of the issue, verify the effectiveness of the fix, and ensure the stability and compatibility of the related features. This process significantly improves debugging efficiency and reduces the complexity of troubleshooting, making it the recommended standard debugging method in the Scheme-langserver project.

Through the above testing framework and methods, Scheme-langserver can comprehensively verify the functionality and stability of the codebase, providing reliable testing support for developers. When developing new features or extending support for new standards, developers should refer to existing test files and promptly supplement test cases for new modules, syntax rules, and edge cases. A complete testing system not only helps uncover potential issues but also ensures the long-term maintainability and compatibility of the project.

### Tests for `R7RS`, `S7`, and `Goldfish`

During the development of `R7RS`, `S7`, and `Goldfish` standard compatibility, I supplemented numerous test files covering core functionalities such as new standard initialization, library loading, dependency management, and macro extensions. These test files are primarily distributed in the `tests` directory and are written to target the characteristics of each standard. They focus on verifying the correctness and stability of newly added modules and rules. These tests provide strong assurance for the integration and subsequent maintenance of new standards.

- `tests/resources/r7rs/`: Testing resource directory. Since the `R7RS/S7` standards differ in compatibility from Scheme-langserver's default `R6RS` standard, a new package management mechanism was introduced during testing, along with dedicated test files. Most subsequent tests rely on the resource files in this directory for validation.
- `tests/analysis/test-workspace.sps`: Verifies the initialization process of the workspace. Test cases related to new standards were added to ensure correct workspace initialization under different `top-environments`.
- `tests/analysis/dependency/rules/test-library-import.sps`: Verifies whether Scheme-langserver correctly parses library import statements under different `top-environments`. The main focus is to determine whether, given a specific `top-environment` and file, the identifiers introduced by `import` statements can be correctly parsed.
- `tests/analysis/dependency/test-file-linkage.sps`: Tests whether Scheme-langserver correctly parses dependency relationships under new standards. Since dependency relationships are stored as graph structures, this test determines correctness by verifying whether paths exist between dependent files.
- `tests/analysis/test-abstract-interpreter.sps`: Verifies the correctness of the abstract interpreter when handling library imports, identifier parsing, and syntax rule applications. This test is crucial because `abstract-interpreter.sls` is the core of the entire analysis engine and forms the basis for intelligent editing features (e.g., code completion, definition navigation) provided by Scheme-langserver.
- `tests/analysis/identifier/rules/test-define*.sps`: Tests the correctness of `define*` syntax parsing. `define*` is an extension of the `define` syntax in the `S7` standard, enabling the definition of functions with optional parameters.
- `tests/analysis/identifier/rules/s7/test-define-macro.sps`: Tests the correctness of `define-macro` syntax parsing. `define-macro` is a structure in the `S7` standard for defining macros, allowing programmers to create rules for code transformation during compilation.
- `tests/analysis/identifier/self-defined-rules/goldfish/test-typed-lambda.sps`: Tests the correctness of `typed-lambda` syntax parsing. `typed-lambda` is a macro in Goldfish Scheme for defining `lambda` functions with type checking. It allows specifying type predicates for each parameter in the parameter list, automatically checking the types during invocation and reporting errors if mismatched.
- `tests/analysis/identifier/self-defined-rules/goldfish/test-let1.sps`: Tests the correctness of `let1` syntax parsing. `let1` is a macro in Goldfish Scheme for simplifying the `let` binding syntax in Scheme.
- `tests/analysis/identifier/self-defined-rules/goldfish/test-define-case-class.sps`: Tests the correctness of `define-case-class` syntax parsing. `define-case-class` is a macro in Goldfish Scheme for defining class-like data structures in Scheme, supporting fields, type checking, instance methods, static methods, and other object-oriented features.
- `tests/analysis/identifier/self-defined-rules/goldfish/test-define-class.sps`: Tests the correctness of `define-class` syntax parsing. `define-class` is a syntactic sugar extension of `define-case-class` that automatically generates variables, `getter`, and `setter` for each field, and supports type checking and default values.

Through extensive test supplementation, I verified the correctness of the newly added features, ensuring their stability and compatibility. This not only validated the effectiveness of the implementation but also laid a solid foundation for the ongoing development and extension of Scheme-langserver, providing strong assurance for its maintenance and support for new standards.

## Conclusion

This manual provides a detailed overview of the architecture and extension methods of Scheme-langserver, helping developers understand how to add new standards, optimize existing features, and ensure the stability of the codebase. We welcome all developers interested in Scheme-langserver to join us in improving this project and contributing to the Scheme community!

## References

- Scheme-langserver Repository: [https://github.com/ufo5260987423/scheme-langserver](https://github.com/ufo5260987423/scheme-langserver)
- Chez Scheme User Manual: [https://cisco.github.io/ChezScheme/csug/csug.html](https://cisco.github.io/ChezScheme/csug/csug.html)
- LSP Official Documentation: [https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)