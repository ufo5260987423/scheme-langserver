## Type inference in scheme-langserver

A program variable can assume a range of values during the execution of a program. An upper bound of such a range is called a type of the variable. Many languages have functioned a type system to prevent the occurrence of execution errors during the running of a program. However, scheme is not the "typed language" where variables can be given (nontrivial) types: it doesn't have types or, equivalently, have a single universal type that contains all values. 

It is also useful to distinguish between two kinds of execution errors: the ones that cause the computation to stop immediately, and the ones that go unnoticed (for a while) and later cause arbitrary behavior. The former are called trapped errors, whereas the latter are untrapped errors. A program fragment is safe if it does not cause untrapped errors to occur. Languages where all program fragments are safe are called safe languages. Therefore, safe languages rule out the most insidious form of execution errors: the ones that may go unnoticed. Untyped languages may enforce safety by performing run time checks. Typed languages may enforce safety by statically rejecting all programs that are potentially unsafe. Typed languages may also use a mixture of run time and static checks.


|        | Typed | Untyped   |
|--------|-------|-----------|
| Safe   | Java  | Scheme    |
| Unsafe | C     | Assembler |

For scheme, programming would be too frustrating in the absence of both compile time and run time checks to protect against corruption, and scheme-langserver would implement a usable type inferencer instead of a fully decidable one. Unlike [hindley-milner-type-inferencer](https://github.com/webyrd/hindley-milner-type-inferencer) and many other implementations, scheme-langserver's type inferencer have to respond LSP requests which interleave code position information, like in completion, goto definition and many others, and consume [r6rs](http://www.r6rs.org/)-based code without any specific type annotations. Instead, according to [Poly Type Inference in Scheme](https://core.ac.uk/download/pdf/38891838.pdf), the first step is to implement First-order Type System. And, according to [Gradual Typing for Functional Languages](https://www.cs.indiana.edu/~lkuper/talks/gradual/gradual.pdf), it would base on the intuition that the structure of a type may be partially known/unknown at compile-time and the job of the type system is to catch incompatibilities between the known parts of types. 

Further, the known part is following [the summary from csug 9.5](https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0), that we have about 1808 forms to construct a rule-based type inferencer with match` macro.
