## Detailed Relases Log
1.2.2 I just fixed some bugs processing my own other projects. 

1.2.1 I just fixed some bugs processing SS/SCM codes. 

1.2.0 It has never occurred to me the next medium version comes so fast. But it is still essential to claim that I just re-construct the identifier catching mechanism with abstract interpreter, which will allow processing identifier claims in self-defined macros. A detailed outline should be referred in [documentation](#detailed-document).

1.1.1: Scheme-langserver now releases type information used in corresponding libraries! Its soundness is still not guaranteed! 

1.1.0: Type inference has been embedded into autocompletion! And it uses a homemade DSL(Domain Specific Language) making type representation and interpreting much easier.  But, I actually do not recommend anyone use this type inference in production because there are many efficiency and soundness problems which I haven't solved. A detailed outline should be referred in documentations.

1.0.13: Fix bug: sometimes can't shutdown server. Optimization: re-construct document-sync mechanism making operation **much more smooth**.

1.0.12: Add ss/scm-import-rnrs option, so that all files with ss/scm extension defaultly import chezscheme library (instead of rnrs because chez-scheme's rnrs seems don't have import procedure. That really puzzles me.)

1.0.11: Gradual Typing system, all basic rules have been passed (you can verify it with `test/analysis/type/*.sps` and `test/analysis/type/rules/*.sps`). Detailed documentation has been published. 

1.0.10: Fix bugs in 1.0.9.

1.0.9: Abandoned: add parallel and synchronize mechanism, which can harshly speed up indexing.

1.0.8: Build index as document synchronizing instead of workspace initializing.

1.0.7: Catch syntax-* identifier bindings.