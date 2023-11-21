## Detailed Relases Log
1.0.13: Fix bug: sometimes can't shutdown server. Optimization: re-construct document-sync mechanism making operation **much more smooth**.

1.0.12: Add ss/scm-import-rnrs option, so that all files with ss/scm extension defaultly import chezscheme library (instead of rnrs because chez-scheme's rnrs seems don't have import procedure. That really puzzles me.)

1.0.11: Gradual Typing system, all basic rules have been passed (you can verify it with `test/analysis/type/*.sps` and `test/analysis/type/rules/*.sps`). Detailed documentation has been published. 

1.0.10: Fix bugs in 1.0.9.

1.0.9: Abandoned: add parallel and synchronize mechanism, which can harshly speed up indexing.

1.0.8: Build index as document synchronizing instead of workspace initializing.

1.0.7: Catch syntax-* identifier bindings.