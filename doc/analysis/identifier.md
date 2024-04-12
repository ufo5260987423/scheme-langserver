## How dose scheme-langserver catch identifier bindings?

Procedures and variable bindings are the fundamental building blocks of Scheme programs. In fact, most part of functionalities including auto-completion, goto definition, document symbol etc., all dependent on them. In this document, I'll describe what scheme-langserver do in analysis/identifier/rules directory. Following forms are from [the summary from csug 9.5](https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0). These form should be caught by define-record-type.sls, lambda.sls, let.sls and so one. 

### Abstract Interpreter
Scheme-langserver's whole identifier catching mechanism is basically fully framed with [abstract interpreter](https://en.wikipedia.org/wiki/Abstract_interpretation). The most essential code located in [this file](../../analysis/abstract-interpreter.sls) now has initially exhibited my design in version 1.2.0. Its main purpose is to allow self-defined macros to introduce new library or identifiers in code, which is rare in main stream programming languages and deeply attract most scheme language programmers. 

An example is from the macro in [try.sls](../../util/try.sls). It allows catching exceptions like in Java without any introduction of keywords:
```scheme
(try
    ...
    (except current-exception 
        [condition do]
        ...
        [else ...]))
```

As you may see, `current-exception` is claimed as an identifier, and is bound with any exception. This identifier maybe used in rest body of `except`, and we all suppose scheme-langserver can help. And this feature will be implemented soon.

### Identifier binding forms in r6rs standard 
In practice, these forms would produce [identifier-reference](../../analysis/identifier/reference.sls) and attach them to [index-nodes](../../virtual-file-system/index-node.sls). Specifically, one form would produce one unique identifier and attach it to `index-node-export-to-other-node`, `index-node-import-in-this-node`, and `index-node-excluded-references`. 

| Head                    | Form                                                               |
|-------------------------|--------------------------------------------------------------------|
| case-lambda             | (case-lambda clause ...)                                           |
| define                  | (define var expr)                                                  |
| define                  | (define var)                                                       |
| define                  | (define (var0 var1 ...) body1 body2 ...)                           |
| define                  | (define (var0 . varr) body1 body2 ...)                             |
| define                  | (define (var0 var1 var2 ... . varr) body1 body2 ...)               |
| define-condition-type   | (define-condition-type name parent constructor pred field ...)     |
| define-enumeration      | (define-enumeration name (symbol ...) constructor)                 |
| define-ftype            | (define-ftype ftype-name ftype)                                    |
| define-ftype            | (define-ftype (ftype-name ftype) ...)                              |
| define-property         | (define-property id key expr)                                      |
| define-record           | (define-record name (fld1 ...) ((fld2 init) ...) (opt ...))        |
| define-record           | (define-record name parent (fld1 ...) ((fld2 init) ...) (opt ...)) |
| define-record-type      | (define-record-type record-name clause ...)                        |
| define-record-type      | (define-record-type (record-name constructor pred) clause ...)     |
| define-structure        | (define-structure (name id1 ...) ((id2 expr) ...))                 |
| define-syntax           | (define-syntax keyword expr)                                       |
| define-top-level-syntax | (define-top-level-syntax symbol obj)                               |
| define-top-level-syntax | (define-top-level-syntax symbol obj env)                           |
| define-top-level-value  | (define-top-level-value symbol obj)                                |
| define-top-level-value  | (define-top-level-value symbol obj env)                            |
| fluid-let               | (fluid-let ((var expr) ...) body1 body2 ...)                       |
| fluid-let-syntax        | (fluid-let-syntax ((keyword expr) ...) form1 form2 ...)            |
| identifier-syntax       | (identifier-syntax tmpl)                                           |
| identifier-syntax       | (identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))              |
| lambda                  | (lambda formals body1 body2 ...)                                   |
| let                     | (let ((var expr) ...) body1 body2 ...)                             |
| let                     | (let name ((var expr) ...) body1 body2 ...)                        |
| let*                    | (let* ((var expr) ...) body1 body2 ...)                            |
| let*-values             | (let*-values ((formals expr) ...) body1 body2 ...)                 |
| let-syntax              | (let-syntax ((keyword expr) ...) form1 form2 ...)                  |
| let-values              | (let-values ((formals expr) ...) body1 body2 ...)                  |
| letrec                  | (letrec ((var expr) ...) body1 body2 ...)                          |
| letrec*                 | (letrec* ((var expr) ...) body1 body2 ...)                         |
| letrec-syntax           | (letrec-syntax ((keyword expr) ...) form1 form2 ...)               |
| syntax                  | (syntax template)                                                  |
| syntax-case             | (syntax-case expr (literal ...) clause ...)                        |
| syntax-rules            | (syntax-rules (literal ...) clause ...)                            |
| with-syntax             | (with-syntax ((pattern expr) ...) body1 body2 ...)                 |

NOTE: 
1.  `define-top-level-syntax`, `define-top-level-value` would bind identifiers to environment instead of library. Detailed catching rule would be programmed when I'm ready. 
2.  `define-record` is only available to Chez Scheme, so I haven't been writing corresponding rules. A direct problem is whether `define-record` binding can make `define-record-type` binding as its parent.

### Identifier binding export/import/load in r6rs standard 
Based on library framework, `export` and `import` would transfer identifier-references across libraries files. Specially, `load` will bind identifiers dynamically, I just try my best to analysis corresponding static code and roughly attach references to caller files.

| Head                    | Form                                 |
|-------------------------|--------------------------------------|
| export                  | (export export-spec ...)             |
| implicit-exports        | (implicit-exports #t)                |
| implicit-exports        | (implicit-exports #f)                |
| import                  | (import import-spec ...)             |
| import-only             | (import-only import-spec ...)        |
| indirect-export         | (indirect-export id indirect-id ...) |
| load                    | (load path)                          |
| load                    | (load path eval-proc)                |
| load-compiled-from-port | (load-compiled-from-port input-port) |
| load-library            | (load-library path)                  |
| load-library            | (load-library path eval-proc)        |
| load-program            | (load-program path)                  |
| load-program            | (load-program path eval-proc)        |
| load-shared-object      | (load-shared-object path)            |

### Identifier binding for self-made macro
As like [try.sls](../../util/try.sls), they also produce identifiers as expanding macros. A patch will be fixed in the future.

### Why does identifier catching interleave index fully updating?
This question is somehow asked maybe they think would speed up auto-complete for long scheme code programming. Their basic idea is that whether the old caught identifiers can be smoothly transformed from old indexed code to newly updated indexed code. Apparently, copy and paste seems to cost less than `match` macro. However, it faced a serious problem that any seemed-locally updating may globally affect whole syntax tree, because code editing is orthogonal with index-nodes tree, and practically, is orthogonal with identifier catching. 

In relation with this, all index node in old code, before it transformed its caught identifiers to new code, two criterions must be followed:
1. Its ancestors must prove similar context with old code, because identifiers imported by ancestors may affect the catching mechanism.
2. Its sibling and children must be consisted with old identifiers. Because some identifier definition macros (like `define`) beyond themselves but fully overlap within their common parent node. And children may affect identifiers' detail.

To be a simple conclusion, these criterions are not more efficient than fully updated. But maybe more work can be done.