## How dose scheme-langserver catch identifier bindings?

Procedures and variable bindings are the fundamental building blocks of Scheme programs. In fact, most part of functionalities including auto-completion, goto definition, document symbol etc., all dependent on them. In this document, I'll describe what scheme-langserver do in analysis/identifier/rules directory. Following forms are from [the summary from csug 9.5](https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0). These form should be caught by define-record-type.sls, lambda.sls, let.sls and so one. 

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