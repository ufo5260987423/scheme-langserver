(library (scheme-langserver analysis type rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (lambda-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('lambda (identifiers **1) _ ... ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([loop-parameter-nodes (cadr (index-node-children index-node))]
              [identifier-list identifiers]
              [result '()])
            (if (null? loop-parameter-nodes)
              (index-node-actural-have-type-set! index-node result)
              (let* ([identifier (car identifier-list)]
                  [current-index-node (car loop-parameter-nodes)]
                  [identifier-reference (index-node-references-export-to-other-node current-index-node)]
                  [type-expression (collect-reference-should-have-type identifier index-node)])
                (index-node-actural-have-type-set! current-index-node type-expression)
                (identifier-reference-type-expressions-set! identifier-reference `(,type-expression))
                (loop (cdr loop-parameter-nodes) (cdr identifier-list) (append result `(,type-expression)))))
            (index-node-actural-have-type-set! 
              index-node 
              (append 
                `(,(index-node-actural-have-type (car (reverse (index-node-children index-node)))))
                (index-node-actural-have-type index-node))))]
        [('lambda (? symbol? identifier) _ ... ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme)) 
          (index-node-actural-have-type-set! 
            index-node 
            (append (index-node-actural-have-type (car (reverse (index-node-children index-node)))) '(((something? x) ...)))) ]
        [('case-lambda (dummy0 ...) dummy1 ... ) 
          (guard-for document index-node 'case-lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (cdr (index-node-children index-node))])
            (if (not (null? rest))
              (let* ([identifier-index-node-grand-parent (car rest)]
                  [grand-parent-expression (annotation-stripped (index-node-datum/annotations identifier-index-node-grand-parent))])
                (match grand-parent-expression 
                  [((param-identifiers **1) body ...)
                    (let loop-parameter ([loop-parameter-nodes (index-node-children (car (index-node-children identifier-index-node-grand-parent)))]
                        [identifier-list param-identifiers]
                        [result '()])
                      (if (null? loop-parameter-nodes)
                        (let* ([tail-index-node (car (reverse (index-node-children identifier-index-node-grand-parent)))]
                            [single-expression `(,(index-node-actural-have-type tail-index-node) ,result)])
                          (index-node-actural-have-type-set! 
                            index-node 
                            (append 
                              (index-node-actural-have-type index-node)
                              `(,single-expression)))
                          (loop (cdr rest)))
                        (let* ([identifier (car identifier-list)]
                            [current-index-node (car loop-parameter-nodes)]
                            [identifier-reference (index-node-references-export-to-other-node current-index-node)]
                            [type-expression (collect-reference-should-have-type identifier identifier-index-node-grand-parent)])
                          (index-node-actural-have-type-set! current-index-node type-expression)
                          (identifier-reference-type-expressions-set! identifier-reference `(,type-expression))
                          (loop-parameter (cdr loop-parameter-nodes) (cdr identifier-list) (append result `(,type-expression))))))]
                  [else '()]
                ))))]
        [else '()])
      (except c
        [else '()]))))
)
