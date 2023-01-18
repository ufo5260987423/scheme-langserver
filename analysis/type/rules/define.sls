(library (scheme-langserver analysis type rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (define-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('define ((? symbol? identifiers) (? symbol? parameters) ... ) dummy0 ... end) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([loop-parameter-nodes (cdr (index-node-children (cadr (index-node-children index-node))))]
              [identifier-list parameters]
              [result '()])
            (if (null? loop-parameter-nodes)
              (let* ([target-node (car (index-node-children (cadr (index-node-children index-node))))]
                  [target-exported-reference (index-node-references-export-to-other-node target-node)]
                  [end-node (car (reverse (index-node-children index-node)))]
                  [end-type (index-node-actural-have-type end-node)]
                  [should-have-type `(,end-type ,result)])
                (identifier-reference-type-expressions-set! target-exported-reference should-have-type)
                (index-node-actural-have-type-set! target-node should-have-type))
              (let* ([identifier (car identifier-list)]
                  [current-index-node (car loop-parameter-nodes)]
                  [identifier-reference (car (index-node-references-export-to-other-node current-index-node))]
                  [type-expression (collect-reference-should-have-type identifier index-node)])
                (index-node-actural-have-type-set! current-index-node type-expression)
                (identifier-reference-type-expressions-set! identifier-reference `(,type-expression))
                (loop (cdr loop-parameter-nodes) (cdr identifier-list) (append result `(,type-expression))))))]
        [('define (? symbol? identifiers) end) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([target-node (car (index-node-children index-node))]
              [target-exported-reference (car (index-node-references-export-to-other-node target-node))]
              [end-node (car (reverse (index-node-children index-node)))]
              [end-type (index-node-actural-have-type end-node)]
              [should-have-type end-type])
            (identifier-reference-type-expressions-set! target-exported-reference should-have-type)
            (index-node-actural-have-type-set! target-node should-have-type))]
        [else '()])
      (except c
        [else '()]))))
)