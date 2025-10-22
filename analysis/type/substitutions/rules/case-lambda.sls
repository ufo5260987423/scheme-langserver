(library (scheme-langserver analysis type substitutions rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (case-lambda-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ clause **1) 
        (map 
          (lambda (clause-index-node)
            (private-clause-process index-node clause-index-node))
          (cdr children))]
      [else '()])))

(define (private-clause-process root-index-node clause-index-node)
  (let ([children (index-node-children clause-index-node)]
      [expression (annotation-stripped (index-node-datum/annotations clause-index-node))])
    (match expression
      [(((? symbol? parameter) ...) _ **1) 
        (map 
          (lambda (t) (extend-index-node-substitution-list root-index-node t))
          (construct-lambdas-with 
            `(,(car (reverse children)))
            (construct-parameter-index-nodes-products-with (index-node-children (car children)))))]
      [else '()])))
)
