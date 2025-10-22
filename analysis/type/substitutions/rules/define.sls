(library (scheme-langserver analysis type substitutions rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (define-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ ((? symbol? identifiers) (? symbol? parameters) ... ) tail) 
        (let* ([identifier-index-node (car (index-node-children (cadr (index-node-children index-node))))]
            [tail-index-node (car (reverse (index-node-children index-node)))]

            [parameter-index-nodes (cdr (index-node-children (cadr (index-node-children index-node))))]
            [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)]
            [lambda-details (construct-lambdas-with (list tail-index-node) parameter-index-nodes-products)])
          (map 
            (lambda (t)
              (extend-index-node-substitution-list identifier-index-node t))
            lambda-details))]
      [(_ (? symbol? identifiers) tail) 
        (let* ([identifier-index-node (cadr (index-node-children index-node))]
            [tail-index-node (car (reverse (index-node-children index-node)))])
          (extend-index-node-substitution-list identifier-index-node tail-index-node)
          (extend-index-node-substitution-list tail-index-node identifier-index-node))]
      [else '()])))
)