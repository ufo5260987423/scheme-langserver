(library (scheme-langserver analysis type substitutions rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (lambda-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ ((? symbol? identifiers) ...) fuzzy **1 ) 
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) **1) index-nodes
              [parameter-index-nodes (index-node-children (cadr children))]
              [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)])
            (extend-index-node-substitution-list index-node 
              .
              (construct-lambdas-with `(,return-index-node) parameter-index-nodes-products)))]
        [else '()])
      (except c
        [else '()]))))
)
