(library (scheme-langserver analysis type substitutions rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type domain-specific-language variable)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (lambda-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ ((? symbol? identifiers) ...) _ **1 ) 
          (let* ([variable (index-node-variable index-node)]

              [return-index-node (car (reverse children))]
              [return-variable (index-node-variable return-index-node)]

              ;((? symbol? identifier) **1) index-nodes
              [parameter-index-nodes (index-node-children (cadr children))]
              [parameter-variable-products (construct-parameter-variable-products-with parameter-index-nodes)])
            (cartesian-product `(,variable) '(=) (construct-lambdas-with `(,return-variable) parameter-variable-products)))]
        [else '()])
      (except c
        [else '()]))))
)
