(library (scheme-langserver analysis type substitutions rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (define-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('define ((? symbol? identifiers) (? symbol? parameters) ... ) tail) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([identifier-index-node (car (cadr (index-node-children index-node)))]
              [identifier-variable (index-node-variable identifier-index-node)]

              [tail-index-node (car (reverse (index-node-children index-node)))]
              [return-variable (index-node-variable tail-index-node)]

              [parameter-index-nodes (cdr (cadr (index-node-children index-node)))]
              [parameter-variable-products (construct-parameter-variable-products-with parameter-index-nodes)]
              [lambda-details (construct-lambdas-with (list return-variable) parameter-variable-products)])
            (fold-left
              add-to-substitutions
              substitutions
              (map 
                (lambda (product)
                  `(,(car product) = ,(cadr product)))
                (cartesian-product `(,identifier-variable) '(=) lambda-details))))]
        [('define (? symbol? identifiers) tail) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([identifier-index-node (cadr (index-node-children index-node))]
              [tail-index-node (car (reverse (index-node-children index-node)))])
            (fold-left
              add-to-substitutions
              substitutions
              (append 
                (construct-substitutions-between-index-nodes identifier-index-node tail-index-node '=)
                (construct-substitutions-between-index-nodes tail-index-node identifier-index-node '=))))]
        [else substitutions])
      (except c
        [else substitutions]))))
)