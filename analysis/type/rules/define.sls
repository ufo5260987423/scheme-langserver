(library (scheme-langserver analysis type rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (define-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('define ((? symbol? identifiers) (? symbol? parameters) ... ) tail) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([identifier-index-node (car (cadr (index-node-children index-node)))]
              [identifier-variables (walk:index-node->single-variable-list substitutions identifier-index-node)]

              [tail-index-node (car (reverse (index-node-children index-node)))]
              [return-variables (walk:index-node->single-variable-list substitutions tail-index-node)]

              [parameter-index-nodes (cdr (cadr (index-node-children index-node)))]
              [parameter-variable-products (construct-parameter-variable-products-with substitutions parameter-index-nodes)]
              [lambda-details (construct-lambdas-with return-variables parameter-variable-products)])
            (append 
              substitutions
              (map 
                (lambda (product)
                  `(,(car product) = ,(cadr product)))
                (cartesian-product identifier-variables lambda-details))))]
        [('define (? symbol? identifiers) tail) 
          (guard-for document index-node 'define '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([identifier-index-node (cadr (index-node-children index-node))]
              [tail-index-node (car (reverse (index-node-children index-node)))])
            (append 
              substitutions
              (construct-substitutions-between-index-nodes substitutions identifier-index-node tail-index-node '=)
              (construct-substitutions-between-index-nodes substitutions tail-index-node identifier-index-node '=)))]
        [else substitutions])
      (except c
        [else substitutions]))))
)