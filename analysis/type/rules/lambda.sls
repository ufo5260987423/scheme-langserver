(library (scheme-langserver analysis type rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (lambda-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('lambda ((? symbol? identifiers) **1) _ **1 ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([variable (index-node-variable index-node)]

              [return-index-node (car (reverse children))]
              [return-variable (index-node-variable return-index-node)]

              ;((? symbol? identifier) **1) index-nodes
              [parameter-index-nodes (index-node-children (cadr children))]
              [parameter-variable-products (construct-parameter-variable-products-with substitutions parameter-index-nodes)])
            (fold-left
              add-to-substitutions
              substitutions 
              (map 
                (lambda (pair)
                  (list (car pair) '= (cadr pair)))
                (cartesian-product
                  `(,variable)
                  (construct-lambdas-with `(,return-variable) parameter-variable-products)))))]
        [('case-lambda clause **1) 
          (guard-for document index-node 'case-lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([variable (index-node-variable index-node)]
              [clause-index-nodes (cdr children)])
            (fold-left
              add-to-substitutions
              substitutions
              (apply 
                append 
                (map 
                  (lambda (clause-index-node)
                    (private-clause-process substitutions `(,variable) clause-index-node))
                  clause-index-nodes))))]
        [else substitutions])
      (except c
        [else substitutions]))))

(define (private-clause-process substitutions root-variables clause-index-node)
  (let ([children (index-node-children clause-index-node)]
      [expression (annotation-stripped (index-node-datum/annotations clause-index-node))])
    (match expression
      [(((? symbol? parameter) **1) _ **1) 
        (map 
          (lambda (pair)
            (list (car pair) '= (cadr pair)))
          (cartesian-product
            root-variables
            (construct-lambdas-with 
              `(,(index-node-variable (car (reverse children))))
              (construct-parameter-variable-products-with substitutions (index-node-children (car children))))))
          ]
      [else '()])))
)
