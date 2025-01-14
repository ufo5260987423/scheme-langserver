(library (scheme-langserver analysis type substitutions rules case-lambda)
  (export case-lambda-process)
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

(define (case-lambda-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ clause **1) 
          (let* ([variable (index-node-variable index-node)]
              [clause-index-nodes (cdr children)])
            (apply 
              append 
              (map 
                (lambda (clause-index-node)
                  (private-clause-process substitutions `(,variable) clause-index-node))
                clause-index-nodes)))]
        [else '()])
      (except c
        [else '()]))))

(define (private-clause-process substitutions root-variables clause-index-node)
  (let ([children (index-node-children clause-index-node)]
      [expression (annotation-stripped (index-node-datum/annotations clause-index-node))])
    (match expression
      [(((? symbol? parameter) ...) _ **1) 
        (cartesian-product
          root-variables
          '(=)
          (construct-lambdas-with 
            `(,(index-node-variable (car (reverse children))))
            (construct-parameter-variable-products-with (index-node-children (car children)))))]
      [else '()])))
)
