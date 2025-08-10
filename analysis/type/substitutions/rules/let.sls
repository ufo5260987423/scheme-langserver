(library (scheme-langserver analysis type substitutions rules let)
  (export 
    let-process
    let:private-process-key-value)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (let-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ (? symbol? loop-identifier) (((? symbol? identifier) value ) ... ) fuzzy ...) 
          (let* ([return-index-node (car (reverse children))]
              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (caddr children))]
              ;identifier index-nodes
              [key-index-nodes (map car (map index-node-children key-value-index-nodes))]
              [parameter-index-nodes-products (construct-parameter-index-nodes-products-with key-index-nodes)]

              ;(? symbol? loop-identifier)
              [loop-index-node (cadr children)]
              [loop-procedure-details (construct-lambdas-with `(,return-index-node) parameter-index-nodes-products)])

            (extend-index-node-substitution-list index-node return-index-node)
            (extend-index-node-substitution-list return-index-node index-node)
            (extend-index-node-substitution-list loop-index-node . loop-procedure-details)
            (map let:private-process-key-value key-value-index-nodes))]
        [(_ (((? symbol? identifier) value) ...) fuzzy **1) 
          (let* ([return-index-node (car (reverse children))]
              [key-value-index-nodes (index-node-children (cadr children))])
            (extend-index-node-substitution-list index-node return-index-node)
            (extend-index-node-substitution-list return-index-node index-node)
            (map let:private-process-key-value key-value-index-nodes))]
        [else '()])
      (except c
        [else '()]))))

(define (let:private-process-key-value parent-index-node)
  (let* ([ann (index-node-datum/annotations parent-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children parent-index-node)])
    (match expression 
      [((? symbol? left) value) 
        (extend-index-node-substitution-list (car children) (cadr children))
        (extend-index-node-substitution-list (cadr children) (car children))]
      [else '()])))
)
