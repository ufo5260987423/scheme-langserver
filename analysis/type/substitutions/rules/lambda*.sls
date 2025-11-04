(library (scheme-langserver analysis type substitutions rules lambda*)
  (export lambda*-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))


(define (lambda*-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ (identifiers ...) fuzzy **1 ) 
        (let* ([return-index-node (car (reverse children))]

            ;(identifier **1) index-nodes
            [parameter-index-nodes (lambda*-parameter-index-node-extract (cadr children) document)]
            [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)])
          (map 
            (lambda (t) (extend-index-node-substitution-list index-node t))
            (construct-lambdas-with `(,return-index-node) parameter-index-nodes-products)))]
      [else '()])))

(define (lambda*-parameter-index-node-extract parameter-index-nodes current-document)
  ;; for lambda*, the parameter could be (identifier1 identifier2 ...)
  ;; and also ((identifier1 value1) identifier2 (identifier3 value3) ...) ,
  ;; this function is used to get all identifier index-nodes.
  (map
    (lambda (index-node)
      (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
          (match expression
            [(? symbol? expression)
              index-node]
            [((? symbol? param) (? symbol? type))
              (map (lambda (id) (extend-index-node-substitution-list (car (index-node-children index-node)) id))
                (map root-ancestor (find-available-references-for current-document index-node type)))
              (car (index-node-children index-node))])))
    (index-node-children parameter-index-nodes)))

)
