(library (scheme-langserver analysis type substitutions rules define*)
  (export define*-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (define*-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ ((? symbol? identifiers) parameters ... ) tail) 
          (let* ([identifier-index-node (car (index-node-children (cadr (index-node-children index-node))))]
              [tail-index-node (car (reverse (index-node-children index-node)))]

              [parameter-index-nodes (define*-parameter-index-node-extract (cdr (index-node-children (cadr (index-node-children index-node)))) document)]
              [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)]
              [lambda-details (construct-lambdas-with (list tail-index-node) parameter-index-nodes-products)])
            (map 
              (lambda (t)
                (extend-index-node-substitution-list identifier-index-node t))
              lambda-details))]
        [else '()])
      (except c
        [else '()]))))

(define (define*-parameter-index-node-extract parameter-index-nodes current-document)
  ;; for define*, the parameter could be (identifier1 identifier2 ...)
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
    parameter-index-nodes))

)