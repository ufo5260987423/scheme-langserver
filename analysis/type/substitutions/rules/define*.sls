(library (scheme-langserver analysis type substitutions rules define*)
  (export define*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (define*-process document index-node)
  (match-index-node index-node
    [(:_ ((? index-node-symbol? name) . params) :_ ... return)
      (let* ([parameter-index-nodes (define*-parameter-index-node-extract params document)]
          [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)]
          [lambda-details (construct-lambdas-with (list return) parameter-index-nodes-products)])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [else '()]))

(define (define*-parameter-index-node-extract parameter-index-nodes current-document)
  ;; for define*, the parameter could be (identifier1 identifier2 ...)
  ;; and also ((identifier1 value1) identifier2 (identifier3 value3) ...) ,
  ;; this function is used to get all identifier index-nodes.
  (map
    (lambda (index-node)
      (match-index-node index-node
        [(? index-node-symbol? param)
          param]
        [((? index-node-symbol? param) (? index-node-symbol? type))
          (for-each (lambda (id) (extend-index-node-substitution-list (car (index-node-children index-node)) id))
            (map root-ancestor (find-available-references-for current-document index-node type)))
          (car (index-node-children index-node))]
        [else index-node]))
    (index-node-children parameter-index-nodes)))
)