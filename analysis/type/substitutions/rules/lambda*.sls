(library (scheme-langserver analysis type substitutions rules lambda*)
  (export lambda*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (lambda*-process document index-node)
  (match-index-node index-node
    [(:_ params :_ ... return)
      (let* ([parameter-index-nodes (lambda*-parameter-index-node-extract params document)]
          [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list index-node t))
          (construct-lambdas-with `(,return) parameter-index-nodes-products)))]
    [else '()]))

(define (lambda*-parameter-index-node-extract parameter-index-nodes current-document)
  ;; for lambda*, the parameter could be (identifier1 identifier2 ...)
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
