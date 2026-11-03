(library (scheme-langserver analysis type substitutions rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (lambda-process document index-node)
  (match-index-node index-node
    [(:_ (and :_
               (? index-node-proper-list?)
               ((? index-node-symbol? params) ...))
        . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (construct-parameter-index-nodes-products-with params))]
    [(:_ (? index-node-symbol? rest) . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (list (construct-rest-parameter-template (list rest))))]
    [(:_ formals . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (list (construct-rest-parameter-template (index-node-children formals))))]
    [else '()]))

(define (private:emit-lambda-substitutions index-node return-node parameter-type-products)
  (for-each 
    (lambda (t) (extend-index-node-substitution-list index-node t))
    (construct-lambdas-with `(,return-node) parameter-type-products)))
)
