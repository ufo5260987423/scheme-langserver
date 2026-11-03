(library (scheme-langserver analysis type substitutions rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (define-process document index-node)
  (match-index-node index-node
    [(:_ (and signature
               (? index-node-proper-list?)
               ((? index-node-symbol? name) . params))
        :_ ... return)
      (let* ([parameter-index-nodes-products (construct-parameter-index-nodes-products-with params)]
          [lambda-details (construct-lambdas-with (list return) parameter-index-nodes-products)])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [(:_ (and signature
               (? index-node-improper-list?)
               ((? index-node-symbol? name) . :_))
        :_ ... return)
      (let* ([parameter-types (construct-rest-parameter-template (cdr (index-node-children signature)))]
          [lambda-details (construct-lambdas-with (list return) (list parameter-types))])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [(:_ (? index-node-symbol? name) :_ ... return)
      (extend-index-node-substitution-list name return)
      (extend-index-node-substitution-list return name)]
    [else '()]))
)
