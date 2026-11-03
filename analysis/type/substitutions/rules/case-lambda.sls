(library (scheme-langserver analysis type substitutions rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (case-lambda-process document index-node)
  (match-index-node index-node
    [(:_ clauses **1)
      (for-each 
        (lambda (clause) (private-clause-process index-node clause))
        clauses)]
    [else '()]))

(define (private-clause-process root-index-node clause)
  (match-index-node clause
    [((and :_
            (? index-node-proper-list?)
            ((? index-node-symbol? params) ...))
      :_ ... return)
      (for-each 
        (lambda (t) (extend-index-node-substitution-list root-index-node t))
        (construct-lambdas-with 
          `(,return)
          (construct-parameter-index-nodes-products-with params)))]
    [(formals :_ ... return)
      (let ([parameter-types (construct-rest-parameter-template (index-node-children formals))])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list root-index-node t))
          (construct-lambdas-with `(,return) (list parameter-types))))]
    [else '()]))
)
