(library (scheme-langserver analysis type substitutions rules cond)
  (export cond-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (cond-process document index-node)
  (match-index-node index-node
    [(:_ clauses **1)
      (for-each 
        (lambda (clause) (private-clause-process index-node clause))
        clauses)]
    [else '()]))

(define (private-clause-process root-index-node clause-index-node)
  (match-index-node clause-index-node
    [(predicate)
      (extend-index-node-substitution-list predicate 'something?)
      (extend-index-node-substitution-list root-index-node predicate)
      (extend-index-node-substitution-list predicate root-index-node) ]
    [(predicate . (:_ ... last))
      (extend-index-node-substitution-list predicate 'something?)
      (extend-index-node-substitution-list root-index-node last)
      (extend-index-node-substitution-list last root-index-node)]
    [else '()]))
)
