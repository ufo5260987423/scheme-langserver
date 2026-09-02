(library (scheme-langserver analysis type substitutions rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (if-process document index-node)
  (match-index-node index-node
    [(:_ condition return)
      (extend-index-node-substitution-list condition 'something?)
      (extend-index-node-substitution-list index-node return)
      (extend-index-node-substitution-list return index-node)]
    [(:_ condition return0 return1)
      (extend-index-node-substitution-list condition 'something?)
      (extend-index-node-substitution-list index-node return0)
      (extend-index-node-substitution-list index-node return1)
      (extend-index-node-substitution-list return0 index-node)
      (extend-index-node-substitution-list return1 index-node)]
    [else '()]))
)
