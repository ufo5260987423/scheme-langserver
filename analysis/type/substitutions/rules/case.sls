(library (scheme-langserver analysis type substitutions rules case)
  (export case-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (case-process document index-node)
  (match-index-node index-node
    [(:_ expr (previous :_ ... return) **1)
      (for-each 
        (lambda (t) (extend-index-node-substitution-list expr t))
        previous)
      (for-each 
        (lambda (t)
          (extend-index-node-substitution-list index-node t)
          (extend-index-node-substitution-list t index-node))
        return)]
    [else '()]))
)
