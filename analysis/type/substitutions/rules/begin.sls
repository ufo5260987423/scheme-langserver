(library (scheme-langserver analysis type substitutions rules begin)
  (export begin-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (begin-process document index-node)
  (match-index-node index-node
    [(:_ body ... return)
      (extend-index-node-substitution-list index-node return)]
    [else '()]))
)
