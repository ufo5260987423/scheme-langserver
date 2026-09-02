(library (scheme-langserver analysis type substitutions rules unless)
  (export unless-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (unless-process document index-node)
  (match-index-node index-node
    [(:_ condition body ... return)
      (extend-index-node-substitution-list index-node return)]
    [else '()]))
)
