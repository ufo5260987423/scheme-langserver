(library (scheme-langserver analysis type substitutions rules unless)
  (export unless-process)
  (import 
    (chezscheme) 

    (scheme-langserver virtual-file-system index-node))

(define (unless-process document index-node)
  (extend-index-node-substitution-list
    index-node
    (car (reverse (index-node-children index-node)))))
)
