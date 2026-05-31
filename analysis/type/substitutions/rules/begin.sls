(library (scheme-langserver analysis type substitutions rules begin)
  (export begin-process)
  (import 
    (chezscheme) 

    (scheme-langserver virtual-file-system index-node))

(define (begin-process document index-node)
  (extend-index-node-substitution-list
    index-node
    (car (reverse (index-node-children index-node)))))
)