(library (scheme-langserver analysis type substitutions rules begin)
  (export begin-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (begin-process document index-node)
  (extend-index-node-substitution-list
    index-node
    (car (reverse (index-node-children index-node)))))
)