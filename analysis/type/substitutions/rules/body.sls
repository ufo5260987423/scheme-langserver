(library (scheme-langserver analysis type substitutions rules body)
  (export body-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (body-process document index-node)
  (let* ([variable (index-node-variable index-node)]
      [children (index-node-children index-node)])
    (if (null? children)
      '()
      `((,variable = ,(index-node-variable (car (reverse children))))))))
)