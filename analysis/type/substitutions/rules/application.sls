(library (scheme-langserver analysis type substitutions rules application)
  (export application-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (application-process document index-node substitutions)
  (let* ([variable (index-node-variable index-node)]
      [children (index-node-children index-node)])
    (if (null? children)
      '()
      `((,variable = ,(map index-node-variable children))))))
)