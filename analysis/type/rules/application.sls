(library (scheme-langserver analysis type rules application)
  (export application-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (application-process document index-node substitutions)
  (let* ([variable (index-node-variable index-node)]
      [children (index-node-children index-node)])
    (if (null? children)
      substitutions
      (add-to-substitutions
        substitutions
        `(,variable = (,(index-node-variable (car children)) (inner:list? ,@(map index-node-variable (cdr children)))))))))
      ; (begin 
        ; (pretty-print (annotation-stripped (index-node-datum/annotations index-node)))
        ; (pretty-print `(,variable = (,(index-node-variable (car children)) (inner:list? ,@(map index-node-variable (cdr children))))))
      ; )
)
