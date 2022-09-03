(library (scheme-langserver virtual-file-system library-node)
  (export 
    make-library-node
    library-node?
    library-node-name
    library-node-parent
    library-node-file-nodes
    library-node-file-nodes-set!
    library-node-children
    library-node-children-set!
    walk-library)
  (import (rnrs)
    ; (chezscheme) 
    ; (ufo-match) 
    ; (scheme-langserver virtual-file-system file-node)
    ; (scheme-langserver virtual-file-system document)
    ; (scheme-langserver util io)
    )

(define-record-type library-node 
  (fields
    (immutable name)
    (immutable parent)
    (mutable children)
    (mutable file-nodes)))

(define (walk-library list-instance current-library-node)
  (if (null? list-instance)
    current-library-node
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [child (find 
              (lambda (child-node) (equal? head (library-node-name child-node))) 
              (library-node-children current-library-node))])
      (if child
        (walk-library rest child)
        '()))))
)