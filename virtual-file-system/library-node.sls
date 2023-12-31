(library (scheme-langserver virtual-file-system library-node)
  (export 
    delete-library-node-from-tree

    make-library-node
    library-node?
    library-node-name
    library-node-parent
    library-node-file-nodes
    library-node-file-nodes-set!
    library-node-children
    library-node-children-set!
    walk-library)
  (import (rnrs))

(define-record-type library-node 
  (fields
    (immutable name)
    (immutable parent)
    (mutable children)
    (mutable file-nodes)))

(define (delete-library-node-from-tree current-library-node)
  (library-node-children-set!
    (library-node-parent current-library-node)
    (filter 
      (lambda (library-node)
        (not (equal? library-node current-library-node)))
      (library-node-children (library-node-parent current-library-node)))))

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