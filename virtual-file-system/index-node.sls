(library (scheme-langserver virtual-file-system index-node)
  (export 
    pick-index-node-by

    make-index-node
    index-node?
    index-node-parent
    index-node-start
    index-node-end
    index-node-datum/annotations

    index-node-children
    index-node-children-set!
    index-node-references-export-to-other-node
    index-node-references-export-to-other-node-set!
    index-node-references-import-in-this-node
    index-node-references-import-in-this-node-set!
    index-node-excluded-references
    index-node-excluded-references-set!)
  (import (rnrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type index-node
  (fields
    (immutable parent)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)

    (mutable children)
    (mutable references-export-to-other-node)
    (mutable references-import-in-this-node)
    (mutable excluded-references)))

(define (pick-index-node-by index-node position)
  (if (and (<= (index-node-start index-node) position) (> (index-node-end index-node) position))
    (let loop ([children (index-node-children index-node)])
      (if (null? children)
        index-node
        (let ([result (pick-index-node-by (car children) position)])
          (if (null? result)
            (loop (cdr children))
            result))))
    '()))
)