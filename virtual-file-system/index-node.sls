(library (scheme-langserver virtual-file-system index-node)
  (export 
    make-index-node
    index-node?
    index-node-parent
    index-node-start
    index-node-end
    index-node-datum/annotations

    index-node-children
    index-node-children-set!
    index-node-available-references
    index-node-available-references-set!
    index-node-current-references
    index-node-current-references-set!)
  (import (rnrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type index-node
  (fields
    (immutable parent)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)

    (mutable children)
    (mutable available-references)
    (mutable current-references)))
)