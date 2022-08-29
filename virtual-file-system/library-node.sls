(library (scheme-langserver virtual-file-system library-node)
  (export 
    make-library-node
    library-node?
    library-node-name
    library-node-parent
    library-node-file-nodes
    library-node-file-nodes-set!
    library-node-children
    library-node-children-set!)
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
)