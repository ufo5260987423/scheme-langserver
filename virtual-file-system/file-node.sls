(library (scheme-langserver virtual-file-system file-node)
  (export 
    make-file-node
    file-node
    file-node?
    file-node-children-set!
    file-node-children
    file-node-folder?
    file-node-parent
    file-node-name
    file-node-path
    file-node-document-set!
    file-node-document)
  (import (rnrs))

(define-record-type file-node 
  (fields
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable path)
    (immutable name)
    (immutable parent)
    (immutable folder?)

    (mutable children)
    (mutable document)))
)