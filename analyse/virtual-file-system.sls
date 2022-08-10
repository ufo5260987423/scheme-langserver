(library (scheme-langserver analyse virtual-file-system)
  (export init-virtual-file-system )
  (import (rnrs) (scheme-langserver util path))

(define-record-type node 
  (fileds
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable path)
    (immutable name)
    (immutable parent-node)
    (immutable children-node-list)
  ))

(define (init-virtual-file-system path)
  (make-node path (uri->name (path->uri path)) '() '()))
)