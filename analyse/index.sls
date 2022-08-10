(library (scheme-langserver analyse index)
  (export init-index)
  (import (rnrs) )

(define-record-type index
  (fileds
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable root-uri)
  ))

(define (init-index root-uri)
  (make-index uri))
)