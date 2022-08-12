(library (scheme-langserver analyse index)
  (export init-index)
  (import (rnrs) )

(define-record-type index-node
  (fileds
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable document)
    (immutable start)
    (immutable end)
    (immutable content)
    (immutable parent)
    (immutable children)
  ))

(define (init-index root-path)
  (make-index root-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string->list text)
  (with-input-from-string text
    (lambda(port)
      (let loop (
          [result '()]
          [datum (read port)])
        (if (eof-object? datum))
          (loop (append result datum) (read port))
          (append result datum)))))
)