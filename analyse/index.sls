(library (scheme-langserver analyse index)
  (export init-index-node)
  (import (rnrs) )

(define-record-type index-node
  (fileds
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable document)
    (immutable parent)
    (immutable children)
    (immutable start)
    (immutable end)
  ))

; (define (init-index-node document parent tree)
;   (let ([text (document-text document)]
;       [node (make-index-node document parent '())]
;       )
;       )
;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string->list text)
  (with-input-from-string text
    (lambda()
      (let loop (
          [result '()]
          [datum (read)])
        (if (eof-object? datum)
          (loop (append result datum) (read))
          (append result datum))))))
)