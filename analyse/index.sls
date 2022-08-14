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
    ; chez scheme read.ss rd-token and more please refer
    ; read.ss 1767->get annotations,yes!
    ; (get-datum/annotations (open-string-input-port "'(1 2 3)") (source-file-descriptor "/home/ufo/1.ss" 1) 0)
    (immutable datum/annotations)
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