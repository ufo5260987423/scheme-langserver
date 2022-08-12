(library (scheme-langserver analyse document)
  (export 
    collect-document-libraries
    init-document)
  (import (rnrs) )

(define-record-type document 
  (fileds 
    (immutable uri)
    (immutable node)
    (mutable text)
    (mutable index)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (init-document uri node text)
  (make-document uri node text (string->list text)))

)