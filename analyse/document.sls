(library (scheme-langserver analyse document)
  (export 
    collect-document-libraries
    init-document)
  (import (rnrs) )

(define-record-type document 
  (fileds 
    (immutable uri)
    (mutable text)
    (mutable tree)
    (immutable mutex)
    (immutable condition)
    ;;;;;;;;;;;;
    (mutable bytecode)
    (mutable libraries)
    (mutable environment)
    (mutable diagnostics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (collect-document-libraries document-hashtable)
  (map document-libraries (hashtable-values document-hashtable)))

(define (init-document uri)
  (make-document uri "" #f #f (make-eq-hashtable) #f '()))

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