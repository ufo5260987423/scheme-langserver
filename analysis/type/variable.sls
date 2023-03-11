(library (scheme-langserver analysis type variable)
  (export 
    make-variable
    variable?)
  (import 
    (uuid)
    (chezscheme))

(define-record-type variable
  (fields
    (immutable uuid))
  (protocol
    (lambda (new)
      (lambda ()
        (new (random-uuid))))))
)