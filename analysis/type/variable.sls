(library (scheme-langserver analysis type variable)
  (export 
    )
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