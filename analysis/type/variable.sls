(library (scheme-langserver analysis type variable)
  (export 
    make-variable
    variable?
    is-pure-variable-misture?)
  (import 
    (uuid)
    (chezscheme)
    (scheme-langserver util contain))

(define-record-type variable
  (fields
    (immutable uuid))
  (protocol
    (lambda (new)
      (lambda ()
        (new (random-uuid))))))

(define (is-pure-variable-misture? expression)
  (if (list? expression)
    (not (contain? (map is-pure-variable-misture? expression) #f))
    (variable? expression)))
)