(library (scheme-langserver analysis type variable)
  (export 
    make-variable
    variable?
    is-pure-variable?
    variable->uuid->string)
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

(define (variable->uuid->string variable)
  (uuid->string (variable-uuid variable)))

(define (is-pure-variable? body)
  (cond
    [(list? body) 
      (fold-left 
        (lambda (flag item)
          (and flag (is-pure-variable? item)))
        #t
        body)]
    [(vector? body)
      (fold-left 
        (lambda (flag item)
          (and flag (is-pure-variable? item)))
        #t
        (vector->list body))]
    [else (variable? body)]))
)