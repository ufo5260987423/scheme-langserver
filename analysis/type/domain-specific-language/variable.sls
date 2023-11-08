(library (scheme-langserver analysis type domain-specific-language variable)
  (export 
    make-variable
    variable?
    pure-variable?
    variable-uuid)
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
        (new (uuid->string (random-uuid)))))))

(define (pure-variable? body)
  (cond
    [(list? body) 
      (fold-left 
        (lambda (flag item)
          (and flag (pure-variable? item)))
        #t
        body)]
    [(vector? body)
      (fold-left 
        (lambda (flag item)
          (and flag (pure-variable? item)))
        #t
        (vector->list body))]
    [else (variable? body)]))
)