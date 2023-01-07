(library (scheme-langserver analysis type-inferencer meta-type)
  (export find-rules)
  (import (chezscheme))

(define (void? x)
  (equal? (void?) x))

(define (something? x) #t)
)