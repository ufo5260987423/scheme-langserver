(library (scheme-langserver analysis type meta-type)
  (export 
    void? 
    something?)
  (import (chezscheme))

(define (void? x)
  (equal? (void) x))

(define (something? x) #t)

(define (textual-output-port? x)
  (and (textual-port? x) (output-port? x)))

(define (binary-output-port? x)
  (and (binary-port? x) (output-port? x)))
  
(define (binary-input-port? x)
  (and (binary-port? x) (input-port? x)))

)