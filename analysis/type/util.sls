(library (scheme-langserver analysis type util)
  (export 
    construct-lambda)
  (import (chezscheme))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

)