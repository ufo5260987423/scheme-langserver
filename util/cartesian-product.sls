(library (scheme-langserver util cartesian-product)
  (export cartesian-product)
  (import (rnrs))

(define (cartesian-product . lists)
  (fold-right 
    (lambda (xs ys)
      (apply append 
        (map (lambda (x)
          (map (lambda (y)
            (cons x y))
            ys))
        xs)))
    '(())
    lists))
)