(library (scheme-langserver util cartesian-product)
  (export cartesian-product)
  (import (rnrs))

;standard cartesian
;but here's a strange thing that: (cartesian-product '(()))=>'((())). Would this affect something?
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