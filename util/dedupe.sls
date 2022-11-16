(library (scheme-langserver util dedupe)
    (export dedupe)
    (import (rnrs))

(define dedupe
    (case-lambda
        [(e) (dedupe e equal?)]
        [(e equal-procedure) 
            (if (null? e) 
                '()
                (cons 
                    (car e) 
                    (dedupe 
                        (filter 
                            (lambda (x) (not (equal-procedure x (car e)))) 
                            (cdr e))
                        equal-procedure)))]))
)