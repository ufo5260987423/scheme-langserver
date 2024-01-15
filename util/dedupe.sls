(library (scheme-langserver util dedupe)
    (export 
        dedupe
        ordered-dedupe)
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

(define ordered-dedupe
    (case-lambda 
        [(e) (ordered-dedupe e equal?)]
        [(e equal-procedure) 
            (cond 
                [(null? e) e]
                [(= 1 (length e)) e]
                [(equal-procedure (car e) (cadr e))
                    (ordered-dedupe (cdr e) equal-procedure)]
                [else
                    (cons 
                        (car e)
                        (ordered-dedupe (cdr e) equal-procedure))])]))
)