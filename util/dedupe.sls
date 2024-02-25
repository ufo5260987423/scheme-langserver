(library (scheme-langserver util dedupe)
    (export 
        dedupe
        ordered-dedupe
        dedupe-deduped)
    (import 
        (rnrs)
        (scheme-langserver util contain))

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

(define dedupe-deduped
    (case-lambda 
        [(e1 e2) (dedupe-deduped e1 e2 equal?)]
        [(e1 e2 equal-procedure) 
            (cond 
                [(or (null? e1) (null? e2)) (append e1 e2)]
                [(< (length e1) (length e2)) (dedupe-deduped e2 e1 equal-procedure)]
                [else
                    (append e1
                        (filter (lambda (x) (not (contain? e1 x equal-procedure))) 
                            e2))])]))
)