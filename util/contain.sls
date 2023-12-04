(library (scheme-langserver util contain)
    (export contain?)
    (import (rnrs))

(define contain? 
    (case-lambda
        [(list-instance item) (contain? list-instance item equal?)]
        [(list-instance item equal-predicator) 
            (let loop ([body list-instance])
                (if (null? body)
                    #f
                    (if (equal-predicator item (car body))
                        #t
                        (loop (cdr body)))))]))
)