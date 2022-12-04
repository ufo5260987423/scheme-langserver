(library (scheme-langserver util contain)
    (export contain?)
    (import (rnrs))

(define contain? 
    (case-lambda
        [(list-instance item) (contain? list-instance item equal?)]
        [(list-instance item equal-predicator) 
            (if (find (lambda(i) (equal-predicator i item)) list-instance) #t #f)]))
)