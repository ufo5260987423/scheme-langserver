(library (scheme-langserver util sub-list)
    (export 
        list-ahead-of
        list-after)
    (import (rnrs))

(define (list-ahead-of list-instance item)
    (let loop ([loop-body list-instance] [result '()])
        (if (null? loop-body)
            result
            (if (equal? item (car loop-body))
                result
                (loop (cdr loop-body) (append result `(,(car loop-body))))))))

(define (list-after list-instance item)
    (let loop ([loop-body list-instance] [flag #f] [result '()])
        (if (null? loop-body)
            result
            (if flag
                (loop (cdr loop-body) flag (append result `(,(car loop-body))))
                (loop (cdr loop-body) (equal? item (car loop-body)) result)))))
)