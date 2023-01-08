(library (scheme-langserver util sub-list)
    (export 
        list-ahead-of
        list-after

        find-intersection)
    (import 
        (rnrs)
        (scheme-langserver util contain))

(define (find-intersection list0 list1 equal-predicator)
    (let loop ([loop-body list0] [result '()])
        (if (null? loop-body)
            result
            (loop (cdr loop-body) 
                (if (contain? list1 (car loop-body) equal-predicator)
                    (append result (list (car loop-body)))
                    result)))))

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