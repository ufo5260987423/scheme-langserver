(library (scheme-langserver util contain)
    (export 
        contain?
        ordered-contain?)
    (import (chezscheme))

(define contain? 
    (case-lambda
        [(list-instance item) (contain? list-instance item equal?)]
        [(list-instance item equal-predicator) 
            (if (null? list-instance)
                #f
                (if (equal-predicator item (car list-instance))
                    #t
                    (contain? (cdr list-instance) item equal-predicator)))]))

(define ordered-contain?
    (case-lambda 
        [(list-instance target order-compare) (ordered-contain? (list->vector list-instance) target order-compare 0 (- (length list-instance) 1))]
        [(vector-instance target order-compare start end)
            (cond
                [(< end start) #f]
                [(= start end) 
                    (and 
                        (order-compare (vector-ref vector-instance start) target)
                        (order-compare target (vector-ref vector-instance start)))]
                [(and 
                    (< start end)
                    (order-compare (vector-ref vector-instance start) target)
                    (order-compare target (vector-ref vector-instance end)))
                    (let* ([pre-mid (floor (/ (+ start end) 2))]
                            [mid (if (flonum? pre-mid) (flonum->fixnum pre-mid) pre-mid)])
                        (cond 
                            [(= mid start) (ordered-contain? vector-instance target order-compare start mid)]
                            [(= mid end) (ordered-contain? vector-instance target order-compare mid end)]
                            [else 
                                (or 
                                    (ordered-contain? vector-instance target order-compare start mid)
                                    (ordered-contain? vector-instance target order-compare mid end))]))]
                [else #f])]))
)