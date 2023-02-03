(library (scheme-langserver util binary-search)
    (export binary-search)
    (import (chezscheme))

(define binary-search 
    (case-lambda
        [(vector-instance order-compare target) (binary-search vector-instance order-compare target 0 (- (vector-length vector-instance) 1))]
        [(vector-instance order-compare target start end) 
            (cond
                [(= start end) (private-collect vector-instance order-compare target start)]
                [(and 
                        (< start end)
                        (order-compare (vector-ref vector-instance start) target)
                        (order-compare target (vector-ref vector-instance end)))
                    (let* ([pre-mid (floor (/ (+ start end) 2))]
                            [mid (if (flonum? pre-mid) (flonum->fixnum pre-mid) pre-mid)]
                            [previous (binary-search vector-instance order-compare target start mid)])
                        (if (and 
                                (null? previous) 
                                (not (= mid start)))
                            (binary-search vector-instance order-compare target mid end)
                            previous))]
                ['()])]))

(define private-collect
    (case-lambda
        [(vector-instance order-compare target index add)
            (if (or (< index 0) (>= index (vector-length vector-instance)))
                '()
                (let ([item (vector-ref vector-instance index)])
                    (if (and 
                            (order-compare target item)
                            (order-compare item target))
                        `(,item ,@(private-collect vector-instance order-compare target (+ index add) add))
                        '())))]
        [(vector-instance order-compare target index)
            (append 
                (private-collect vector-instance order-compare target (- index 1) -1)
                (private-collect vector-instance order-compare target index 1))]))
)