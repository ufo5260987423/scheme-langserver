(library (scheme-langserver util cartesian-product)
    (export cartesian-product)
    (import (rnrs))

(define cartesian-product 
;todo: well,should be a pure cartesian-product, but now ...
    (case-lambda 
        [() '()]
        [(list1) list1]
        [(list1 list2 . rest)
    (let ([current
                (apply append
                    (map 
                        (lambda (item1)
                            (map 
                                (lambda (item2)
                                    (list item1 item2))
                            list2))
                    list1))])
        (if (null? rest)
            current
            (apply cartesian-product `(,current ,@rest))))]))
)