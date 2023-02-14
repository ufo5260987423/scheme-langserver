(library (scheme-langserver util natural-order-compare)
    (export natural-order-compare)
    (import (rnrs))

(define natural-order-compare 
    (case-lambda 
        [(string-a string-b) (natural-order-compare string-a string-b 0 0)]
        [(string-a string-b index-a index-b)
            (let ([length-a (string-length string-a)]
                    [length-b (string-length string-b)])
                (if (or (>= index-a length-a)
                        (>= index-b length-b))
                    (<= length-a length-b)
                    (let ([char-a (string-ref string-a index-a)]
                            [char-b (string-ref string-b index-b)])
                        (if (char=? char-a char-b)
                            (natural-order-compare string-a string-b (+ 1 index-a) (+ 1 index-b))
                            (char<? char-a char-b)))))]))
)