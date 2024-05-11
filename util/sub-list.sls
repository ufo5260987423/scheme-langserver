(library (scheme-langserver util sub-list)
  (export 
    list-ahead-of
    list-after

    find-intersection)
  (import 
    (rnrs)
    (scheme-langserver util contain))

(define (find-intersection list0 list1 equal-predicator)
  (fold-left 
    (lambda (l r)
      (if (contain? list1 r equal-predicator)
        (append l `(,r))
        l))
    '()
    list0))

(define (list-ahead-of list-instance item)
  (let loop ([loop-body list-instance])
    (if (null? loop-body)
      '()
      (if (equal? item (car loop-body))
        '()
          `(,(car loop-body) .  ,(loop (cdr loop-body)))))))

(define (list-after list-instance item)
  (reverse (list-ahead-of  (reverse list-instance) item)))
)