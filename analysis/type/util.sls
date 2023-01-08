(library (scheme-langserver analysis type util)
  (export 
    construct-lambda)
  (import 
    (rnrs)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

(define (type-equal? type0 type1)
  (if (and (list? type0) (list? type1))
    (let* ([set0 (dedupe (private-process-or type0))]
        [set1 (dedupe (private-process-or type1))])
      (if (and (null? set0) (null? set1))
        #t
        (not (null? (find-intersection set0 set1 type-equal?)))))
    #f))

(define (private-process-or type)
  (if (list? type)
    (if (equal? 'or (car type))
      (filter (lambda(x) (not (null? x)))
        (map private-process-or (cdr type)))
      `(,type))
    '()))
)