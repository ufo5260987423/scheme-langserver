(library (scheme-langserver analysis type util)
  (export 
    construct-lambda)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

(define (type-satisfy>= type0 type1)
  (type-equal? type0 type1 private-satisfy>=))

(define (private-satisfy>= type0 type1)
  (if (equal? type0 type1)
    #t
    (let ([numeric0 (private-type->numeric-type type0)]
        [numeric1 (private-type->numeric-type type1)])
      (if (and numeric0 numeric1)
        (>= numeric0 numeric1)
        #f))))

(define (private-type->numeric-type type0)
  (cond 
    [(equal? (car type0) fixnum?) 0]
    [(equal? (car type0) integer?) 1]
    [(equal? (car type0) flonum?) 2]
    [(equal? (car type0) real?) 3]
    [(equal? (car type0) number?) 4]
    [else #f]))

(define (type-equal? type0 type1 equal-predicator)
  (if (and (list? type0) (list? type1))
    (let* ([set0 (dedupe (private-process-or type0))]
        [set1 (dedupe (private-process-or type1))])
      (if (and (null? set0) (null? set1))
        #t
        (not (null? (find-intersection set0 set1 equal-predicator)))))
    #f))

(define (private-process-or type)
  (if (list? type)
    (if (equal? 'or (car type))
      (filter (lambda(x) (not (null? x)))
        (map private-process-or (cdr type)))
      `(,type))
    '()))
)