(library (scheme-langserver analysis type util)
  (export 
    construct-lambda
    type-intersection
    type-satisfy>=intersection)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

(define (type-satisfy>=intersection type-expression0 index-node0 document0 type-expression1 index-node1 document1)
  (type-intersection type-expression0 index-node0 document0 type-expression1 index-node1 document1 private-satisfy>=))

(define (private-satisfy>= type0 type1)
  (if (equal? type0 type1)
    #t
    (let ([numeric0 (private-type->numeric-type type0)]
        [numeric1 (private-type->numeric-type type1)])
      (if (and numeric0 numeric1)
        (>= numeric0 numeric1)
        (and (null? (cdr type0)) (equal? (car type0) 'something?))))))

;;numeric tower
(define (private-type->numeric-type type0)
  (if (contain? (list '(rnrs) '(scheme) '(chezscheme) '(rnrs base) '(rnrs r5rs)'(rnrs arithmetic flonums) '(rnrs arithmetic bitwise)'(rnrs arithmetic fixnums)) (identifier-reference-library-identifier (cadr type0)))
    (cond 
      [(equal? (car type0) 'fixnum?) 0]
      [(equal? (car type0) 'bignum?) 1]
      [(equal? (car type0) 'integer?) 2]
      [(equal? (car type0) 'cflonum?) 3]
      [(equal? (car type0) 'flonum?) 4]
      [(equal? (car type0) 'rational?) 5]
      [(equal? (car type0) 'real?) 6]
      [(equal? (car type0) 'complex?) 7]
      [(equal? (car type0) 'number?) 8]
      [else #f])
    #f))

;; truly equal should consider the identifier's document context
(define (type-intersection type-expression0 index-node0 document0 type-expression1 index-node1 document1 equal-predicator)
  (if (and (list? type-expression0) (list? type-expression1))
    (let* ([set0 
          (map 
            (lambda (dryed-identifier) `(,dryed-identifier ,(find-available-references-for document0 index-node0 dryed-identifier)))
            (map private-dry-rule (dedupe (private-process-or type-expression0))))]
        [set1 
          (map 
            (lambda (dryed-identifier) `(,dryed-identifier ,(find-available-references-for document1 index-node1 dryed-identifier)))
            (map private-dry-rule (dedupe (private-process-or type-expression1))))])
      (if (and (null? set0) (null? set1))
        '()
        (find-intersection set0 set1 equal-predicator)))
    '()))

(define (private-dry-rule type) (map (lambda(x) (car x)) (private-process-or type)))

(define (private-process-or type)
  (if (list? type)
    (if (equal? 'or (car type))
      (apply append 
        (filter (lambda(x) (not (null? x)))
          (map private-process-or (cdr type))))
      `(,type))
    '()))
)