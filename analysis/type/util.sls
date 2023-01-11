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

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta))

(define-syntax construct-lambda 
  (syntax-rules ()
    [(_ body) (eval `(lambda(x) ,body))]))

(define (type-satisfy>=intersection type-expression0 type-expression1)
  (type-intersection type-expression0 type-expression1 private-satisfy>=))

(define (private-satisfy>= type0 type1)
  (if (equal? type0 type1)
    #t
    (let ([numeric0 (private-type->numeric-type type0)]
        [numeric1 (private-type->numeric-type type1)])
      (if (and numeric0 numeric1)
        (>= numeric0 numeric1)
        (or (null? type1) (equal? type0 'something?))))))

;;numeric tower
(define (private-type->numeric-type type0)
  (if (identifier-reference? type0)
    (let ([target-meta (find-meta (identifier-reference-library-instance type0))]
        [target-identifier (identifier-reference-identifier type0)])
      (if (null? target-meta)
        #f
        (let* ([target (find (lambda(m) (equal? m target-identifier)) target-meta)])
          (if (equal? target type0)
            (cond 
              [(equal? target-identifier 'fixnum?) 0]
              [(equal? target-identifier 'bignum?) 1]
              [(equal? target-identifier 'integer?) 2]
              [(equal? target-identifier 'cflonum?) 3]
              [(equal? target-identifier 'flonum?) 4]
              [(equal? target-identifier 'rational?) 5]
              [(equal? target-identifier 'real?) 6]
              [(equal? target-identifier 'complex?) 7]
              [(equal? target-identifier 'number?) 8]
              [else #f])
            #f))))
    #f))

;; truly equal should consider the identifier's document context
(define (type-intersection type-expression0 type-expression1 equal-predicator)
  (if (and (list? type-expression0) (list? type-expression1))
    (let* ([set0 (map private-dry-rule (dedupe (private-process-or type-expression0)))]
        [set1 (map private-dry-rule (dedupe (private-process-or type-expression1)))])
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