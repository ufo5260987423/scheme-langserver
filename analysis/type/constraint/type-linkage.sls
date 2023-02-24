(library (scheme-langserver analysis type type-linkage)
  (export 
    collect-type-satisfication-and-build-numeric-tower
    assert-recursive)
  (import 
    (chezscheme)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util sub-list)
    (scheme-langserver util matrix))

(define-record-type type-linkage
  (fields
    (mutable type-vector)
    (mutable matrix)))

(define (init-type-linkage)
  (let ([type-vector '#((something? x) ())]
      [matrix '#(0 1 0 0)])
    (make-type-linkage type-vector matrix)))

(define (assert-recursive type-linkage type0 type1)
  (if (equal? type0 type1)
    `(type0)
    (let* ([type0-id (private-get-id type-linkage type0)]
        [type1-id (private-get-id type-linkage type1)]
        [matrix (type-linkage-matrix type-linkage)]
        [froms (matrix-to matrix type1-id)]
        [recursive-result 
          (filter (lambda (item) (not (null? item)))
            (map 
              (lambda (from)
                (assert-recursive type-linkage type0 (vector-ref (type-linkage-type-vector type-linkage) from)))
              froms))])
      (if (null? recursive-result)
        '()
        (append `(,type0) recursive-result)))))

;tail is subset of head
(define linkage-add
  (case-lambda
    [(linkage tail) 
      (if (not (private-get-id linkage tail))
        (let ([matrix (type-linkage-matrix linkage)])
          (type-linkage-type-vector-set! linkage (list->vector (append (vector->list (type-linkage-type-vector linkage)) `(,tail))))
          (type-linkage-matrix-set! linkage (matrix-expand matrix))))
          (linkage-add linkage '(something? x) tail)]
    [(linkage head tail)
      (let ([head-id (private-get-id linkage head)]
          [tail-id (private-get-id linkage tail)]
          [matrix (type-linkage-matrix linkage)])
        (if (not head-id) (linkage-add linkage head))
        (if (not tail-id) (linkage-add linkage tail))
        (if (and head-id tail-id) (matrix-set! matrix head-id tail-id)))]))

(define linkage-remove
  (case-lambda 
    [(linkage tail)
      (let ([type-list (vector->list (type-linkage-type-vector linkage))]
          [matrix (type-linkage-matrix linkage)]
          [tail-id (private-get-id linkage tail)])
        ;; can't remove '(something? x)
        (if (and tail-id (not (zero? tail-id)))
          (begin
            (type-linkage-type-vector-set! linkage (list->vector (append (list-ahead-of type-list tail) (list-after type-list tail))))
            (type-linkage-matrix-set! linkage (private-remove matrix tail-id)))))]
    [(linkage head tail)
      (let ([head-id (private-get-id linkage head)]
          [tail-id (private-get-id linkage tail)]
          [matrix (type-linkage-matrix linkage)])
        (if (and head-id tail-id) (matrix-set! matrix head-id tail-id 0))
        (linkage-remove linkage head)
        (linkage-remove linkage tail))]))

(define (private-remove matrix target-id)
  (if (zero? target-id)
    matrix
    (let ([size (length matrix)]
        [to-target (matrix-to matrix target-id)]
        [from-target (matrix-from matrix target-id)]
        [new-matrix (make-vector (- size 1))])
      (map 
        (lambda (from-id)
          (map 
            (lambda (to-id)
              (if (> to-id target-id)
                (matrix-set! matrix from-id (- to-id 1)))
              (if (< to-id target-id)
                (matrix-set! matrix from-id to-id)))
            from-target))
        to-target)
      (let loop ([id 0])
        (if (< id size)
          (let* ([p (decode id)]
              [row (car p)]
              [column (cadr p)])
            (if (not (or (= row target-id) (= column target-id)))
              (matrix-set! 
                new-matrix 
                (if (< row target-id) 
                  row 
                  (- row 1))
                (if (< column target-id) 
                  column 
                  (- column 1))
                (vector-ref matrix id)))
            (loop (+ 1 id)))
          new-matrix)))))

(define (private-get-id linkage target)
  (let loop ([body (vector->list (type-linkage-type-vector linkage))] [result 0])
    (if (null? body)
      #f
      (if (equal? target (car body))
        result
        (loop (cdr body) (+ 1 result))))))

(define (collect-type-satisfication-and-build-numeric-tower linkage identifier-reference-list)
  (let loop ([body
      (dedupe (apply append
        (map 
          (lambda (identifier-reference) 
            (apply append 
              (map expand-or (identifier-reference-type-expressions identifier-reference)))))
        identifier-reference-list))]
      [linkage (init-type-linkage)])
    (if (null? body)
      (add-numeric-tower linkage)
      (loop (cdr body) (linkage-add linkage (car body))))))

(define (add-numeric-tower linkage)
  (let* ([type-vector (type-linkage-type-vector linkage)]
      [all-type-header (map car (vector->list type-vector))]
      [pre-targets (filter (lambda (header) (identifier-reference? header)) all-type-header)]
      [targets (filter (lambda (header) (null? (identifier-reference-index-node? header))) pre-targets)]

      [fixnum?s (filter (lambda (t) (equal? 'fixnum? (identifier-reference-identifier header))) targets)]
      [bignum?s (filter (lambda (t) (equal? 'bignum? (identifier-reference-identifier header))) targets)]
      [integer?s (filter (lambda (t) (equal? 'integer? (identifier-reference-identifier header))) targets)]
      [cflonum?s (filter (lambda (t) (equal? 'cflonum? (identifier-reference-identifier header))) targets)]
      [flonum?s (filter (lambda (t) (equal? 'flonum? (identifier-reference-identifier header))) targets)]
      [rational?s (filter (lambda (t) (equal? 'rational? (identifier-reference-identifier header))) targets)]
      [real?s (filter (lambda (t) (equal? 'real? (identifier-reference-identifier header))) targets)]
      [complex?s (filter (lambda (t) (equal? 'complex? (identifier-reference-identifier header))) targets)]
      [number?s (filter (lambda (t) (equal? 'number? (identifier-reference-identifier header))) targets)])
    (private-for-numeric-tower linkage number?s complex?s)
    (private-for-numeric-tower set0 (append number?s complex?s) real?x)
    (private-for-numeric-tower set1 (append number?s complex?s real?x) rational?s)
    (private-for-numeric-tower set2 (append number?s complex?s real?x rational?s) flonum?s)
    (private-for-numeric-tower set3 (append number?s complex?s real?x rational?s flonum?s) cflonum?s)
    (private-for-numeric-tower set4 (append number?s complex?s real?x rational?s flonum?s cflonum?s) integer?s)
    (private-for-numeric-tower set5 (append number?s complex?s real?x rational?s flonum?s cflonum?s integer?s) bignum?s)
    (private-for-numeric-tower set6 (append number?s complex?s real?x rational?s flonum?s cflonum?s integer?s bignum?s) fixnum?s)
    linkage))

(define (private-for-numeric-tower linkage list0 list1)
  (map 
    (lambda (type0) 
      (map 
        (lambda (type1) (linkage-add linkage type0 type1))
        list1))
    list0))
)