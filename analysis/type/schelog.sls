(library (scheme-langserver analysis type schelog.sls)
  (export 
    construct/extend-predicates
    collect-type-satisfication-and-build-numeric-tower
    construct-variable-type-predicates
    construct-type-satisfication-predicates)
  (import 
    (chezscheme)
    (rebottled schelog)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)
    (scheme-langserver util dedupe))

(define construct/extend-predicates
  (case-lambda 
    [() (%rel ())]
    [(set head . rst)
      (map 
        (lambda (tail)
          (%assert set [(head tail)]))
      rst)]))

(define (collect-type-satisfication-and-build-numeric-tower identifier-reference-list)
  (let* ([all-types
      (dedupe (apply append
        (map 
          (lambda (identifier-reference) 
            (apply append 
              (map expand-or (identifier-reference-type-expressions identifier-reference)))))
        identifier-reference-list))]
      [set (construct-type-satisfication-predicates)]
    (let loop ([body all-types]
        [result set])
      (if (null? body)
        (add-numeric-tower result)
        (loop (cdr body) (construct-type-satisfication-predicates (car body))))))))

(define (add-numeric-tower set)
  (let* ([all-type-header (map car (private-find-all-types-without-something? set))]
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
      [number?s (filter (lambda (t) (equal? 'number? (identifier-reference-identifier header))) targets)]
      
      [set0 (private-for-numeric-tower set number?s complex?x)]
      [set1 (private-for-numeric-tower set0 (append number?s complex?s) real?x)]
      [set2 (private-for-numeric-tower set1 (append number?s complex?s real?x) rational?s)]
      [set3 (private-for-numeric-tower set2 (append number?s complex?s real?x rational?s) flonum?s)]
      [set4 (private-for-numeric-tower set3 (append number?s complex?s real?x rational?s flonum?s) cflonum?s)]
      [set5 (private-for-numeric-tower set4 (append number?s complex?s real?x rational?s flonum?s cflonum?s) integer?s)]
      [set6 (private-for-numeric-tower set5 (append number?s complex?s real?x rational?s flonum?s cflonum?s integer?s) bignum?s)]
      [set7 (private-for-numeric-tower set6 (append number?s complex?s real?x rational?s flonum?s cflonum?s integer?s bignum?s) fixnum?s)])
    set7))

(define (private-for-numeric-tower set list0 list1)
  (let loop ([body list0] [result set])
    (if (null? body)
      result
      (loop (cdr body) (construct-type-satisfication-predicates result (car list0) list1)))))

(define construct-variable-type-predicates
  (case-lambda
    [(identifier-reference-list) 
      (construct-variable-type-predicates (construct/extend-predicates) identifier-reference-list)]
    [(set identifier-reference-list)
      (let loop ([result set]
          [body identifier-reference-list])
        (if (null? body)
          result
          (let ([current-identifier-reference (car identifier-reference-list)]
              [rest (cdr identifier-reference-list)]
              [head (identifier-reference-index-node current-identifier-reference)]
              [tails (identifier-reference-type-expressions current-identifier-reference)])
            (loop
              (construct/extend-predicates 
                result 
                (if (null? head)
                  (identifier-reference-identifier current-identifier-reference)
                  head)
                  tails)
              rest))))]))

(define (private-find-all-types-without-something? set)
  (let loop ([current (%which (what) (set '(something? x) what))] 
      [result '()])
    (if current 
      result
      (loop (%more) (cadar current)))))

(define construct-type-satisfication-predicates
  (case-lambda 
    [() (construct/extend-predicates 
          (construct/extend-predicates) 
          '(something? x)
          '())]
    [(type) 
      (construct-type-satisfication-predicates 
        (construct/extend-predicates 
          (construct/extend-predicates) 
          '(something? x)
          '())
        '(something? x)
        type)]
    [(set satisfication-head type)
      (let loop ([current (%which (what) (set what satisfication-head))]
          [result set])
        (if current
          (loop (%more) (construct/extend-predicates set (cadar current) type))
          set))]
    [(set satisfication-head type . rest)
      (let ([result (construct-type-satisfication-predicates set satisfication-head type)])
        (if (null? rest)
          result
          (construct-type-satisfication-predicates set satisfication-head (car rest) (cdr rest))))]))
)