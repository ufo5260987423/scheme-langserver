(library (scheme-langserver analysis type schelog.sls)
  (export construct/extend-predicates)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver analysis reference)

    (rebottled schelog))

(define construct/extend-predicates
  (case-lambda 
    [() (%rel ())]
    [(set head . rst)
      (map 
        (lambda (tail)
          (%assert set [(head tail)]))
      rst)]))

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

(define construct-type-satisfiction-predicates
  (case-lambda 
    [(type) 
      (construct-type-satisfiction-predicates 
        (construct/extend-predicates 
          (construct/extend-predicates) 
          'something?
          '())
        'something?
        type)]
    [(set satifiction-head typequery returns an answer that is a list of bindings, one for each logic variable mentioned in its second subform. Thus,

(%which (what)
)
      (let loop ([current (%which (what) (set what satification-head))]
          [result set])
        (if current
          (loop (%more) (construct/extend-predicates set (cadar current) type))
          set))]))
)