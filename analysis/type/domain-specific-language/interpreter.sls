(library (scheme-langserver analysis type domain-specific-language interpreter)
  (export 
    type:interpret
    type:interpret-result-list
    type:environment-result-list)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define-record-type type:environment
  (fields
    (mutable substitution-list)
    (mutable result-list)))

(define type:interpret-result-list
  (case-lambda 
    [(expression) (type:environment-result-list (type:interpret expression))]
    [(expression env) (type:environment-result-list (type:interpret expression env))]))

(define type:interpret 
  (case-lambda 
    [(expression env)
      (type:environment-result-list-set! env '())
      (cond
        [(inner:executable? expression)
          ;the clause sequence is important
          (match expression
            ;todo
            [((? inner:record-lambda? l) params ...) 
              (match (inner:record-lambda-type l)
                ['<-record-set!
                  (if (= 2 (length params))
                    (if (equal? (car params) (inner:record-lambda-record-predicator l))
                      '()
                        ))
                ]
                ['<-record-ref
                  (if (= 1 (length params))
                    (if (equal? (car params) (inner:record-lambda-record-predicator l))
                      '()
                        ))
                  '()
                ]
                ['<-record-constructor
                  '()
                ])]
            [((? inner:lambda? l) params ...)
              (if (inner:list? (inner:lambda-param l))
                (if (private-matchable? 
                    (type:interpret-result-list (inner:list-content (inner:lambda-param l)) env)
                    (apply cartesian-product (map (lambda(param) (type:interpret-result-list param env)) params)))
                  (type:environment-result-list-set! env (list (inner:lambda-return l)))
                  (type:environment-result-list-set! env '()))
                (type:environment-result-list-set! env (list (inner:lambda-return l))))]
            [else expression])]
        [(or (inner:list? expression) (inner:vector? expression) (inner:pair? expression) (inner:lambda? expression) (inner:record? expression))
          (type:environment-result-list-set! env 
            (fold-left 
              (lambda (result param)
                (apply cartesian-product `(,@result ,(type:interpret-result-list param env))))
              '()
              expression))]
        [else (type:environment-result-list-set! env (list expression))])
      env]
    [(expression)
      (type:interpret expression (make-type:environment '() '()))]))

(define private-matchable? 
  (case-lambda 
    [(cartesian-product-list)
      (if (null? cartesian-product-list)
        #f
        (if (apply candy:matchable? (car cartesian-product-list))
          #t
          (private-matchable? (cdr cartesian-product-list))))]
    [(a-list b-list)
      (private-matchable? (cartesian-product a-list b-list))]))
)