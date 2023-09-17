(library (scheme-langserver analysis type domain-specific-language interpreter)
  (export 
    type:interpret
    type:interpret-result-list
    type:environment-result-list

    substitution:walk

    make-type:environment)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util binary-search)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)
    (scheme-langserver util dedupe)
    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define-record-type type:environment
  (fields
    (mutable substitution-list)
    (mutable result-list))
  (protocol
    (lambda (new)
      (lambda (substitution-list)
        (new substitution-list '())))))

(define type:interpret-result-list
  (case-lambda 
    [(expression) (type:environment-result-list (type:interpret expression))]
    [(expression env) (type:environment-result-list (type:interpret expression env))]
    [(expression env memory) (type:environment-result-list (type:interpret expression env memory))]))

(define type:interpret 
  (case-lambda 
    [(expression env memory)
      (type:environment-result-list-set! env '())
      ; (if (not (contain? memory expression))
      ;   (begin
      ;     (print-graph #t)
      ;     (pretty-print 'interpret)
      ;     (pretty-print (length memory))
      ;     (pretty-print expression)))
      (let ([new-memory (dedupe `(,@memory ,expression))])
        (cond
          [(contain? memory expression) 
            (type:environment-result-list-set! env `(,expression))]
          [(inner:executable? expression)
            ;the clause sequence is important
            (match expression
              ;todo
              [((? inner:record-lambda? l) (? inner:record? record) (? inner:trivial? params) ...) 
                (match (inner:record-lambda-type l)
                  ['<-record-set!
                    (if (and (= 1 (length params)) (equal? (inner:record-lambda-record-predicator l) (inner:record-predicator record)))
                      (begin
                        (type:environment-result-list-set! 
                          env
                          ('void?))
                        (type:environment-substitution-list-set! 
                          env
                          (fold-left
                            add-to-substitutions 
                            (type:environment-substitution-list env)
                            (map 
                              (lambda (result)
                                `(,(inner:record-variable record) = ,result))
                              (filter 
                                (lambda (r)
                                  (variable? (inner:record-variable record)))
                                (type:interpret-result-list (car params) env new-memory))
                              )))))]
                  ['<-record-ref
                    (if (and (null? params) (equal? (inner:record-lambda-record-predicator l) (inner:record-predicator record)))
                      (type:environment-result-list-set! 
                        env
                        (map 
                          (lambda (item) (car (reverse item)))
                          (filter (lambda (property) (equal? (inner:pair-car property) (cadddr l))) (inner:record-properties record)))))]
                  ['<-record-constructor
                    ; (if (private-matchable? 
                    ;     (type:interpret-result-list (inner:record-lambda-params l) env)
                    ;     (apply cartesian-product (map (lambda(param) (type:interpret-result-list param env)) params)))
                        ;todo:use real constructor!
                      (type:environment-result-list-set! env (list (inner:record-lambda-return l)))
                      ; (type:environment-result-list-set! env '()))
                      ])]
              [((? inner:lambda? l) params ...)
                (if (inner:list? (inner:lambda-param l))
                  (if (private-matchable? 
                      (type:interpret-result-list (inner:list-content (inner:lambda-param l)) env new-memory)
                      (apply 
                        cartesian-product 
                        (map (lambda (param) (type:interpret-result-list param env new-memory)) params)))
                    (type:environment-result-list-set! env (list (inner:lambda-return l)))
                    (type:environment-result-list-set! env '()))
                  (type:environment-result-list-set! env (list (inner:lambda-return l)))) ]
              [else expression])]
          [(variable? expression)
            (let ([maybe
                  (fold-left
                    (lambda (left reified-item) `(,@left ,@(type:interpret-result-list reified-item env new-memory)))
                    '()
                    (filter 
                      (lambda (item) 
                        (and (not (null? item)) (not (contain? new-memory item)))) 
                      (map caddr (substitution:walk (type:environment-substitution-list env) expression))))])
              (type:environment-result-list-set! env (if (null? maybe) `(,expression) maybe)))]
          [(macro? expression)
            (let ([inputs 
                  (map 
                    (lambda (item) (type:interpret-result-list item env new-memory)) 
                    (macro-inputs expression))])
              (type:environment-result-list-set! env 
                (if (zero? (length inputs))
                  '()
                  (apply append 
                    (map 
                      (lambda (for-template) 
                        (try
                          (type:interpret-result-list (macro-head-execute-with expression for-template) env new-memory)
                          (except c [else '()])))
                      (apply cartesian-product inputs))))))]
          [(or (inner:list? expression) (inner:vector? expression) (inner:pair? expression) (inner:lambda? expression) (inner:record? expression))
            (type:environment-result-list-set! env (apply cartesian-product (map (lambda (item) (type:interpret-result-list item env new-memory)) expression)))]
          [(list? expression)
            (type:environment-result-list-set! 
              env 
                (apply append 
                  (map 
                    (lambda (type) (type:interpret-result-list type env new-memory))
                    (apply 
                      cartesian-product 
                      (map (lambda (item) (type:interpret-result-list item env new-memory)) expression)))))]
          [else (type:environment-result-list-set! env (list expression))]))
      ; (if (not (contain? memory expression))
      ;   (begin
      ;     (print-graph #t)
      ;     (pretty-print 'byebye)
      ;     (pretty-print (length memory))
      ;     (pretty-print expression)))
      env]
    [(expression env) (type:interpret expression env '())]
    [(expression) (type:interpret expression (make-type:environment '()) '())]))

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

(define (macro? expression)
  (match expression
    [(('with ((? private-macro-template? denotions) **1) body) (? inner:trivial? inputs) **1) #t]
    [else #f]))

(define (macro-inputs expression)
  (match expression
    [(('with ((? private-macro-template? denotions) **1) body) (? inner:trivial? inputs) **1) inputs]
    [else '()]))

(define (macro-head-execute-with expression interpreted-inputs)
  (match expression
    [(('with ((? private-macro-template? denotions) **1) body) (? inner:trivial? inputs) **1) 
      (execute-macro `((with ,denotions ,body) ,@interpreted-inputs))]
    [else (raise 'macro-not-match)]))

(define (private-macro-template? expression)
  (cond
    [(list? expression) 
      (fold-left
        (lambda (left right)
          (and left (private-macro-template? right)))
        #t
        expression)]
    [(symbol? expression) 
      (cond
        [(equal? expression 'something?) #f]
        [(equal? expression 'void?) #f]
        [(equal? expression '<-) #f]
        [(equal? expression '<-record-ref) #f]
        [(equal? expression '<-record-set!) #f]
        [(equal? expression '<-record-constructor) #f]
        [(equal? expression 'inner:list?) #f]
        [(equal? expression 'inner:pair?) #f]
        [(equal? expression 'inner:vector?) #f]
        [(equal? expression 'inner:record?) #f]
        [else #t])]
    [else #f]))

(define (execute-macro expression)
  (match expression
    [(('with ((? private-macro-template? denotions) **1) body) (? inner:trivial? inputs) **1)
      (execute-macro (private-with body (candy:match-left denotions inputs)))]
    ;only usable in with-macro
    [('with-append (? list? a) (? list? b)) (execute-macro (append a b))]
    ;only usable in with-macro
    [('with-equal? a b body) (if (equal? a b) (execute-macro body) expression)]
    [else expression]))

(define (private-with body match-pairs)
  (fold-left
    (lambda (left pair)
      (let ([denotion (car pair)]
          [input (cdr pair)])
        (cond 
          [(symbol? denotion) (private-substitute left denotion input)]
          [(and (list? denotion) (list? input)) 
            (private-with body (candy:match-left denotion input))]
          [else (raise 'macro-not-match)])))
    body 
    match-pairs))

(define (private-substitute tree from to)
  (if (equal? tree from)
    to
    (if (list? tree)
      (map (lambda (item) (private-substitute item from to)) tree)
      tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;substitutions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (substitution:walk substitutions target)
  (binary-search 
    (list->vector substitutions) 
    substitution-compare 
    `(,target '? '?)))

(define (debug:substitution-sorted? substitutions)
  (let loop ([l substitutions]
      [s (sort substitution-compare substitutions)])
    (cond 
      [(and (null? l) (null? s)) #t]
      [(or (null? l) (null? s)) #f]
      [(equal? (car (car l)) (car (car s))) (loop (cdr l) (cdr s))]
      [else 
        (pretty-print 'debug:sorted-origin)
        (pretty-print (car l))
        (pretty-print 'debug:sorted-sorted)
        (pretty-print (car s))
        #f])))
)