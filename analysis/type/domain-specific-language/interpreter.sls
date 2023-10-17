(library (scheme-langserver analysis type domain-specific-language interpreter)
  (export 
    type:interpret
    type:interpret-result-list
    type:environment-result-list
    type:solved?
    type:depature&interpret->result-list

    type:->?
    type:<-?
    type:=?

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

(define PRIVATE-MAX-DEPTH 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;type equity;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type:->?
  (case-lambda
    [(left right env) (type:->? left right env PRIVATE-MAX-DEPTH)]
    [(left right env max-depth) 
      (cond
        [(equal? left right) #t]
        [(equal? right 'something?) #t]
        [(and (identifier-reference? left) (identifier-reference? right)) 
          (if (null? (identifier-reference-parents right))
            #f
            (if (contain? (identifier-reference-parents right) left)
              #t
              (fold-left
                (lambda (l r)
                  (if l
                    (type:->? left r env max-depth)
                    #f))
                #t
                (identifier-reference-parents right))))]
        ; [(and (inner:lambda? left) (inner:lambda? right)) ]
        [(and (list? left) (list? right)) 
          (let* ([processed-left (inner:?->pair left)]
              [processed-right (inner:?->pair right)])
            (if (candy:matchable? processed-left processed-right)
              (fold-left
                (lambda (l pair)
                  (if l
                    (type:->? (car pair) (cdr pair) env)
                    #f))
                #t
                (candy:match-left processed-left processed-right))
              #f))]
        [else  (contain? (type:interpret-result-list left env '() max-depth) right)])]))

(define type:<-?
  (case-lambda
    [(left right env) (type:->? right left env PRIVATE-MAX-DEPTH)]
    [(left right env max-depth) (type:->? right left env max-depth)]))

(define type:=?
  (case-lambda
    [(left right env) (and (type:->? left right env PRIVATE-MAX-DEPTH) (type:<-? left right env PRIVATE-MAX-DEPTH))]
    [(left right env max-depth) (and (type:->? left right env max-depth) (type:<-? left right env max-depth))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type:interpret-result-list
  (case-lambda 
    [(expression) (type:environment-result-list (type:interpret expression))]
    [(expression env) (type:environment-result-list (type:interpret expression env))]
    [(expression env memory) (type:environment-result-list (type:interpret expression env memory))]
    [(expression env memory max-depth) (type:environment-result-list (type:interpret expression env memory max-depth))]))

(define (type:solved? expression)
  (cond
    [(variable? expression) #f]
    [(inner:macro? expression) #f]
    [(inner:executable? expression) #f]
    [(list? expression)
      (if (not (inner:trivial? expression))
        #f
        (fold-left 
          (lambda (r l)
            (and r (type:solved? l)))
          #t
          expression))]
    [else #t]))

(define type:depature&interpret->result-list
  (case-lambda
    [(expression env) (type:depature&interpret->result-list expression env PRIVATE-MAX-DEPTH)]
    [(expression env max-depth)
      (cond
        [(inner:executable? expression) (type:interpret-result-list expression env '() max-depth)]
        [(and (list? expression) (inner:contain? expression inner:macro?)) 
          (dedupe (apply append 
            (map 
              (lambda (item) (type:interpret-result-list item env '() max-depth))
              (apply cartesian-product
                (map (lambda (item) (type:depature&interpret->result-list item env max-depth)) expression)))))]
        [else (type:interpret-result-list expression env '() max-depth)])]))

(define type:interpret 
  (case-lambda 
    [(expression env memory max-depth)
      (type:environment-result-list-set! env '())
      ; (pretty-print 'interpret)
      ; (print-graph #t)
      ; (pretty-print (length memory))
      ; (pretty-print expression)
      (let ([new-memory `(,@memory ,expression)])
        (cond
          [(null? expression) expression]
          [(<= max-depth (length memory)) 
            ; (pretty-print 'max)
            ; (print-graph #t)
            ; (pretty-print memory)
            (type:environment-result-list-set! env `(,expression))]
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
              [((? inner:macro? l) params ...)
                (type:environment-result-list-set! 
                  env 
                  (apply append 
                    (map 
                      (lambda (for-template) 
                        (try
                          (type:interpret-result-list (macro-head-execute-with expression for-template) env new-memory)
                          ;thie except branch brings a problem that the expression may nest many things into itself 
                          ;and leads to non-stop result.
                          (except c [else (list expression)])))
                      (apply cartesian-product (map (lambda (item) (type:interpret-result-list item env new-memory)) params)))))]
              [((? inner:lambda? l) params ...)
                (if (inner:list? (inner:lambda-param l))
                  (if (candy:matchable? (inner:list-content (inner:lambda-param l)) params)
                    (type:environment-result-list-set! env 
                      (type:interpret-result-list 
                        (private-with (inner:lambda-return l) (candy:match-left (inner:list-content (inner:lambda-param l)) params))
                        env
                        new-memory))
                    (type:environment-result-list-set! env '()))
                  ; ;Assume that lambda-param, after interpreting, it won't change it form.
                  ; (let* ([pres (type:interpret-result-list (inner:lambda-param l) env new-memory)]
                  ;     [find-result (find (lambda (pre) (candy:matchable? (inner:list-content pre) params)) pres)])
                  ;   (if find-result 
                  ;     ; (type:environment-result-list-set! env (list (private-with (inner:lambda-return l) find-result)))
                  ;     (type:environment-result-list-set! env (list (inner:lambda-return l)))
                  ;     (type:environment-result-list-set! env '())))
                  (type:environment-result-list-set! env (list (inner:lambda-return l))))]
              [else expression])]
          [(variable? expression)
            (type:environment-result-list-set! 
              env 
              (apply append 
                (map 
                  (lambda (reified)
                    (if (equal? reified expression) 
                      `(,reified)
                      (type:interpret-result-list reified env new-memory)))
                  (map caddr (substitution:walk (type:environment-substitution-list env) expression)))))]
          ; [(and (inner:lambda? expression) (inner:contain? expression inner:macro?)) (type:environment-result-list-set! env `(,expression))]
          [(inner:macro? expression) (type:environment-result-list-set! env `(,expression))]
          [(or (inner:list? expression) (inner:vector? expression) (inner:pair? expression) (inner:lambda? expression) (inner:record? expression))
            (type:environment-result-list-set! env (apply cartesian-product (map (lambda (item) (type:interpret-result-list item env new-memory)) expression)))]
          ;'list?' deeply involved the syntax of the DSL, though it's acturally not the case in DSL.
          ;This senario means current expression is not strict inner type expression, but after some 
          ;process on macro and triangular substitution, it may bring a executable one.
          ;If here's no "not", it will leads to error because of its item maybe failed macro indicated
          ;by above "except" branch. The only solution is type:depature&interpret->result-list.
          [(and (list? expression) (not (inner:contain? expression inner:macro?)))
            (type:environment-result-list-set! 
              env 
              (apply append 
                (map 
                  (lambda (type) 
                    (if (equal? type expression)
                      `(,type)
                      (type:interpret-result-list type env new-memory)))
                  ;interpret first item in order to confirm is it executable or macro
                  (apply 
                    cartesian-product
                    (map 
                      (lambda (item) (type:interpret-result-list item env new-memory)) expression)))))]
          [else (type:environment-result-list-set! env (list expression))]))
      ; (pretty-print 'bye0)
      ; (pretty-print expression)
      ; (pretty-print 'bye1)
      ; (pretty-print (type:environment-result-list env))
      (type:environment-result-list-set! 
        env 
        (dedupe (type:environment-result-list env)))
      env]
    [(expression env memory) (type:interpret expression env memory PRIVATE-MAX-DEPTH)]
    [(expression env) (type:interpret expression env '())]
    [(expression) (type:interpret expression (make-type:environment '()) '())]))

(define (macro-head-execute-with expression interpreted-inputs)
  (match expression
    [(('with ((? inner:macro-template? denotions) **1) body) (? inner:trivial? inputs) **1) 
      (execute-macro `((with ,denotions ,body) ,@interpreted-inputs))]
    [else (raise 'macro-not-match)]))

(define (execute-macro expression)
  (match expression
    [(('with ((? inner:macro-template? denotions) **1) body) (? inner:trivial? inputs) **1)
      (if (candy:matchable? denotions inputs)
        (execute-macro (private-with body (candy:match-left denotions inputs)))
        expression)]
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
          [(variable? denotion) (private-substitute left denotion input)]
          ; [(identifier-reference? denotion) 
          ;   (if (type:<- denotion input env)
          ;     left
          ;     (riase 'macro-not-match))]
          [(identifier-reference? denotion) left]
          [(and (list? denotion) (list? input)) 
            (if (candy:matchable? denotion input)
              (if (or (contain? input '**1) (contain? input '...))
                (private-with body (candy:match-right denotion input))
                (private-with body (candy:match-left denotion input)))
              (raise 'macro-not-match))]
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