(library (scheme-langserver analysis type domain-specific-language interpreter)
  (export 
    type:interpret
    type:interpret-result-list
    type:environment-result-list
    type:solved?
    type:partially-solved?
    type:depature&interpret->result-list
    type:recursive-interpret-result-list

    type:interpret->strings

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
    (ufo-try)

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
(define PRIVATE-MAX-RECURSION 2)
(define PRIVATE-MAX-RECURSION-SET-SIZE 400)
(define PRIVATE-MAX-CARTESIAN-PRODUCT 50000)

(define (type:interpret->strings target)
  (map inner:type->string (type:interpret-result-list (private-substitute-variable&macro target))))

(define (private-substitute-variable&macro target)
  (cond 
    [(inner:macro? target) 'something?]
    [(variable? target) 'something?]
    [(null? target) target]
    [(symbol? target) target]
    [(list? target) 
      (let ([tmp (map private-substitute-variable&macro target)])
        (if (equal? 'something? (car tmp))
          'something?
          tmp))]
    [(and (list? target) (equal? 'something? (car target))) 'something?]
    [else target]))
;;;;;;;;;;;;;;;;;;;;;;;;;;type equity;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type:->?
  (case-lambda
    [(left right env) (type:->? left right env PRIVATE-MAX-DEPTH)]
    [(left right env max-depth) 
      (cond
        [(equal? left right) #t]
        [(equal? right 'something?) #t]
        [(and (identifier-reference? left) (inner:record? right)) 
          (equal? left (cadr right))]
        [(and (inner:record? right) (identifier-reference? right)) 
          (equal? right (cadr left))]
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

(define type:partially-solved? 
  (case-lambda 
    [(expression) (type:partially-solved? expression 1)]
    [(expression minium-solved-leaves) 
      (letrec ([get-leaves 
            (lambda (current-expression)
              (if (list? current-expression)
                (apply append (map get-leaves current-expression))
                `(,current-expression)))])
        (<= minium-solved-leaves (length (filter type:solved? (get-leaves expression))))) ]))

(define type:recursive-interpret-result-list
  (case-lambda 
    [(expression env) (type:recursive-interpret-result-list expression env PRIVATE-MAX-DEPTH PRIVATE-MAX-RECURSION PRIVATE-MAX-RECURSION-SET-SIZE)]
    [(expression env max-depth max-recursion max-recursion-set-size) 
      ; (debug:pretty-print-substitution (type:environment-substitution-list env))
      (let loop ([i 0]
          [target-expression-list `(,expression)]
          [env-iterator (make-type:environment (type:environment-substitution-list env))]
          [result '()])
        (if (or (<= max-recursion i) (<= max-recursion-set-size (length target-expression-list)))
          (dedupe-deduped result target-expression-list)
          (let* ([r0 
                (fold-left
                  dedupe-deduped
                  '()
                  (map
                    (lambda (e) (type:depature&interpret->result-list e env-iterator max-depth))
                    target-expression-list))]
              [r1 (filter type:solved? r0)]
              [s0 (type:environment-substitution-list env-iterator)]
              [s1 (remove-from-substitutions s0 (lambda (i) (equal? expression (car i))))]
              [s2 (fold-left add-to-substitutions s1 (map (lambda(i) `(,expression = ,i)) r1))])
            (loop 
              (+ 1 i)
              (filter (lambda (maybe) (not (type:solved? maybe))) r0)
              (make-type:environment s2)
              (append result r1)))))]))

(define type:depature&interpret->result-list
  (case-lambda
    [(expression env) (type:depature&interpret->result-list expression env PRIVATE-MAX-DEPTH)]
    [(expression env max-depth)
      ; (pretty-print 'depature)
      ; (pretty-print expression)
      (cond
        [(inner:executable? expression) (type:interpret-result-list expression env '() max-depth)]
        [(and (list? expression) (inner:contain? expression inner:macro?)) 
          (fold-left 
            (lambda (l r) (dedupe-deduped l (type:interpret-result-list r env '() max-depth)))
            '()
            (apply 
              (private-generate-cartesian-product-procedure)
              ; cartesian-product
              (map (lambda (item) (type:depature&interpret->result-list item env max-depth)) expression)))]
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
              [((? inner:macro? l) params ...)
                (type:environment-result-list-set! 
                  env 
                  (apply append 
                    (map 
                      (lambda (for-template) 
                        ;avoid nested macros
                        (if (inner:contain? for-template inner:macro?)
                          (list expression)
                          (try
                            (type:interpret-result-list (macro-head-execute-with expression for-template) env new-memory)
                            ;thie except branch brings a problem that the expression may nest many things into itself 
                            ;and leads to non-stop result.
                            (except c [else (list expression)]))))
                      (apply 
                        (private-generate-cartesian-product-procedure)
                        ; cartesian-product 
                        (map (lambda (item) (type:interpret-result-list item env new-memory)) params)))))]
              [((? inner:lambda? l) params ...)
                (if (inner:list? (inner:lambda-param l))
                  (if (inner:contain? l variable?)
                    (if (candy:matchable? (inner:list-content (inner:lambda-param l)) params) 
                      (try
                        (type:environment-result-list-set! env 
                          (type:interpret-result-list 
                            (private-with (inner:lambda-return l) (candy:match-left (inner:list-content (inner:lambda-param l)) params))
                            env
                            new-memory))
                        (except c (type:environment-result-list-set! env '())))
                      (type:environment-result-list-set! env '()))
                    (type:environment-result-list-set! env (list (inner:lambda-return l))))
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
          [(or (inner:list? expression) (inner:vector? expression) (inner:pair? expression) (inner:lambda? expression))
            (type:environment-result-list-set! env 
              (apply 
                (private-generate-cartesian-product-procedure)
                ; cartesian-product 
                (map (lambda (item) (type:interpret-result-list item env new-memory)) expression)))]
          ;'list?' deeply involved the syntax of the DSL, though it's acturally not the case in DSL.
          ;This senario means current expression is not strict inner type expression, but after some 
          ;process on macro and triangular substitution, it may bring a executable one.
          ;If here's no "not", it will leads to error because of its item maybe failed macro indicated
          ;by above "except" branch. The only solution is type:depature&interpret->result-list.
          [(and (list? expression) (not (inner:contain? expression inner:macro?)))
            (let ([filtered 
                  (filter
                    (lambda (r) (or (inner:macro? r) (inner:lambda? r)))
                    (type:interpret-result-list (car expression) env new-memory))])
              (type:environment-result-list-set! 
                env 
                (if (null? filtered)
                  `(,expression)
                  (apply append 
                    (map 
                      (lambda (r) (type:interpret-result-list `(,r ,@(cdr expression)) env new-memory))
                      filtered)))))]
          [else (type:environment-result-list-set! env (list expression))]))
      (type:environment-result-list-set! 
        env 
        (dedupe (type:environment-result-list env)))
      ; (pretty-print 'bye0)
      ; (pretty-print expression)
      ; (pretty-print 'bye1)
      ; (pretty-print (length memory))
      ; (pretty-print (length (type:environment-result-list env)))
      ; (pretty-print (type:environment-result-list env))
      env]
    [(expression env memory) (type:interpret expression env memory PRIVATE-MAX-DEPTH)]
    [(expression env) (type:interpret expression env '())]
    [(expression) (type:interpret expression (make-type:environment '()) '())]))

(define (macro-head-execute-with expression interpreted-inputs)
  (match expression
    [(('with-type ((? inner:macro-template? denotions) **1) body) (? inner:trivial? inputs) **1) 
      (execute-macro `((with-type ,denotions ,body) ,@interpreted-inputs))]
    [else (raise 'macro-not-match:macro-head-execute-with)]))

(define (execute-macro expression)
  (match expression
    [(('with-type ((? inner:macro-template? denotions) **1) body) (? inner:trivial? inputs) **1)
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
          ; [(variable? input) (private-substitute denotion left input)]
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
              (raise 'macro-not-match:private-with-list?))]
          [else (raise 'macro-not-match:private-with-else)])))
    body 
    match-pairs))

(define (private-substitute tree from to)
  (if (equal? tree from)
    to
    (if (list? tree)
      (map (lambda (item) (private-substitute item from to)) tree)
      tree)))

(define private-generate-cartesian-product-procedure 
  (case-lambda 
    [() (private-generate-cartesian-product-procedure PRIVATE-MAX-CARTESIAN-PRODUCT)]
    [(max) 
      (lambda target-list
        (cdr 
          (fold-left 
            (lambda (left right)
              (if (car left)
                `(#t . ,(cdr left))
                (let* ([filtered-result (map right (cdr left))]
                    [amount (apply * (map length filtered-result))])
                  (if (> max amount)
                    `(#t . ,(apply cartesian-product filtered-result))
                    `(#f . ,filtered-result)))))
            `(#f . ,target-list)
            (list 
              (lambda (i) i)
              (lambda (i) (filter type:partially-solved? i))
              (lambda (i) (filter (lambda (r) (type:partially-solved? r 2)) i))
              (lambda (i) (filter (lambda (r) (type:partially-solved? r 3)) i))
              (lambda (i) (filter type:solved? i))
              (lambda (i) (filter (lambda (oh-my-god) #f) i))))))]))
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