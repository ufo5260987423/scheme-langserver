(library (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (export 
    inner:trivial?

    inner:lambda?
    inner:lambda-param
    inner:lambda-return

    inner:record?
    inner:record-properties
    inner:record-predicator
    inner:record-variable

    inner:record-lambda?
    inner:record-lambda?
    inner:record-lambda-record-predicator
    inner:record-lambda-identifier
    inner:record-lambda-type
    inner:record-lambda-return

    inner:list?
    inner:list-content

    inner:with-macro

    inner:vector?
    inner:pair?
    inner:pair-car
    inner:pair-cdr
    inner:executable?)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util contain)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define (inner:trivial? pre-expression)
  (let ([expression (inner:with-macro pre-expression)])
    (cond
      [(private-inner:trivial-item? expression) #t]
      [(or (inner:list? expression) (inner:vector? expression) (inner:pair? expression))
        (if (or 
            (contain? (cdr expression) '<-)
            (contain? (cdr expression) 'list?)
            (contain? (cdr expression) 'vector?)
            (contain? (cdr expression) 'pair?))
          #f
          (fold-left
            (lambda (left right)
              (and left right))
            #t
            (map inner:trivial? (cdr expression))))]
      [(inner:lambda? expression) #t]
      [(inner:record? expression) #t]
      [(inner:record-lambda? expression) #t]
      [(inner:executable? expression) #t]
      [else #f])))

(define (inner:with-macro expression)
  (match expression
    [(('with ((? symbol? denotions) **1) body) input **1)
      (try
        (inner:with-macro 
          (fold-left
            (lambda (left pair)
              (private-substitute left (car pair) (cdr pair)))
            body 
            (candy:match denotions input)))
        (except c [else (raise (list c 'macro-error))]))]
    [else expression]))

(define (private-substitute tree from to)
  (if (equal? tree from)
    to
    (if (list? tree)
      (map (lambda (item) (private-substitute item from to)) tree)
      tree)))

(define (private-inner:trivial-item? item)
  (cond
    [(variable? item) #t]
    [(identifier-reference? item) #t]
    ;NOTE: variable is different from something! Porque variable is more like undefined, and something is defined.
    ;Or in Hott's tongue, something is confired as one of universe. And variable haven't been desided.
    [(equal? 'something? item) #t]
    [(equal? 'void? item) #t]
    [else #f]))

(define (inner:executable? body)
  (match body
    [((? inner:lambda? head) (? inner:trivial? tail) ...) #t]
    ; [((? variable? head) (? inner:trivial? tail) ...) #t]
    ; [((? identifier-reference? head) (? inner:trivial? tail) ...) #t]
    [((? inner:record-lambda? head) (? inner:record? tail) (? inner:trivial? tail)) #t]
    [((? inner:record-lambda? head) (? inner:record? tail)) #t]
    [else #f]))

(define (inner:record? body)
  (match body
    [('record? (? identifier-reference? predicator) (? variable? variable) ('pair? (? identifier-reference? ref) (? inner:trivial? type-value)) ...) #t]
    [('record? (? identifier-reference? predicator) (? null? non-variable) ('pair? (? identifier-reference? ref) (? inner:trivial? type-value)) ...) #t]
    [else #f]))

(define (inner:record-variable body)
  (if (inner:record? body)
    (caddr body)
    '()))

(define (inner:record-properties body)
  (if (inner:record? body)
    (cdddr body)
    '()))

(define (inner:record-predicator body)
  (if (inner:record? body)
    (cadr body)
    '()))

(define (inner:record-lambda? body)
  (match body
    [('void? 
        '<-record-set! 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? ref)) #t]
    [((? inner:trivial? return) 
        '<-record-ref 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? ref)) #t]
    [((? inner:record? return) 
        '<-record-constructor 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? constructor)) #t]
    [else #f]))

(define (inner:record-lambda-record-predicator body)
  (match body
    [('void? 
        '<-record-set! 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? ref)) record-predicator]
    [((? inner:trivial? return) 
        '<-record-ref 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? ref)) record-predicator]
    [((? inner:record? return) 
        '<-record-constructor 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? constructor)) record-predicator]))

(define (inner:record-lambda-type body)
  (cadr body))

(define (inner:record-lambda-return body)
  (car body))

(define (inner:record-lambda-identifier body)
  (match body
    [('void? 
        '<-record-set! 
        (? identifier-reference? record-predicator) 
        ;here we use ref, it's consistent to inner:record's properties
        (? identifier-reference? ref)) ref]
    [((? inner:trivial? return) 
        '<-record-ref 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? ref)) ref]
    [((? inner:record? return) 
        '<-record-constructor 
        (? identifier-reference? record-predicator) 
        (? identifier-reference? constructor)) constructor]))

(define (inner:lambda? body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) #t]
    [((? inner:trivial? head) '<- (? variable? tail)) #t]
    [else #f]))

(define (inner:lambda-param body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) tail]
    [((? inner:trivial? head) '<- (? variable? tail)) tail]
    [else '()]))

(define (inner:lambda-return body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) head]
    [((? inner:trivial? head) '<- (? variable? tail)) head]
    [else '()]))

(define (inner:list? body)
  (match body
    [('list? item ...) 
      (and 
        (candy:segmentable? item)
        (fold-left 
          (lambda (left right)
            (and left (inner:trivial? right)))
          #t
          (filter 
            (lambda (t) (and (not (equal? t '...)) (not (equal? t '**1)))) 
            item)))]
    [else #f]))

(define (inner:list-content body)
  (match body
    [('list? (? inner:trivial? item) ...) item]
    [else #f]))

(define (inner:vector? body)
  (match body
    [('vector? (? inner:trivial? item) ...) (candy:segmentable? item)]
    [else #f]))

(define (inner:pair? body)
  (match body
    [('pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) #t]
    [else #f]))

(define (inner:pair-car body)
  (match body
    [('pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) fuzzy0]))

(define (inner:pair-cdr body)
  (match body
    [('pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) fuzzy1]))
)