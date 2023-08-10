(library (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (export 
    inner:trivial?

    inner:lambda?
    inner:lambda-param
    inner:lambda-return

    inner:list?
    inner:vector?
    inner:pair?
    inner:executable?)
  (import 
    (rnrs)
    (ufo-match)

    (scheme-langserver util contain)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define (inner:trivial? expression)
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
    [(inner:lambda? expression)
      (if (inner:vector? (inner:lambda-param expression))
        #f
        (and 
          (inner:trivial? (inner:lambda-param expression))
          (inner:trivial? (inner:lambda-return expression))))]
    [(inner:executable? expression) #t]
    [else #f]))

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
    [((? variable? head) (? inner:trivial? tail) ...) #t]
    [((? identifier-reference? head) (? inner:trivial? tail) ...) #t]
    [else #f]))

; (define (inner:record? body)
;   (match body
;     [('record? (? symbol? type) ('pair? method-name value) **1) #t]
;     [else #f]))

; (define (inner:record-lambda? body)
;   (match body
;     [('record-lambda? (? inner:record? record) (? inner:lambda? method)) #t]
;     [else #f]))

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
    [('list? (? inner:trivial? item) ...) (candy:segmentable? item)]
    [else #f]))

(define (inner:vector? body)
  (match body
    [('vector? (? inner:trivial? item) ...) (candy:segmentable? item)]
    [else #f]))

(define (inner:pair? body)
  (match body
    [('pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) #t]
    [else #f]))
)