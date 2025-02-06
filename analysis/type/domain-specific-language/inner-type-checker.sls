(library (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (export 
    inner:trivial?
    inner:contain?

    inner:lambda?
    inner:lambda-param
    inner:lambda-return

    inner:record?
    inner:record-properties
    inner:record-predicator

    inner:list?
    inner:list-content

    inner:macro?
    inner:macro-template?

    inner:?->pair
    inner:type->string

    inner:vector?
    inner:pair?
    inner:pair-car
    inner:pair-cdr
    inner:executable?)
  (import 
    (chezscheme)
    (ufo-match)

    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define (inner:type->string target)
  (cond
    [(null? target) "() "]
    [(and (list? target) (inner:trivial? target)) 
      (string-append "(" (apply string-append (map inner:type->string target)) ") ")]
    [(symbol? target) (string-append (symbol->string target) " ")]
    [(variable? target) (string-append "[variable " (variable-uuid target) "] ")]
    [(identifier-reference? target) (string-append "[identifier-reference " (symbol->string (identifier-reference-identifier target))  "] ")]
    [else
      (print-graph #t)
      (pretty-print target)
      (raise "can't do the transformation")]))

(define (inner:?->pair target)
  (match target
    [(inner:list? item _ **1) `(inner:pair? ,item ,(inner:?->pair `(inner:list? ,@_)))]
    [else target]))

(define (inner:trivial? expression)
  (cond
    [(private-inner:trivial-item? expression) #t]
    [(inner:list? expression) #t]
    [(inner:vector? expression) #t]
    [(inner:pair? expression) #t]
    [(inner:lambda? expression) #t]
    [(inner:macro? expression) #t]
    [(inner:record? expression) #t]
    [(inner:executable? expression) #t]
    [else #f]))

(define (inner:macro? expression)
  (match expression
    [('with ((? inner:macro-template? denotions) **1) body) #t]
    [else #f]))

(define (inner:macro-template? expression)
  (cond
    [(list? expression) 
      (fold-left
        (lambda (left right)
          (and left (inner:macro-template? right)))
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

(define (private-inner:trivial-item? item)
  (cond
    [(variable? item) #t]
    [(identifier-reference? item) #t]
    ;NOTE: variable is different from something! Porque variable is more like undefined, and something is defined.
    ;Or in Hott's tongue, something is confired as one of universe. And variable haven't been desided.
    [(equal? 'something? item) #t]
    [(equal? 'void? item) #t]
    [else #f]))

;inner:macro? cases should be excuted before inner:lambda? cases
;so, excecpt inner:macro? should be exclude in other cases
(define (inner:executable? body)
  (match body
    [((? inner:lambda? head) (? inner:trivial? tail) ...) 
      (not (inner:contain? body inner:macro?))]
    [((? inner:macro? head) (? inner:trivial? tail) ...) #t]
    ; [((? variable? head) (? inner:trivial? tail) ...) #t]
    ; [((? identifier-reference? head) (? inner:trivial? tail) ...) #t]
    [else #f]))

(define (inner:contain? body func)
  (cond 
    [(func body) #t]
    [(list? body) 
      (fold-left
        (lambda (l r)
          (if l #t (inner:contain? r func)))
        #f
        body)]
    [else #f]))

(define (inner:record? body)
  (match body
    [('inner:record? (? identifier-reference? predicator) ('inner:pair? (? identifier-reference? ref) (? inner:trivial? type-value)) ...) #t]
    [else #f]))

(define (inner:record-properties body)
  (if (inner:record? body)
    (cddr body)
    '()))

(define (inner:record-predicator body)
  (if (inner:record? body)
    (cadr body)
    '()))

(define (inner:lambda? body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) #t]
    [((? inner:trivial? head) '<- (? inner:pair? tail)) #t]
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
    [('inner:list? item ...) 
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
    [('inner:list? item ...) item]
    [else '()]))

(define (inner:vector? body)
  (match body
    [('inner:vector? (? inner:trivial? item) ...) (candy:segmentable? item)]
    [else #f]))

(define (inner:pair? body)
  (match body
    [('inner:pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) #t]
    [else #f]))

(define (inner:pair-car body)
  (match body
    [('inner:pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) fuzzy0]
    [else '()]))

(define (inner:pair-cdr body)
  (match body
    [('inner:pair? (? inner:trivial? fuzzy0) (? inner:trivial? fuzzy1)) fuzzy1]
    [else '()]))
)