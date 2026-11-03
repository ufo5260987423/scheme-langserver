(library (scheme-langserver analysis type domain-specific-language inner-type-checker)
  (export 
    inner:trivial?
    inner:contain?

    inner:lambda?
    inner:lambda-param
    inner:lambda-return

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

    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node))

(define (inner:type->string target)
  (cond
    [(null? target) "() "]
    [(list? target) 
      (string-append "(" (apply string-append (map inner:type->string target)) ") ")]
    [(symbol? target) (string-append (symbol->string target) " ")]
    [(index-node? target) (string-append "[index-node-uuid " (index-node-uuid target) "] ")]
    [(identifier-reference? target) (string-append "[identifier-reference " (symbol->string (identifier-reference-identifier target))  "] ")]
    [else
      (pretty-print target)
      (raise "can't do the transformation")]))

(define (inner:trivial? expression)
  (cond
    [(private-inner:trivial-item? expression) #t]
    [(inner:list? expression) #t]
    [(inner:vector? expression) #t]
    [(inner:pair? expression) #t]
    [(inner:lambda? expression) #t]
    [(inner:macro? expression) #t]
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
        [(equal? expression 'inner:void?) #f]
        [(equal? expression '<-) #f]
        [(equal? expression 'inner:list?) #f]
        [(equal? expression 'inner:pair?) #f]
        [(equal? expression 'inner:vector?) #f]
        [else #t])]
    [else #f]))

(define (private-inner:trivial-item? item)
  (cond
    [(index-node? item) #t]
    [(identifier-reference? item) #t]
    ;NOTE: index-node/variable is different from something! Porque variable is more like undefined, and something is defined.
    ;Or in Hott's tongue, something is confired as one of universe. And variable haven't been desided.
    [(equal? 'something? item) #t]
    [(equal? 'inner:void? item) #t]
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

(define (inner:lambda? body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) #t]
    [((? inner:trivial? head) '<- (? inner:pair? tail)) #t]
    [((? inner:trivial? head) '<- (? index-node? tail)) #t]
    [else #f]))

(define (inner:lambda-param body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) tail]
    [((? inner:trivial? head) '<- (? index-node? tail)) tail]
    [else '()]))

(define (inner:lambda-return body)
  (match body
    [((? inner:trivial? head) '<- (? inner:list? tail)) head]
    [((? inner:trivial? head) '<- (? index-node? tail)) head]
    [else '()]))

;rewrites an inner:list?-headed type with at least two items into a
;right-nested inner:pair? chain (the established "list as pair chain"
;candy, pinned by test-inner-type-checker.sls). Explicit predicates
;replaced the old ufo-match pattern, which fired on ANY >=3-element
;list and dropped its head -- mangling arrows (their return position;
;ARROWS ARE EXEMPT below) as well as (inner:pair? ...) types, (with
;...) macros, and any other 3-tuple that reached type:->?'s list
;branch. Those shapes now pass through untouched.
(define (inner:?->pair target)
  (if (or (inner:lambda? target)
          (not (and (list? target)
                  (>= (length target) 3)
                  (eq? (car target) 'inner:list?))))
    target
    `(inner:pair? ,(cadr target)
        ,(inner:?->pair `(inner:list? ,@(cddr target))))))

(define (inner:list? body)
  (match body
    [('inner:list? item ...) 
      (fold-left 
        (lambda (left right)
          (and left (inner:trivial? right)))
        #t
        (filter 
          (lambda (t) (and (not (equal? t '...)) (not (equal? t '**1)))) 
          item))]
    [else #f]))

(define (inner:list-content body)
  (match body
    [('inner:list? item ...) item]
    [else '()]))

(define (inner:vector? body)
  (match body
    [('inner:vector? item ...) 
      (fold-left 
        (lambda (left right)
          (and left (inner:trivial? right)))
        #t
        (filter 
          (lambda (t) (and (not (equal? t '...)) (not (equal? t '**1)))) 
          item))]
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