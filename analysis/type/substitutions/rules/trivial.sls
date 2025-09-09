(library (scheme-langserver analysis type substitutions rules trivial)
  (export 
    trivial-process
    generate-symbols-with 
    index-of)
  (import 
    (chezscheme) 

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)
    (ufo-try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions rules record)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define private-char? (construct-type-expression-with-meta 'char?))
(define private-string? (construct-type-expression-with-meta 'string?))
(define private-boolean? (construct-type-expression-with-meta 'boolean?))
(define private-fixnum? (construct-type-expression-with-meta 'fixnum?))
(define private-bignum? (construct-type-expression-with-meta 'bignum?))
(define private-integer? (construct-type-expression-with-meta 'integer?))
(define private-cflonum? (construct-type-expression-with-meta 'cflonum?))
(define private-flonum? (construct-type-expression-with-meta 'flonum?))
(define private-rational? (construct-type-expression-with-meta 'rational?))
(define private-real? (construct-type-expression-with-meta 'real?))
(define private-complex? (construct-type-expression-with-meta 'complex?))
(define private-number? (construct-type-expression-with-meta 'number?))
(define private-symbol? (construct-type-expression-with-meta 'symbol?))

(define trivial-process 
  (case-lambda 
    [(document index-node) 
      (let* ([ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)])
        (if (null? (index-node-children index-node))
          (trivial-process document index-node expression #f #f)
          '()))]
    [(document index-node expression allow-unquote? quoted?)
      (cond
        ;These clauses won't be affected by quote
        [(char? expression) (extend-index-node-substitution-list index-node private-char?)]
        [(string? expression) (extend-index-node-substitution-list index-node private-string?)]
        [(boolean? expression) (extend-index-node-substitution-list index-node private-boolean?)]
        [(fixnum? expression) (extend-index-node-substitution-list index-node private-fixnum?)]
        [(bignum? expression) (extend-index-node-substitution-list index-node private-bignum?)]
        [(integer? expression) (extend-index-node-substitution-list index-node private-integer?)]
        [(cflonum? expression) (extend-index-node-substitution-list index-node private-cflonum?)]
        [(flonum? expression) (extend-index-node-substitution-list index-node private-flonum?)]
        [(rational? expression) (extend-index-node-substitution-list index-node private-rational?)]
        [(real? expression) (extend-index-node-substitution-list index-node private-real?)]
        [(complex? expression) (extend-index-node-substitution-list index-node private-complex?)]
        [(number? expression) (extend-index-node-substitution-list index-node private-number?)]

        [(and (symbol? expression) (not quoted?))
          (map 
            (lambda (identifier-reference) 
              (private-process document identifier-reference index-node))
            (find-available-references-for document index-node expression))]
        [(symbol? expression) (extend-index-node-substitution-list index-node private-symbol?)]

        [(and (pair? expression) (not (list? expression)))
          (let* ([f (car expression)]
              [l (cdr expression)]
              [new-index-node-f (make-virtual-index-node index-node)]
              [new-index-node-l (make-virtual-index-node index-node)])
            (extend-index-node-substitution-list
              index-node
              `(inner:pair? ,new-index-node-f ,new-index-node-l))
            (trivial-process document new-index-node-f f allow-unquote? quoted?)
            (trivial-process document new-index-node-l l allow-unquote? quoted?))]
        [(or (list? expression) (vector? expression))
          (let* ([is-list? (list? expression)]
              [middle-result
                (map
                  (lambda (current-expression)
                    (if (and (private-unquote-splicing? index-node document current-expression) allow-unquote? quoted?)
                      (let* ([v (make-index-node index-node '() '() '() '() '() '() '())])
                        (trivial-process document v current-expression #f #t)
                        v)))
                  ; (if is-list? '(inner:list?) '(inner:vector?))
                  (if is-list? expression (vector->list expression)))]
              [final-result `(,(if is-list? 'inner:list? 'inner:vector?) ,@middle-result)])
            (extend-index-node-substitution-list index-node final-result))]
        [else '()])]))

(define (private-process document identifier-reference index-node)
  (if (null? (identifier-reference-parents identifier-reference))
    (let* ([target-document (identifier-reference-document identifier-reference)]
        [target-index-node (identifier-reference-index-node identifier-reference)]
        [type-expressions (identifier-reference-type-expressions identifier-reference)]
        [type (identifier-reference-type identifier-reference)])
      (cond 
        [(and (contain? '(constructor getter setter predicator) type) (not (null? target-index-node)))
          (extend-index-node-substitution-list index-node identifier-reference)]
        ;it's in r6rs library?
        [(null? target-index-node)
          (map 
            (lambda (t) (extend-index-node-substitution-list index-node t))
            type-expressions)]
        ; this can't be excluded by identifier-catching rules
        [(equal? index-node target-index-node) '()]
        ;local
        ;You can't cache and speed up this clause by distinguishing variable in/not in 
        ;identifier-reference-initialization-index-node scope, because reify depdends on 
        ;implicit conversion and there may be several nested variable initializations for
        ;which we can't cleanly decide when to do the imlicit conversion.
        [(equal? document target-document)
          (extend-index-node-substitution-list index-node target-index-node)
            ; `((,variable = ,(index-node-variable target-index-node)))
            ;implicit conversion for gradual typing
          (cond 
            [(null? (index-node-parent index-node)) '()]
            [(and 
                (is-ancestor? (identifier-reference-initialization-index-node identifier-reference) index-node) 
                (is-first-child? index-node) 
                (equal? 'parameter (identifier-reference-type identifier-reference)))
              (let* ([ancestor (index-node-parent index-node)]
                  [children (index-node-children ancestor)]
                  [rests (cdr children)])
                (extend-index-node-substitution-list target-index-node `(,ancestor <- (inner:list? ,@rests))))]
            [(and 
                (is-ancestor? (identifier-reference-initialization-index-node identifier-reference) index-node) 
                (equal? 'parameter (identifier-reference-type identifier-reference)))
              (let* ([ancestor (index-node-parent index-node)]
                  [children (index-node-children ancestor)]
                  [head (car children)]
                  [rests (cdr children)]
                  [index (index-of (list->vector rests) index-node)]
                  [symbols (generate-symbols-with "d" (length rests))])
                (if (= index (length rests))
                  '()
                  (extend-index-node-substitution-list
                    target-index-node
                    `((with ((a b c)) 
                      ((with ((x ,@symbols))
                        ,(vector-ref (list->vector symbols) index))
                        c)) 
                      ,head))))]
            [else '()])]
        ;import
        [else 
          (if (null? type-expressions)
            (extend-index-node-substitution-list
              index-node
              (identifier-reference-index-node identifier-reference))
            (map 
              (lambda (t) (extend-index-node-substitution-list index-node t))
              type-expressions))]))
    (map 
      (lambda (parent) (private-process document parent index-node))
      (identifier-reference-parents identifier-reference))))

(define (generate-symbols-with base-string max)
  (let loop ([result '()])
    (if (< (length result) max)
      (loop `(,@result ,(string->symbol (string-append base-string (number->string (length result))))))
      result)))

(define (index-of target-vector target-index-node)
  (let loop ([i 0])
    (cond 
      [(= i (vector-length target-vector)) i]
      [(equal? (vector-ref target-vector i) target-index-node) i]
      [else (loop (+ i 1))])))

(define (private-unquote-splicing? index-node document current-expression)
  (if (pair? current-expression)
    (if (equal? unquote-splicing? (car current-expression))
      (try
        (guard-for document index-node 'unquote-splicing '(chezscheme) '(rnrs) '(rnrs base) '(scheme)) 
        #t
      (except c
        [else #f]))
      #f)
    #f))
)
