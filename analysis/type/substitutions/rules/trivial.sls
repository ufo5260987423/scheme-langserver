(library (scheme-langserver analysis type substitutions rules trivial)
  (export 
    trivial-process
    find-return-variable 
    generate-symbols-with 
    index-of)
  (import 
    (chezscheme) 

    (scheme-langserver util dedupe)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language interpreter)

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
    [(document index-node substitutions) 
      (let* ([ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)]
          [variable (index-node-variable index-node)])
        (append
          substitutions
          (if (null? (index-node-children index-node))
            (trivial-process document index-node variable expression substitutions #f #t)
            '())))]
    [(document index-node variable expression substitutions allow-unquote? unquoted?)
      (cond
        ;These clauses won't be affected by quote
        [(char? expression) (list `(,variable : ,private-char?))]
        [(string? expression) (list `(,variable : ,private-string?))]
        [(boolean? expression) (list `(,variable : ,private-boolean?))]
        [(fixnum? expression) (list `(,variable : ,private-fixnum?))]
        [(bignum? expression) (list `(,variable : ,private-bignum?))]
        [(integer? expression) (list `(,variable : ,private-integer?))]
        [(cflonum? expression) (list `(,variable : ,private-cflonum?))]
        [(flonum? expression) (list `(,variable : ,private-flonum?))]
        [(rational? expression) (list `(,variable : ,private-rational?))]
        [(real? expression) (list `(,variable : ,private-real?))]
        [(complex? expression) (list `(,variable : ,private-complex?))]
        [(number? expression) (list `(,variable : ,private-number?))]

        [(and (symbol? expression) unquoted?)
          (sort substitution-compare
            (apply 
              append 
              (map 
                (lambda (identifier-reference) 
                  (private-process document identifier-reference index-node variable))
                (find-available-references-for document index-node expression))))]
        [(symbol? expression) (list `(,variable : ,private-symbol?))]

        ;here, must be a list or vector
        [(and (private-quasiquote? expression) unquoted?)
          (trivial-process 
            document 
            index-node 
            variable 
            (cadr expression) 
            substitutions 
            #t 
            #f)]
        [(private-quote? expression) 
          (trivial-process 
            document 
            index-node 
            variable 
            (cadr expression) 
            substitutions 
            #f 
            #f)]
        [(and (private-unquote? expression) allow-unquote? (not unquoted?))
          (trivial-process 
            document 
            index-node 
            variable 
            (cadr expression) 
            substitutions 
            #f 
            #t)]
        [(and (private-unquote-slicing? expression) (or (not allow-unquote?) unquoted?)) '()]

        [(or (list? expression) (vector? expression))
          (let* ([is-list? (list? expression)]
              [final-result
                (fold-left 
                  (lambda (ahead-result current-expression)
                    (if (and (private-unquote-slicing? current-expression) allow-unquote? (not unquoted?))
                      (let loop ([body (cdr current-expression)]
                          [current-result ahead-result])
                        (if (null? body)
                          current-result
                          (let* ([current-item (car body)]
                              [v (make-variable)]
                              [r (trivial-process document index-node v current-item substitutions #f #t)]
                              [first `(,@(car current-result) ,v)]
                              [last `(,@(cadr current-result) ,@r)])
                            (loop (cdr body) `(,first ,last)))))
                      (let* ([v (make-variable)]
                          [r (trivial-process document index-node v current-expression substitutions allow-unquote? unquoted?)]
                          [first `(,@(car ahead-result) ,v)]
                          [last `(,@(cadr ahead-result) ,@r)])
                        `(,first ,last))))
                  `((,(if is-list? 'inner:list? 'inner:vector?))())
                  (if is-list? expression (vector->list expression)))]
              [variable-list (car final-result)]
              [extend-substitution-list (cadr final-result)])
            (sort substitution-compare `(,@extend-substitution-list (,variable = ,variable-list))))]
        [else '()])]))

(define (private-unquote-slicing? expression)
  (if (list? expression)
    (if (= 1 (length expression))
      (equal? 'quasiquote-slicing (car expression))
      #f)
    #f))

(define (private-unquote? expression)
  (if (list? expression)
    (if (= 1 (length expression))
      (equal? 'quasiquote (car expression))
      #f)
    #f))

(define (private-quote? expression)
  (if (list? expression)
    (if (= 1 (length expression))
      (equal? 'quote (car expression))
      #f)
    #f))

(define (private-quasiquote? expression)
  (if (list? expression)
    (if (= 1 (length expression))
      (equal? 'quasiquote (car expression))
      #f)
    #f))

(define (private-process document identifier-reference index-node variable)
  (sort substitution-compare
    (if (null? (identifier-reference-parents identifier-reference))
      (let* ([target-document (identifier-reference-document identifier-reference)]
          [target-index-node (identifier-reference-index-node identifier-reference)]
          [type-expressions (identifier-reference-type-expressions identifier-reference)])
        (cond 
          ;it's in r6rs library?
          [(null? target-index-node)
            (if (null? type-expressions) '() (cartesian-product `(,variable) '(:) type-expressions))]
          ; this can't be excluded by identifier-catching rules
          [(equal? index-node target-index-node) '()]
          ;local
          ;You can't cache and speed up this clause by distinguishing variable in/not in 
          ;identifier-reference-initialization-index-node scope, because reify depdends on 
          ;implicit conversion and there may be several nested variable initializations for
          ;which we can't cleanly decide when to do the imlicit conversion.
          [(equal? document target-document)
            (append
              `((,variable = ,(index-node-variable target-index-node)))
              ;implicit conversion for gradual typing
              (cond 
                [(null? (index-node-parent index-node)) '()]
                [(and 
                    (is-ancestor? (identifier-reference-initialization-index-node identifier-reference) index-node) 
                    (is-first-child? index-node) 
                    (or (equal? 'parameter (identifier-reference-type identifier-reference))
                      (equal? 'syntax-parameter (identifier-reference-type identifier-reference))))
                  (let* ([ancestor (index-node-parent index-node)]
                      [children (index-node-children ancestor)]
                      [rests (cdr children)]
                      [rest-variables (map index-node-variable rests)]
                      [return (find-return-variable index-node (identifier-reference-initialization-index-node identifier-reference))])
                    (if (null? return)
                      '()
                      `((,variable = (,return <- (inner:list? ,@rest-variables))))))]
                [(and 
                    (is-ancestor? (identifier-reference-initialization-index-node identifier-reference) index-node) 
                    (or (equal? 'parameter (identifier-reference-type identifier-reference))
                      (equal? 'syntax-parameter (identifier-reference-type identifier-reference))))
                  (let* ([ancestor (index-node-parent index-node)]
                      [children (index-node-children ancestor)]
                      [target-variable (index-node-variable target-index-node)]
                      [head (car children)]
                      [head-variable (index-node-variable head)]
                      [rests (cdr children)]
                      [rest-variables (map index-node-variable rests)]
                      [index (index-of (list->vector rests) index-node)]
                      [symbols (generate-symbols-with "d" (length rest-variables))])
                    (if (= index (length rests))
                      '()
                      `((,target-variable 
                          = 
                          ((with ((a b c)) 
                            ((with ((x ,@symbols))
                              ,(vector-ref (list->vector symbols) index))
                              c)) 
                            ,head-variable)))))]
                [else '()]))]
          ;import
          [else 
            ; (pretty-print 'import)
            ; (print-graph #t)
            ; (pretty-print (document-uri (identifier-reference-document identifier-reference)))
            (let ([run 
                  (lambda ()
                    (if (null? type-expressions)
                      (identifier-reference-type-expressions-set! 
                        identifier-reference 
                        (dedupe 
                          (type:interpret-result-list 
                            (index-node-variable target-index-node)
                            (make-type:environment (document-substitution-list target-document)))))))])
              (if (null? (document-mutex target-document))
                (run)
                (with-mutex (document-mutex target-document) (run))))
            (cartesian-product `(,variable) '(:) (identifier-reference-type-expressions identifier-reference))]))
      (apply 
        append 
        (map 
          (lambda (parent) (private-process document parent index-node variable))
          (identifier-reference-parents identifier-reference))))))

(define (find-return-variable target-index-node define-index-node)
  (cond 
    [(not (is-ancestor? define-index-node target-index-node)) '()]
    [(and (is-first-child? target-index-node) (null? (index-node-parent target-index-node))) '()]
    [(is-first-child? target-index-node) (find-return-variable (index-node-parent target-index-node) define-index-node)]
    [else 
      (let* ([ancestor (index-node-parent target-index-node)]
          [children (index-node-children ancestor)]
          [target-variable (index-node-variable target-index-node)]
          [head (car children)]
          [head-variable (index-node-variable head)]
          [rests (cdr children)]
          [rest-variables (map index-node-variable rests)]
          [index (index-of (list->vector rests) target-index-node)]
          [symbols (generate-symbols-with "d" (length rest-variables))])
        (if (= index (length rests))
          '()
          `((with ((a b c)) 
              ((with ((x ,@symbols))
                ,(vector-ref (list->vector symbols) index))
                c)) 
            ,head-variable)))]))

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
)
