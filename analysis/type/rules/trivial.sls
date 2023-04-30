(library (scheme-langserver analysis type rules trivial)
  (export trivial-process)
  (import 
    (chezscheme) 

    (scheme-langserver util dedupe)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define trivial-process 
  (case-lambda 
    [(document index-node substitutions) 
      (let* ([ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)]
          [variable (index-node-variable index-node)])
        (fold-left 
          add-to-substitutions 
          substitutions
          (if (null? (index-node-children index-node))
            (trivial-process document index-node variable expression substitutions #f #t)
            '())))]
    [(document index-node variable expression substitutions allow-unquote? unquoted?)
      (cond
        ;These clauses won't be affected by quote
        [(char? expression) (list `(,variable : ,(construct-type-expression-with-meta 'char?)))]
        [(string? expression) (list `(,variable : ,(construct-type-expression-with-meta 'string?)))]
        [(boolean? expression) (list `(,variable : ,(construct-type-expression-with-meta 'boolean?)))]
        [(fixnum? expression) (list `(,variable : ,(construct-type-expression-with-meta 'fixnum?)))]
        [(bignum? expression) (list `(,variable : ,(construct-type-expression-with-meta 'bignum?)))]
        [(integer? expression) (list `(,variable : ,(construct-type-expression-with-meta 'integer?)))]
        [(cflonum? expression) (list `(,variable : ,(construct-type-expression-with-meta 'cflonum?)))]
        [(flonum? expression) (list `(,variable : ,(construct-type-expression-with-meta 'flonum?)))]
        [(rational? expression) (list `(,variable : ,(construct-type-expression-with-meta 'rational?)))]
        [(real? expression) (list `(,variable : ,(construct-type-expression-with-meta 'real?)))]
        [(complex? expression) (list `(,variable : ,(construct-type-expression-with-meta 'complex?)))]
        [(number? expression) (list `(,variable : ,(construct-type-expression-with-meta 'number?)))]

        [(and (symbol? expression) unquoted?)
          (sort substitution-compare
            (apply 
              append 
              (map 
                (lambda (identifier-reference) 
                  (private-process document identifier-reference variable))
                (find-available-references-for document index-node expression))))]
        [(symbol? expression) (list `(,variable : ,(construct-type-expression-with-meta 'symbol?)))]

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
                    '(()())
                  (if is-list? expression (vector->list expression)))]
              [variable-list (if is-list? (car final-result) (list->vector (car final-result)))]
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

(define (private-process document identifier-reference variable)
  (sort substitution-compare
    (if (null? (identifier-reference-parents identifier-reference))
      (let* ([target-document (identifier-reference-document identifier-reference)]
          [target-index-node (identifier-reference-index-node identifier-reference)])
        (cond 
          ;it's in r6rs library?
          [(null? target-index-node) 
            (map 
              (lambda (expression)
                `(,variable : ,expression))
              (identifier-reference-type-expressions identifier-reference))]
          ;local
          [(equal? document target-document)
            (list `(,variable = ,(index-node-variable target-index-node)))]
          ;import
          [else 
            (map 
              (lambda (reified) `(,variable : ,reified))
              (dedupe 
                (filter 
                  is-pure-identifier-reference-misture? 
                  (reify 
                    (document-substitution-list target-document) 
                    (index-node-variable target-index-node)))))]))
      (apply 
        append 
        (map 
          (lambda (parent) (private-process document parent variable))
          (identifier-reference-parents identifier-reference))))))
)
