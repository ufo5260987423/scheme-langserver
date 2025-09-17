(library (scheme-langserver analysis identifier macro-expander)
  (export 
    expand:step-by-step
    expand:step-by-step-identifier-reference
    generate-pair:template+callee
    generate-pair:template+expanded
    generate-pair:callee+expanded)
  (import 
    (chezscheme)
    (ufo-match)

    (ufo-try)
    (scheme-langserver util path)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis local-expand)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (expand:step-by-step-identifier-reference identifier-reference callee-index-node callee-document)
  (map 
    (lambda (p)
      (let* ([target-index-node (cdr p)]
          [identifier-reference (car p)])
        `(,identifier-reference . ,(private:dispatch (cdr p) callee-index-node callee-document))))
    (private:unwrap identifier-reference)))

(define (expand:step-by-step identifier-reference callee-index-node callee-document)
  (try 
    (map cdr (expand:step-by-step-identifier-reference identifier-reference callee-index-node callee-document))
    (except c 
      [(condition? c)
        (if (not (equal? "invalid syntax" (condition-message c))) (display-condition c))
        '()]
    [else '()])))

(define generate-pair:callee+expanded
  (case-lambda 
    [(identifier-reference callee-index-node callee-document)
      (generate-pair:callee+expanded (expand:step-by-step identifier-reference callee-index-node callee-document) identifier-reference callee-index-node callee-document)]
    [(expanded-expression-list identifier-reference callee-index-node callee-document)
      (let ([template+callees (generate-pair:template+callee identifier-reference callee-index-node callee-document)])
        (map 
          (lambda (expanded-expression)
            (generate-pair:callee+expanded template+callees 
              (init-index-node 
                (identifier-reference-initialization-index-node identifier-reference) 
                (car 
                  (source-file->annotations 
                    (with-output-to-string (lambda () (pretty-print expanded-expression)))
                    (uri->path (document-uri (identifier-reference-document identifier-reference))))))
              identifier-reference callee-index-node callee-document))
          expanded-expression-list))]
    [(template+callees expanded-index-node identifier-reference callee-index-node callee-document)
      (let ([template+expandeds (generate-pair:template+expanded identifier-reference expanded-index-node callee-index-node callee-document template+callees)])
        (apply append 
          (map 
            (lambda (template+callee)
              (let* ([template (car template+callee)]
                  [callee (cdr template+callee)]
                  [pre (assoc template template+expandeds)]
                  [after (if pre pre (assoc `(,template ...) template+expandeds))]
                  [target-expandeds (if after (cdr after) '())])
                (cond 
                  [(and (index-node? callee) (index-node? target-expandeds)) 
                  `((,callee ,target-expandeds))]
                  [(and (index-node? callee) (find list? target-expandeds)) 
                  `((,callee . ,(apply append target-expandeds)))]
                  [(index-node? callee) 
                  `((,callee . ,target-expandeds))]
                  [(find list? target-expandeds) 
                    (fold-left 
                      (lambda (left right)
                        (fold-left 
                          (lambda (current-left . rights) 
                            (if (list? (car rights))
                              `(,@current-left ,(append (car rights) (cdr rights)))
                              `(,@current-left ,rights)))
                          '()
                          left
                          right))
                      callee
                      target-expandeds)]
                  [else 
                    (let ([m (min (length callee) (length target-expandeds))])
                      (fold-left 
                        (lambda (left . rights) `(,@left ,rights))
                        '()
                        (list-head callee m)
                        (list-head target-expandeds m)))])))
            template+callees)))]))

(define (generate-pair:template+expanded identifier-reference expanded-index-node callee-index-node callee-document template+callees)
  (fold-left 
    (lambda (left maybe-pair)
      (let* ([head (car maybe-pair)]
          [tail (cdr maybe-pair)]
          [prev-pair (find (lambda (l) (equal? head (car l))) left)]
          [head... `(,head ...)]
          [prev-pair... (find (lambda (l) (equal? head... (car l))) left)])
        (cond 
          [prev-pair...
            (append 
              (filter (lambda (p) (not (equal? head... (car p)))) left)
              `((,head... . (,@(cdr prev-pair...) ,tail))))]
          [prev-pair 
            (append 
              (filter (lambda (p) (not (equal? head (car p)))) left)
              `((,head... . (,(cdr prev-pair) ,tail))))]
          [else 
          (append left (list maybe-pair))])))
    '()
    (apply append 
      (map 
        (lambda (p)
          (private:dispatch-for-expanded-pair (cdr p) callee-index-node callee-document expanded-index-node template+callees))
        (private:unwrap identifier-reference)))))

(define (generate-pair:template+callee identifier-reference callee-index-node callee-document)
  (fold-left 
    (lambda (left maybe-pair)
      (let* ([head (car maybe-pair)]
          [tail (cdr maybe-pair)]
          [prev-pair (find (lambda (l) (equal? head (car l))) left)]
          [head... `(,head ...)]
          [prev-pair... (find (lambda (l) (equal? head... (car l))) left)])
        (cond 
          [prev-pair...
            (append 
              (filter (lambda (p) (not (equal? head... (car p)))) left)
              `((,head... . (,@(cdr prev-pair...) ,tail))))]
          [prev-pair 
            (append 
              (filter (lambda (p) (not (equal? head (car p)))) left)
              `((,head... . (,(cdr prev-pair) ,tail))))]
          [else (append left (list maybe-pair))])
          ))
    '()
    (apply append 
      (map 
        (lambda (p)
          (try 
            (private:dispatch-for-callee-pair (cdr p) callee-index-node callee-document)
            (except c 
              [(condition? c)
                (if (not (equal? "invalid syntax" (condition-message c))) (display-condition c))
                '()]
              [else '()])))
        (private:unwrap identifier-reference)))))

(define (private:dispatch-for-expanded-pair index-node callee-index-node callee-document expanded-index-node template+callees)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)]
      [first-child (car children)]
      [callee-expression (annotation-stripped (index-node-datum/annotations callee-index-node))]
      [expanded-expression (annotation-stripped (index-node-datum/annotations expanded-index-node))]
      [callee-index-nodes (index-node-children callee-index-node)])
    (match expression
      [('syntax-rules (keywords ...) clauses **1) 
        (if (root-meta-check callee-document first-child 'syntax-rules)
          (let* ([clause-index-nodes (cddr children)]
              [template (eval `(syntax-case ',callee-expression ,keywords ,@(map private:get-specific-template clauses)))]
              [body-index-node (private:get-specific-body-index-node clause-index-nodes template)]
              [template-index-node (car (index-node-children (index-node-parent body-index-node)))]
              [body-expression (annotation-stripped (index-node-datum/annotations body-index-node))])
            (private:template-variable+expanded template-index-node body-index-node body-expression expanded-index-node template+callees callee-document #f))
          '())]
      [('lambda ((? symbol? parameter)) _ ... ('syntax-case like-parameter (keywords ...) clauses **1))
        (if (and 
            (equal? parameter like-parameter)
            (root-meta-check callee-document first-child 'lambda)
            (root-meta-check callee-document (car (index-node-children (car (reverse children)))) 'syntax-case))
          (let* ([clause-index-nodes (cdddr (index-node-children (car (reverse children))))]
              [template (eval `(syntax-case ',callee-expression ,keywords ,@(map private:get-specific-template clauses)))]
              [body-index-node (private:get-specific-body-index-node clause-index-nodes template)]
              [template-index-node (car (index-node-children (index-node-parent body-index-node)))]
              [body-expression (annotation-stripped (index-node-datum/annotations body-index-node))]
              [body-index-node-children (index-node-children body-index-node)])
            (if (or 
                (equal? (car body-expression) 'quasisyntax)
                (equal? (car body-expression) 'syntax))
              (private:template-variable+expanded template-index-node (car body-index-node-children) (cadr body-expression) expanded-index-node template+callees callee-document #f)
              (private:template-variable+expanded template-index-node body-index-node body-expression expanded-index-node template+callees callee-document #f)))
          '())]
      [else '()])))

(define (private:dispatch-for-callee-pair index-node callee-index-node callee-document)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)]
      [first-child (car children)]
      [callee-expression (annotation-stripped (index-node-datum/annotations callee-index-node))]
      [callee-index-nodes (index-node-children callee-index-node)])
    (match expression
      [('syntax-rules (keywords ...) clauses **1) 
        (if (root-meta-check callee-document first-child 'syntax-rules)
          (let* ([template (eval `(syntax-case ',callee-expression ,keywords ,@(map private:get-specific-template clauses)))])
            (private:template-variable+callee template callee-index-nodes callee-expression keywords))
          '())]
      [('lambda ((? symbol? parameter)) _ ... ('syntax-case like-parameter (keywords ...) clauses **1))
        (if (and 
            (equal? parameter like-parameter)
            (root-meta-check callee-document first-child 'lambda)
            (root-meta-check callee-document (car (index-node-children (car (reverse children)))) 'syntax-case))
          (let* ([template (eval `(syntax-case ',callee-expression ,keywords ,@(map private:get-specific-template clauses)))])
            (private:template-variable+callee template callee-index-nodes callee-expression keywords))
          '())]
      [else '()])))

(define (private:get-specific-template clause) 
  (match clause
    [(template body ...) `(,template ',template)]
    [else '()]))

(define (private:get-specific-body clause) 
  (match clause
    [(template body ...) `(,template ',body)]
    [else '()]))

(define (private:get-specific-body-index-node clause-index-nodes target-template) 
  (fold-left
    (lambda (stop? clause-index-node)
      (if (not stop?)
        (let* ([clause (annotation-stripped (index-node-datum/annotations clause-index-node))]
            [template (car clause)]
            [children (index-node-children clause-index-node)])
          (if (equal? template target-template)
            (car (reverse children))
            stop?))
        stop?))
    #f
    clause-index-nodes))

(define (private:template-variable+expanded template-index-node body-index-node body-expression expanded-index-node template+callees document is-...?)
  (cond 
    [(equal? body-expression '...) '()]
    [(and (private:syntax-parameter-index-node? template-index-node body-index-node document body-expression) is-...? (assoc body-expression template+callees))
      `((,body-expression . ,expanded-index-node))]
    [(and (private:syntax-parameter-index-node? template-index-node body-index-node document body-expression) is-...? (assoc `(,body-expression ...) template+callees))
      (let ([v (list->vector (cdr (assoc `(,body-expression ...) template+callees)))])
        (if (> (vector-length v) is-...?)
          `(((,body-expression ...) . ,expanded-index-node))
          '()))]
    ; [(and (private:syntax-parameter-index-node? body-index-node document body-expression) is-...?) (raise 'IdontKnowWhy)]
    [(and (private:syntax-parameter-index-node? template-index-node body-index-node document body-expression) (assoc body-expression template+callees))
      `((,body-expression . ,expanded-index-node))]
    [(private:syntax-parameter-index-node? template-index-node body-index-node document body-expression) '()]
    [(not (pair? body-expression)) '()]

    [(or (null? body-expression) (null? body-index-node) (null? expanded-index-node)) '()]
    [(and (pair? body-expression) (index-node? body-index-node)) 
      (if (or
          (equal? (car body-expression) 'quasisyntax)
          (equal? (car body-expression) 'syntax)
          (equal? (car body-expression) 'quasiquote)
          (equal? (car body-expression) 'unquote)
          (equal? (car body-expression) 'unquote-splicing)
          (equal? (car body-expression) 'unsyntax)
          (equal? (car body-expression) 'unsyntax-splicing))
        '()
        (private:template-variable+expanded template-index-node (index-node-children body-index-node) body-expression (index-node-children expanded-index-node) template+callees document is-...?))]
    [(vector? body-expression) 
      (private:template-variable+expanded template-index-node body-index-node (vector->list body-expression) expanded-index-node template+callees document is-...?)]
    [(not (list? body-expression)) 
      (private:template-variable+expanded template-index-node body-index-node `(,(car body-expression) ,(cdr body-expression)) expanded-index-node template+callees document is-...?)]

    [(>= (length body-expression) 2)
      (if (equal? (cadr body-expression) '...)
        (if is-...?
          (raise 'IdontKnowWhy)
          (let loop ([auto-increase 0]
              [rest-expanded-index-node expanded-index-node])
            (if (null? rest-expanded-index-node)
              '()
              (let ([what-i-got
                    (private:template-variable+expanded template-index-node (car body-index-node) (car body-expression) (car rest-expanded-index-node) template+callees document auto-increase)])
                (if (null? what-i-got)
                  (private:template-variable+expanded template-index-node (cddr body-index-node) (cddr body-expression) rest-expanded-index-node template+callees document #f)
                  (append what-i-got (loop (+ 1 auto-increase) (cdr rest-expanded-index-node))))))))
        (append 
          (private:template-variable+expanded template-index-node (car body-index-node) (car body-expression) (car expanded-index-node) template+callees document is-...?)
          (private:template-variable+expanded template-index-node (cdr body-index-node) (cdr body-expression) (cdr expanded-index-node) template+callees document is-...?)))]
    [(list? body-expression) 
      (private:template-variable+expanded template-index-node (car body-index-node) (car body-expression) (car expanded-index-node) template+callees document is-...?)]))

(define (private:syntax-parameter-index-node? template-index-node index-node document expression)
  (if (and (symbol? expression) (index-node? index-node))
    (find 
      (lambda (x) 
        (and 
          (equal? 'syntax-parameter (identifier-reference-type x))
          (equal? template-index-node (identifier-reference-index-node x))))
      (find-available-references-for document index-node expression))
    #f))

(define (private:template-variable+callee template callee-index-node callee-expression keywords)
  (cond 
    [(symbol? template) `((,template . ,callee-index-node))]
    [(and (not (symbol? template)) (index-node? callee-index-node))
      (private:template-variable+callee template (index-node-children callee-index-node) callee-expression keywords)]
    [(vector? template) 
      (private:template-variable+callee (vector->list template) (index-node-children callee-index-node) (vector->list callee-expression) keywords)]
    ; [(or (null? template) (null? callee-index-node) (null? callee-expression)) '()]
    [(or (null? template) (null? callee-index-node)) '()]
    ;pair
    [(not (list? template)) 
      (private:template-variable+callee `(,(car template) ,(cdr template)) callee-index-node `(,(car callee-expression) ,(cdr callee-expression)) keywords)]
    [(>= (length template) 2)
      (if (equal? (cadr template) '...)
        (let* ([new-callee-expression (syntax->datum (eval `(syntax-case ',callee-expression ,keywords (,template #'(,(car template) ,(cadr template))))))]
            [size (length new-callee-expression)])
          (append 
            (apply append 
              (map 
                (lambda (l)
                  (private:template-variable+callee 
                    (car template) 
                    l
                    (annotation-stripped (index-node-datum/annotations l))
                    keywords))
                (list-head callee-index-node size)))
            (private:template-variable+callee 
              (cddr template)
              (list-tail callee-index-node size)
              (list-tail callee-expression size)
              keywords)))
        (append 
          (private:template-variable+callee (car template) (car callee-index-node) (car callee-expression) keywords)
          (private:template-variable+callee (cdr template) (cdr callee-index-node) (cdr callee-expression) keywords)))]
      [(list? template)
        (private:template-variable+callee (car template) (car callee-index-node) (car callee-expression) keywords)]))

(define (private:dispatch index-node callee-index-node callee-document)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)]
      [first-child (car children)]
      [callee-expression (annotation-stripped (index-node-datum/annotations callee-index-node))])
    (match expression
      [('syntax-rules (keywords ...) clauses **1) 
        (if (root-meta-check callee-document first-child 'syntax-rules)
          (syntax->datum (eval `(syntax-case ',callee-expression ,keywords ,@(map private:rule-clause->case-clause clauses))))
          '())]
      [('lambda ((? symbol? parameter)) _ ... ('syntax-case like-parameter (keywords ...) clauses **1))
        (if (and 
            (equal? parameter like-parameter)
            (root-meta-check callee-document first-child 'lambda)
            (root-meta-check callee-document (car (index-node-children (car (reverse children)))) 'syntax-case))
          (syntax->datum (eval `(syntax-case ',callee-expression ,keywords ,@clauses)))
          '())]
      [else '()])))

(define (private:rule-clause->case-clause clause)
  (match clause
    [(template body) `(,template #',body)]
    [else '()]))

(define (private:unwrap identifier-reference)
  (let* ([initial (identifier-reference-initialization-index-node identifier-reference)]
      [initial-expression (annotation-stripped (index-node-datum/annotations initial))]
      [initial-children (index-node-children initial)]
      [first-child (car initial-children)]
      [first-child-expression (annotation-stripped (index-node-datum/annotations first-child))]
      [document (identifier-reference-document identifier-reference)]
      [first-child-identifiers (find-available-references-for document initial first-child-expression)]
      [first-child-top-identifiers (apply append (map root-ancestor first-child-identifiers))])
    (map 
      (lambda (first-child-identifier)
        (case (identifier-reference-identifier first-child-identifier)
          ['define-syntax `(,first-child-identifier . ,(caddr initial-children))]
          ['let-syntax `(,first-child-identifier . ,(index-node-parent (identifier-reference-index-node identifier-reference)))]))
      (filter meta? first-child-top-identifiers))))
)
