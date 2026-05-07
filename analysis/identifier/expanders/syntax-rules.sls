(library (scheme-langserver analysis identifier expanders syntax-rules)
  (export 
    syntax-rules->generator:map+expansion)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver analysis identifier expanders pattern)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis identifier reference))

;; ---- Signature extraction for fast clause filtering ----

(define (private:param-shape param)
  (cond
    [(null? param) 'null]
    [(symbol? param) 'symbol]
    [(pair? param) 'pair]
    [(vector? param) 'vector]
    [else 'other]))

(define (private:fixed-prefix params)
  (cond
    [(null? params) '()]
    [(and (not (null? (cdr params))) (eq? '... (cadr params)))
      (private:fixed-prefix (cddr params))]
    [else (cons (car params) (private:fixed-prefix (cdr params)))]))

(define (private:extract-signature pattern-expr)
  (let ([params (cdr pattern-expr)])
    (if (list? params)
      (let ([fixed (private:fixed-prefix params)])
        (cons (length fixed) (map private:param-shape fixed)))
      (let ([prefix 
              (let loop ([p params])
                (if (pair? p) (cons (car p) (loop (cdr p))) '()))])
        (cons (length prefix) (map private:param-shape prefix))))))

(define (private:shape-match? shape param)
  (case shape
    [(symbol) #t]
    [(null) (null? param)]
    [(pair) (or (pair? param) (null? param))]
    [(vector) (vector? param)]
    [else #t]))

(define (private:params-shapes-match? shapes params)
  (if (or (null? shapes) (null? params))
    (and (null? shapes) (null? params))
    (and (private:shape-match? (car shapes) (car params))
         (private:params-shapes-match? (cdr shapes) (cdr params)))))

(define (private:signature-match? signature input-expr)
  (let ([input-params (cdr input-expr)]
      [min-len (car signature)]
      [shapes (cdr signature)])
    (and (list? input-params)
         (>= (length input-params) min-len)
         (private:params-shapes-match? shapes 
           (let loop ([lst input-params] [n (length shapes)])
             (if (or (zero? n) (null? lst)) '() 
               (cons (car lst) (loop (cdr lst) (- n 1)))))))))

;; ---- Main generator factory ----

;input-index-node is supposed have the form of `(syntax-rules ...)`
(define (syntax-rules->generator:map+expansion root-file-node root-library-node document input-index-node)
  (match (annotation-stripped (index-node-datum/annotations input-index-node))
  ;clause means pattern and template
    [(_ (literals ...) clauses **1) 
      (index-node-expansion-generator-set! input-index-node
        (lambda (local-root-file-node local-root-library-node local-document local-index-node)
          (let* ([local-expression (annotation-stripped (index-node-datum/annotations local-index-node))]
              [clause-index-nodes (cddr (index-node-children input-index-node))]
              [index+expansion (private:confirm-clause literals clause-index-nodes local-expression)])
            (if (or (private:tree-has? local-expression '...) (not index+expansion))
              #f
              (let* ([index (car index+expansion)]
                  [clause-index-node (vector-ref (list->vector clause-index-nodes) index)]
                  [clause-expression (annotation-stripped (index-node-datum/annotations clause-index-node))]
                  [expansion-expression (cdr index+expansion)]

                  [pattern-expression (car clause-expression)]
                  [pattern (make-pattern pattern-expression)]
                  [template-expression (car (reverse clause-expression))]
                  [template-pattern (make-pattern template-expression)]
                  [pattern-context (gather-context pattern)]
                  [pairs (pattern+index-node->pair-list pattern local-index-node)]
                  [bindings (map (lambda (literal) (generate-binding literal ((pattern+context->pairs->iterator literal pattern-context) pairs))) (pattern-exposed-literals template-pattern))]
                  [callee-compound-index-node-list (expand->index-node-compound-list template-pattern bindings pattern-context)]

                  ;todo: expansion and expansion-expression should be emmm, isomophism? This should be checked.
                  [expansion-index-node 
                    (init-index-node 
                      local-index-node
                      (car 
                        (source-file->annotations 
                          (with-output-to-string (lambda () (pretty-print expansion-expression)))
                          (uri->path (document-uri local-document)))))]
                  [matching-pairs (private:expansion+index-node->pairs callee-compound-index-node-list expansion-index-node)])
                `(,matching-pairs . ,expansion-index-node))))))]
    [else #f]))

(define (private:take list n)
  (if (or (zero? n) (null? list))
    '()
    (cons (car list) (private:take (cdr list) (- n 1)))))

(define (private:tree-has? ready target)
  (cond 
    [(equal? ready target) #t]
    [(null? ready) #f]
    [(vector? ready) (private:tree-has? (vector->list ready) target)]
    [(pair? ready) (or (private:tree-has? (car ready) target) (private:tree-has? (cdr ready) target))]
    [else #f]))

(define (compound-list->printable-list compound-list)
  (cond
    [(index-node? compound-list) (annotation-stripped (index-node-datum/annotations compound-list))]
    [(list? compound-list) (map compound-list->printable-list compound-list)]
    [(vector? compound-list) (vector-map compound-list->printable-list compound-list)]
    [(pair? compound-list) `(,(compound-list->printable-list (car compound-list)) . ,(compound-list->printable-list (cdr compound-list)))]
    [else compound-list]))

;these two parameter are supposed to be correct and this procedure won't do fault-tolerant things.
(define (private:expansion+index-node->pairs compound-list index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)])
    (cond 
      [(index-node? compound-list) `((,index-node . ,compound-list))]
      [(list? compound-list) 
        (let ([len-c (length compound-list)]
              [len-ch (length children)])
          (apply append 
            (map 
              (lambda (left right) 
              (private:expansion+index-node->pairs left right))
              (if (> len-c len-ch) (private:take compound-list len-ch) compound-list)
              (if (> len-ch len-c) (private:take children len-c) children))))]
      [(vector? compound-list) 
        (private:expansion+index-node->pairs (vector->list compound-list) index-node)]
      [(pair? compound-list) 
        (private:expansion+index-node->pairs `(,(car compound-list) ,(cdr compound-list)) index-node) ]
      [(and (symbol? compound-list) (symbol? expression))
        `((,index-node . ,compound-list))]
      ;symbol won't get pairs
      [else '()])))

(define (private:confirm-clause literals clause-index-nodes input-expression)
  (let loop ([rest clause-index-nodes] [index 0])
    (if (null? rest) 
      #f
      (let* ([current-clause-index-node (car rest)]
          [current-clause-expression (annotation-stripped (index-node-datum/annotations current-clause-index-node))]
          [pattern-expression (car current-clause-expression)]
          [signature (private:extract-signature pattern-expression)])
        (if (private:signature-match? signature input-expression)
          (let* ([pre-target 
            `(syntax-case ',input-expression ,literals 
              (,(car current-clause-expression) 
                ;result
                #'(,index . ,(car (reverse current-clause-expression))))
              (else #f))]
              [target (syntax->datum (eval pre-target))])
            (if target target (loop (cdr rest) (+ 1 index))))
          (loop (cdr rest) (+ 1 index)))))))
) ; end library
