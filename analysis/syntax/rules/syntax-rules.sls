(library (scheme-langserver analysis syntax rules syntax-rules)
  (export 
    syntax-rules-process)
  (import 
    (chezscheme)
    (ufo-match)
    (ufo-try)

    (scheme-langserver analysis syntax util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver util contain)
    (scheme-langserver util path)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (syntax-rules-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(s ((? symbol? literals) ...) (pattern template) **1) 
          (guard-for document index-node s '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([children (index-node-children index-node)]
              [rest-index-nodes (cddr children)])
            (map 
              (lambda (clause-index-node)
                (clause-process literals clause-index-node document))
              rest-index-nodes))]
        [else '()])
      (except c
        [else '()]))))

(define (clause-process literals clause-index-node document)
  (let* ([children (index-node-children clause-index-node)]
      [pattern-index-node (car children)]
      [pattern-expression (annotation-stripped (index-node-datum/annotations pattern-index-node))]
      [pattern-symbols (pattern->symbols literals pattern-expression)]

      [template-index-node (cadr children)]
      [template-process-result (template-process pattern-index-node template-index-node pattern-symbols document)]
      [export-identifier-list (map index-node-references-export-to-other-node (map car template-process-result))]
      [export*imports-list 
        (map 
          (lambda (identifier)
            ;template-symbol
            `(,(identifier-reference-identifier identifier) 
              . ,(map identifier-reference-identifier
                (map car
                  (filter
                    (lambda (r) (contain? (cdr r) identifier)) 
                    template-process-result)))))
          export-identifier-list)])
    (lambda (callee-document callee-index-node)
      (let* ([local-environment 
            (literals+pattern+callee-index-node/s->pairs:pattern-variable+index-node literals pattern-expression callee-index-node)]
          [new-result (substitute template-index-node local-environment template-process-result)]
          [new-sexp (car new-result)]
          [out (open-output-string)]
          [new-sexp-string 
            (begin
              (pretty-print new-sexp out)
              (get-output-string out))]
          [new-route (cdr new-result)]
          [new-annotations (source-file->annotations new-sexp-string (uri->path (document-uri callee-document)))]
          [new-index-node (init-index-node template-index-node new-annotations)])
        (route+node->index-node-pair:old+new new-route new-index-node)))))

(define (substitute current-index-node local-environment template-process-result)
  (let* ([children (index-node-children current-index-node)]
      [ann (index-node-datum/annotations current-index-node)]
      [expression (annotation-stripped ann)])
    (cond 
      [(and (symbol? expression) (contain? (map car template-process-result) current-index-node))
        (let ([target (find (lambda (p) (equal? (car p) expression)) local-environment)])
          (if target
            `(,(annotation-stripped (index-node-datum/annotations (cdr target))) 
              . ,(cdr target))
            '()))]
      [(or (symbol? expression) (null? expression))
        `(,expression . ,expression)]
      [(list? expression)
        (let loop ([body children] [result '(() . ())][e local-environment][brother '()])
          (cond 
            [(null? body) result]
            [(equal? '... (annotation-stripped (index-node-datum/annotations (car body)))) 
              (let ([child brother]
                  [r (substitute child e template-process-result)]
                  [deleted (private:dry (cdr r))]
                  [new-e (filter (lambda (i) (not (contain? deleted (cdr i)))) e)])
                (loop
                  body
                  `(,(append (car result) (car r)) . ,(append (cdr result) (cdr r)))
                  ;only for ...
                  new-e
                  child))]
            [else 
              (let ([child (car body)]
                  [r (substitute child local-environment template-process-result)]
                  [deleted (private:dry (cdr r))]
                  [new-e (filter (lambda (i) (not (contain? deleted (cdr i)))) e)])
                (loop
                  (cdr body)
                  `(,(append (car result) (car r)) . ,(append (cdr result) (cdr r)))
                  ;only for ...
                  new-e
                  child))]))]
      [(pair? expression)
        (let* ([head-child (car children)]
            [tail-child (cdr children)]
            [head-result (substitute head-child '() local-environment template-process-result)]
            [child-result (substitute tail-child head-child local-environment template-process-result)])
          `((,head-expression . ,tail-expression) . ((,head-child))))]
      [(vector? expression) 
        (let ([children-result 
            (map (lambda (child) (substitute child '() local-environment template-process-result)) children)])
          `(,(list->vector (map car children-result)) . ,(map cdr children-result)))])))

(define (template-process pattern-index-node template-index-node pattern-symbols document)
  (let* ([ann (index-node-datum/annotations template-index-node)]
      [expression (annotation-stripped ann)]
      [children (annotation-children template-index-node)])
    (cond 
      [(not (null? children)) 
        (fold-left
          (lambda (left right)
            (append left (template-process pattern-index-node right pattern-symbols document)))
          '()
          children)]
      [(not (symbol? expression)) '()]
      [(contain? pattern-symbols expression) 
        (if 
          (contain? 
            (map 
              identifier-reference-index-node
              (find-available-references-for document template-index-node expression))
            template-index-node)
          ; `((,template-index-node . , (find-available-references-for document template-index-node)))
          `(,template-index-node)
          '())]
      [else '()])))

(defien (private:dry tree)
  (cond 
    [(index-node? tree) `(,tree)]
    [(null? tree) '()]
    [(pair? tree) `(,@(private:dry (car tree)) . ,(private:dry (cdr tree)))]))
)