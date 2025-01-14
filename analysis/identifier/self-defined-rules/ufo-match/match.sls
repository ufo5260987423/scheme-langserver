(library (scheme-langserver analysis identifier self-defined-rules ufo-match match)
  (export match-process)
  (import 
    (chezscheme) 
    (ufo-match)
    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (match-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ something **1)
          (let* ([children (index-node-children index-node)]
              [rest-children-index-nodes (cdr children)])
            (map 
              (lambda (i) 
                (let ([c (index-node-children i)])
                  (if (not (null? c))
                    (private:pattern+scope document (car c) i (car c)))))
              rest-children-index-nodes))]
        [else '()])
      (except c
        [else '()]))))

(define (private:pattern+scope document pattern-index-node scope-index-node exclude-index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations pattern-index-node))]
      [children (index-node-children pattern-index-node)])
    (match expression 
      ['() '()]

      [(? private:check? s) '()]
      [(? symbol? s)
        (let* ([r (make-identifier-reference s document pattern-index-node pattern-index-node '() 'variable '() '())])
          (append-references-into-ordered-references-for document scope-index-node `(,r))
          (index-node-excluded-references-set! pattern-index-node 
            (append (index-node-excluded-references exclude-index-node) `(,r))))]
      ; [('set! (? symbol? s)) (private:pattern+scope document (car (reverse children)) scope-index-node exclude-index-node)]
      [('? something (? symbol? s)) (private:pattern+scope document (car (reverse children)) scope-index-node exclude-index-node)]
      [('= something (? symbol? s)) (private:pattern+scope document (car (reverse children)) scope-index-node exclude-index-node)]
      [('and (? symbol? s) **1) 
        (map 
          (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
          (cdr children))]
      [('or (? symbol? s) **1) 
        (map 
          (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
          (cdr children))]
      [('not (? symbol? s) **1) 
        (map 
          (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
          (cdr children))]
      [('& something ((? symbol? fuzzy) (? symbol? s)) **1) 
        (map 
          (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
          (map cadr 
            (map 
              index-node-children
              (cddr children))))]
      
      [else 
        (map 
          (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
          children)])))

(define (private:check? s)
  (case s 
    [(... *** **1 =.. = & set! and or not _ else) #t]
    [else #f]))
)