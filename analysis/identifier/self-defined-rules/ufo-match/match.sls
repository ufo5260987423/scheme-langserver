(library (scheme-langserver analysis identifier self-defined-rules ufo-match match)
  (export match-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

(define (match-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ rest-children-index-nodes **1)
      (map 
        (lambda (i) 
          (let ([c (index-node-children i)])
            (if (not (null? c))
              (private:pattern+scope document (car c) i (car c)))))
        rest-children-index-nodes)]
    [else '()]))

(define (private:pattern+scope document pattern-index-node scope-index-node exclude-index-node)
  (match-index-node pattern-index-node
    [(:= index-node-expression (? private:check? s)) '()]
    [(:= index-node-expression (? symbol? s))
      (let* ([r (make-identifier-reference s document pattern-index-node (index-node-parent scope-index-node) '() 'variable '() '())])
        (append-references-into-ordered-references-for document scope-index-node `(,r))
        (index-node-excluded-references-set! pattern-index-node 
          (append (index-node-excluded-references exclude-index-node) `(,r)))
        (index-node-references-export-to-other-node-set! pattern-index-node 
          (append (index-node-references-export-to-other-node pattern-index-node) `(,r))))]
    [() '()]
    [('set! :_ (? index-node-symbol? last-index-node))
      (private:pattern+scope document last-index-node scope-index-node exclude-index-node)]
    [('? :_ (? index-node-symbol? last-index-node))
      (private:pattern+scope document last-index-node scope-index-node exclude-index-node)]
    [('= :_ (? index-node-symbol? last-index-node))
      (private:pattern+scope document last-index-node scope-index-node exclude-index-node)]
    [('and rest-children-index-nodes **1)
      (map 
        (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
        rest-children-index-nodes)]
    [('or rest-children-index-nodes **1)
      (map 
        (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
        rest-children-index-nodes)]
    [('not rest-children-index-nodes **1)
      (map 
        (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
        rest-children-index-nodes)]
    [('& :_ field-nodes **1)
      (map 
        (lambda (i) (private:pattern+scope document (cadr (index-node-children i)) scope-index-node exclude-index-node))
        (filter 
          (lambda (i) (>= (length (index-node-children i)) 2))
          field-nodes))]
    [(children **1)
      (map 
        (lambda (i) (private:pattern+scope document i scope-index-node exclude-index-node))
        children)]))

(define (private:check? s)
  (case s 
    [(... *** **1 =.. = & set! and or not _ else) #t]
    [else #f]))
)
