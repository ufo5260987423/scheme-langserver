(library (scheme-langserver analysis identifier expanders syntax-rules)
  (export )
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver analysis identifier expanders pattern)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference))

;input-index-node is supposed have the form of `(syntax-rules ...)`
(define (syntax-rules root-file-node root-library-node document input-index-node)
  (match (annotation-stripped (index-node-datum/annotations input-index-node))
  ;clause means pattern and template
    [(_ (literals ...) clauses **1) 
      ;return a procedure
      (lambda (local-root-file-node local-root-library-node local-document local-index-node)
        (let* ([local-expression (annotation-stripped (index-node-datum/annotations local-index-node))]
            [clause-index-nodes (cddr (index-node-children input-index-node))]
            [index+expansion (private:confirm-clause literals clause-index-nodes local-expression)]
            [index (car index+expansion)]

            [clause-index-node (vector-ref (list->vector clause-index-nodes) index)]
            [clause-expression (annotation-stripped (index-node-datum/annotations clause-index-node))]
            [expansion-expression (cdr index+expansion)]

            [pattern-expression (cdar clause-expression)]
            [pattern (make-pattern pattern-expression)]
            [template-expression (car (reverse clause-expression))]
            [template-pattern (make-pattern template-expression)]
            [pattern-context (gather-context pattern)]
            [pairs (pattern+index-node->pair-list pattern local-index-node)]
            [bindings (map (lambda (literal) (generate-binding literal ((pattern+context->pairs->iterator literal context) pairs))) (pattern-exposed-literals template-pattern))]
            [expansion (expand->index-node-compound-list template bindings context)]

            ;todo: expansion and expansion-expression should be emmm, isomophism? This should be checked.
            [expansion-index-node 
              (init-index-node 
                local-index-node
                (car 
                  (source-file->annotations 
                    (with-output-to-string (lambda () (pretty-print expansion-expression)))
                    (uri->path (document-uri local-document)))))]
            [pairs (private:expansion+index-node->pairs expansion expansion-index-node)])
          ()
        )
    )]
    [else '()]))

;these two parameter are supposed to be correct and this procedure won't do fault-tolerant things.
(define (private:expansion+index-node->pairs compound-list index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)])
    (cond 
      [(index-node? compound-list) `((,index-node . ,compound-list))]
      [(list? compound-list) 
        (apply append 
          (map 
            (lambda (left right) (private:expansion+index-node->pairs left right))
            children
            compound-list))]
      [(vector? compound-list) 
        (private:expansion+index-node->pairs (vector->list compound-list) index-node)]
      [(pair? compound-list) 
        (private:expansion+index-node->pairs `(,(car compound-list) ,(cdr compound-list)) index-node) ]
      ;symbol won't get pairs
      [else '()])))

(define (private:confirm-clause literals clause-index-nodes input-expression)
  (let loop ([rest clause-index-nodes] [index 0])
    (let* ([current-clause-index-node (car rest)]
        [current-clause-expression (annotation-stripped (index-node-datum/annotations current-clause-index-node))]
      [target (syntax->datum (eval 
        `(syntax-case ',input-expression ,literals 
          (,(car current-clause-expression) 
            ;result
            '(,index . #'(,(cdar current-clause-expression) . ,(car (reverse current-clause-expression)))))
          (else  #f))))])
      (if target
        (if (null? rest)
          #f
          (loop (cdr rest) (+ 1 index)))))))
)