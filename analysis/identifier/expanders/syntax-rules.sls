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
            ;todo: match index-nodes in expansion and its expression form.
            )
          ()
        )
    )]
    [else '()]))

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