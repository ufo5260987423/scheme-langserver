(library (scheme-langserver analysis identifier expanders syntax-rules)
  (export )
  (import 
    (chezscheme)
    (ufo-match)
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
            [expansion (cdr index+expansion)]

            ;we'll use these following
            [pattern-expression (cdar clause-expression)]
            [template-expression (car (reverse clause-expression))]
            [pattern-expansion (car expansion)]
            [template-expansion (cdr expansion)])
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