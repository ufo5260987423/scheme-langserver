(library (scheme-langserver analysis type substitutions rules define)
  (export define-process possible-fix-point-process:define)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions fixpoint)

    (scheme-langserver virtual-file-system index-node))

(define (define-process document index-node)
  (match-index-node index-node
    [(:_ (and signature
               (? index-node-proper-list?)
               ((? index-node-symbol? name) . params))
        :_ ... return)
      (let* ([parameter-index-nodes-products (construct-parameter-index-nodes-products-with params)]
          [lambda-details (construct-lambdas-with (list return) parameter-index-nodes-products)])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [(:_ (and signature
               (? index-node-improper-list?)
               ((? index-node-symbol? name) . :_))
        :_ ... return)
      (let* ([parameter-types (construct-rest-parameter-template (cdr (index-node-children signature)))]
          [lambda-details (construct-lambdas-with (list return) (list parameter-types))])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [(:_ (? index-node-symbol? name) :_ ... return)
      (extend-index-node-substitution-list name return)
      (extend-index-node-substitution-list return name)]
    [else '()]))

;;fixpoint (P1) collection hook, mounted as the cddr half of the rule chain
;;in generator.sls (see fixpoint.sls's header): runs AFTER this define's
;;subtree has been stepped. Builds a raw recursion group for the three
;;recursive-define shapes and notes it; merging and solving stay in the
;;gated fixpoint pass.
(define (possible-fix-point-process:define document index-node)
  (let ([children (index-node-children index-node)])
    (when (and (>= (length children) 3) (index-node? (cadr children)))
      (let* ([name-expr
              (annotation-stripped
                (index-node-datum/annotations (cadr children)))])
        (cond
          ;;(define (name . params) body ...)
          [(and (pair? name-expr) (not (null? name-expr)) (symbol? (car name-expr)))
            (private:note-define-group! document (car name-expr) (cddr children))]
          ;;(define name (lambda ...)) / (define name (case-lambda ...))
          [(symbol? name-expr)
            (let ([value-node (caddr children)])
              (when (index-node? value-node)
                (let* ([value-kids (index-node-children value-node)]
                    [value-head-expr
                      (and (not (null? value-kids))
                        (let ([e (annotation-stripped
                                    (index-node-datum/annotations (car value-kids)))])
                          (and (symbol? e) e)))])
                  (cond
                    [(eq? 'lambda value-head-expr)
                      (private:note-define-group! document name-expr (cddr value-kids))]
                    [(eq? 'case-lambda value-head-expr)
                      (private:note-case-lambda-group! document name-expr (cdr value-kids))]
                    [else '()]))))]
          [else '()])))))

;;(define (name . params) body ...) / (define name (lambda ...))
(define (private:note-define-group! document name body)
  (when (and name (not (null? body)))
    (let ([calls (fixpoint-find-calls body)])
      (unless (null? calls)
        (fixpoint-note-group! document
          `(define (,name) ,(list (list-ref body (- (length body) 1))) ,calls))))))

;;(define name (case-lambda ...)) -- each clause is an independent return
;;chain; solve jointly
(define (private:note-case-lambda-group! document name clauses)
  (let ([return-nodes
          (filter index-node?
            (map
              (lambda (clause)
                (let ([body (cdr (index-node-children clause))])
                  (if (null? body) #f (list-ref body (- (length body) 1)))))
              clauses))]
      [calls
        (fold-left
          (lambda (acc clause)
            (append acc (fixpoint-find-calls (cdr (index-node-children clause)))))
          '()
          clauses)])
    (unless (null? calls)
      (fixpoint-note-group! document `(define (,name) ,return-nodes ,calls)))))
)
