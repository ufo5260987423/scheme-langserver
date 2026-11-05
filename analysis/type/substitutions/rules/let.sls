(library (scheme-langserver analysis type substitutions rules let)
  (export 
    let-process
    let:emit-substitutions!
    possible-fix-point-process:named-let)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions fixpoint)

    (scheme-langserver virtual-file-system index-node))

(define (let-process document index-node)
  (match-index-node index-node
    [(:_ (? index-node-symbol? loop-identifier) ((left value) ...) :_ ... return)
      (let* ([parameter-index-nodes-products (construct-parameter-index-nodes-products-with left)]
          [loop-procedure-details (construct-lambdas-with `(,return) parameter-index-nodes-products)])
        (let:emit-substitutions! index-node return left value)
        (for-each 
          (lambda (t) (extend-index-node-substitution-list loop-identifier t))
          loop-procedure-details))]
    [(:_ ((left value) ...) :_ ... return)
      (let:emit-substitutions! index-node return left value)]
    [else '()]))

(define (let:emit-substitutions! index-node return left value)
  (extend-index-node-substitution-list index-node return)
  (extend-index-node-substitution-list return index-node)
  (for-each 
    (lambda (l v)
      (extend-index-node-substitution-list l v)
      (extend-index-node-substitution-list v l))
    left value))

;;fixpoint (P1) collection hook for NAMED let, mounted as the cddr half of
;;the rule chain in generator.sls (see fixpoint.sls's header): runs after
;;the let's subtree has been stepped. Named-let groups stand alone (never
;;merge) and solve under their own name.
(define (possible-fix-point-process:named-let document index-node)
  (let ([children (index-node-children index-node)])
    (when (>= (length children) 4)
      (let ([name
              (let ([name-node (cadr children)])
                (and (index-node? name-node)
                  (let ([e (annotation-stripped
                              (index-node-datum/annotations name-node))])
                    (and (symbol? e) e))))])
        (when name
          (let ([body (cdddr children)])
            (unless (null? body)
              (let ([calls (fixpoint-find-calls body)])
                (unless (null? calls)
                  (fixpoint-note-group! document
                    `(named-let (,name) ,(list (list-ref body (- (length body) 1))) ,calls)))))))))))
)
