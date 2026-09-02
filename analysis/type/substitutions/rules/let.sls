(library (scheme-langserver analysis type substitutions rules let)
  (export 
    let-process
    let:emit-substitutions!)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

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
)
