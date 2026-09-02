(library (scheme-langserver analysis type substitutions rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (lambda-process document index-node)
  (match-index-node index-node
    [(:_ (and :_
               (? index-node-proper-list?)
               ((? index-node-symbol? params) ...))
        . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (construct-parameter-index-nodes-products-with params))]
    [(:_ (? index-node-symbol? rest) . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (list `((inner:list? something? ...))))]
    [(:_ formals . (body ... return))
      (private:emit-lambda-substitutions index-node return
        (list (private:collect-param-types formals)))]
    [else '()]))

(define (private:emit-lambda-substitutions index-node return-node parameter-type-products)
  (for-each 
    (lambda (t) (extend-index-node-substitution-list index-node t))
    (construct-lambdas-with `(,return-node) parameter-type-products)))

(define (private:collect-param-types formals-node)
  (let loop ([children (index-node-children formals-node)])
    (cond
      [(null? children) '()]
      [(null? (cdr children)) `((inner:list? something? ...))]
      [else (cons (car children) (loop (cdr children)))])))
)
