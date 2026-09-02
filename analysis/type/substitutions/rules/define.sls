(library (scheme-langserver analysis type substitutions rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

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
      (let* ([parameter-types (private:collect-param-types (cadr (index-node-children signature)))]
          [lambda-details (construct-lambdas-with (list return) (list parameter-types))])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list name t))
          lambda-details))]
    [(:_ (? index-node-symbol? name) :_ ... return)
      (extend-index-node-substitution-list name return)
      (extend-index-node-substitution-list return name)]
    [else '()]))

; Collect parameter types for a dotted formal list. The last parameter (the rest
; parameter) is represented as (inner:list? something? ...).
(define (private:collect-param-types formals-node)
  (let ([expression (annotation-stripped (index-node-datum/annotations formals-node))]
      [children (index-node-children formals-node)])
    (cond
      [(symbol? expression) `((inner:list? something? ...))]
      [(null? children) '()]
      [(null? (cdr children)) `((inner:list? something? ...))]
      [else
        (let ([car-node (car children)]
            [cdr-node (cadr children)])
          (if (symbol? (annotation-stripped (index-node-datum/annotations cdr-node)))
            `(,car-node (inner:list? something? ...))
            (cons car-node (private:collect-param-types cdr-node))))])))
)
