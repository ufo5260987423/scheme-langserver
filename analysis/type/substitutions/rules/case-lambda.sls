(library (scheme-langserver analysis type substitutions rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (case-lambda-process document index-node)
  (match-index-node index-node
    [(:_ clauses **1)
      (for-each 
        (lambda (clause) (private-clause-process index-node clause))
        clauses)]
    [else '()]))

(define (private-clause-process root-index-node clause)
  (match-index-node clause
    [((and :_
            (? index-node-proper-list?)
            ((? index-node-symbol? params) ...))
      :_ ... return)
      (for-each 
        (lambda (t) (extend-index-node-substitution-list root-index-node t))
        (construct-lambdas-with 
          `(,return)
          (construct-parameter-index-nodes-products-with params)))]
    [(formals :_ ... return)
      (let ([parameter-types (private:collect-param-types formals)])
        (for-each 
          (lambda (t) (extend-index-node-substitution-list root-index-node t))
          (construct-lambdas-with `(,return) (list parameter-types))))]
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
