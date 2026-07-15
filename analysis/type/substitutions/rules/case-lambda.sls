(library (scheme-langserver analysis type substitutions rules case-lambda)
  (export case-lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node))

(define (case-lambda-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ clause **1) 
        (for-each 
          (lambda (clause-index-node)
            (private-clause-process index-node clause-index-node))
          (cdr children))]
      [else '()])))

(define (private-clause-process root-index-node clause-wrapper-node)
  (let ([children (index-node-children clause-wrapper-node)]
      [expression (annotation-stripped (index-node-datum/annotations clause-wrapper-node))])
    (match expression
      [(((? symbol? parameter) ...) :_ **1) 
        (for-each 
          (lambda (t) (extend-index-node-substitution-list root-index-node t))
          (construct-lambdas-with 
            `(,(car (reverse children)))
            (construct-parameter-index-nodes-products-with (index-node-children (car children)))))]
      [((identifier . rest) :_ **1) 
        (let* ([inner-clause-node (car children)]
            [inner-children (index-node-children inner-clause-node)]
            [return-index-node (car (reverse inner-children))]
            [formals-index-node (car inner-children)]
            [parameter-types (private:collect-param-types formals-index-node)])
          (for-each 
            (lambda (t) (extend-index-node-substitution-list root-index-node t))
            (construct-lambdas-with `(,return-index-node) (list parameter-types))))]
      [else '()])))

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
