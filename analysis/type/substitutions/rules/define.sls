(library (scheme-langserver analysis type substitutions rules define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (define-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ ((? symbol? identifiers) (? symbol? parameters) ... ) tail) 
        (let* ([identifier-index-node (car (index-node-children (cadr (index-node-children index-node))))]
            [tail-index-node (car (reverse (index-node-children index-node)))]

            [parameter-index-nodes (cdr (index-node-children (cadr (index-node-children index-node))))]
            [parameter-index-nodes-products (construct-parameter-index-nodes-products-with parameter-index-nodes)]
            [lambda-details (construct-lambdas-with (list tail-index-node) parameter-index-nodes-products)])
          (for-each 
            (lambda (t)
              (extend-index-node-substitution-list identifier-index-node t))
            lambda-details))]
      [(_ ((? symbol? identifier) . rest) tail)
        (let* ([identifier-index-node (car (index-node-children (cadr (index-node-children index-node))))]
            [tail-index-node (car (reverse (index-node-children index-node)))]
            [formals-index-node (cadr (index-node-children index-node))]
            [parameter-types (private:collect-param-types (cadr (index-node-children formals-index-node)))]
            [lambda-details (construct-lambdas-with (list tail-index-node) (list parameter-types))])
          (for-each 
            (lambda (t)
              (extend-index-node-substitution-list identifier-index-node t))
            lambda-details))]
      [(_ (? symbol? identifiers) tail) 
        (let* ([identifier-index-node (cadr (index-node-children index-node))]
            [tail-index-node (car (reverse (index-node-children index-node)))])
          (extend-index-node-substitution-list identifier-index-node tail-index-node)
          (extend-index-node-substitution-list tail-index-node identifier-index-node))]
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
