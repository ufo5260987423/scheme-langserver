(library (scheme-langserver analysis type substitutions rules let)
  (export 
    let-process
    let:private-process-key-value)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (let-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ (? symbol? loop-identifier) (((? symbol? identifier) value ) ... ) _ **1) 
          (let* ([return-index-node (car (reverse children))]
              [return-variable (index-node-variable return-index-node)]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (caddr children))]
              ;identifier index-nodes
              [key-index-nodes (map car (map index-node-children key-value-index-nodes))]
              [parameter-variable-products (construct-parameter-variable-products-with key-index-nodes)]

              ;(? symbol? loop-identifier)
              [loop-index-node (cadr children)]
              [loop-variable (index-node-variable loop-index-node)]
              ;((return-variable (parameter-variable ...)) **1)
              [loop-procedure-details (construct-lambdas-with `(,return-variable) parameter-variable-products)])
            (append 
              (apply append 
                (map 
                  (lambda (current-index-node)
                    (private-process-loop 
                      document 
                      loop-identifier 
                      current-index-node 
                      loop-index-node 
                      (map index-node-variable key-index-nodes)))
                  (cdddr children)))
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for loop procedure
              (cartesian-product `(,loop-variable) '(=) loop-procedure-details)
              ;for key value index-nodes
              (apply append 
                (map 
                  (lambda (key-value-index-node) (let:private-process-key-value key-value-index-node)) 
                  key-value-index-nodes))))]
        [(_ (((? symbol? identifier) value) ...) _ **1) 
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (let:private-process-key-value key-value-index-node)) key-value-index-nodes))))]
        [else '()])
      (except c
        [else '()]))))

(define (let:private-process-key-value parent-index-node)
  (let* ([ann (index-node-datum/annotations parent-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children parent-index-node)])
    (match expression 
      [((? symbol? left) value) 
        (append 
          (construct-substitutions-between-index-nodes (car children) (cadr children) '=)
          (construct-substitutions-between-index-nodes (cadr children) (car children) '=))]
      [else '()])))

(define (private-process-loop document loop-identifier current-index-node loop-index-node key-index-node-variables)
  (fold-left
    (lambda (left right)
      (append left (private-process-loop document loop-identifier right loop-index-node key-index-node-variables)))
    (let ([expression (annotation-stripped (index-node-datum/annotations current-index-node))]
        [check? (lambda (target) (equal? loop-identifier target))])
      (match expression
        [((? check? loop-target) _ **1)
          (if (and 
              (contain? 
                (map 
                  identifier-reference-index-node 
                  (find-available-references-for document current-index-node loop-identifier)) 
                loop-index-node)
              (= (length key-index-node-variables) (length _)))
            (let loop ([rest-variables (map index-node-variable (cdr (index-node-children current-index-node)))]
                [key-variables key-index-node-variables]
                [new-substitutions '()])
              (if (null? rest-variables)
                new-substitutions
                (loop 
                  (cdr rest-variables)
                  (cdr key-variables)
                  `(,@new-substitutions (,(car key-variables) = ,(car rest-variables))))))
            '())]
        [else '()]))
    (index-node-children current-index-node)))
)
