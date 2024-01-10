(library (scheme-langserver analysis type substitutions rules let)
  (export let-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
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
        [('let (? symbol? loop-identifier) (((? symbol? identifier) value ) ... ) _ **1) 
          (guard-for document index-node 'let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
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
                  (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) 
                  key-value-index-nodes))))]
        [('let (((? symbol? identifier) value) ...) _ **1) 
          (guard-for document index-node 'let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) key-value-index-nodes))))]
        ; [('fluid-let (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
        ;   (guard-for document index-node 'fluid-let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        ;   (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ;     (if (not (null? rest))
        ;       (let* ([identifier-parent-index-node (car rest)]
        ;             [identifier-index-node (car (index-node-children identifier-parent-index-node))])
        ;         (index-node-excluded-references-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-excluded-references identifier-parent-index-node)
        ;             (private-process identifier-index-node index-node '() document 'variable)))
        ;         (loop (cdr rest)))))]
        ; [('fluid-let-syntax (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
        ;   (guard-for document index-node 'fluid-let '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        ;   (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ;     (if (not (null? rest))
        ;       (let* ([identifier-parent-index-node (car rest)]
        ;             [identifier-index-node (car (index-node-children identifier-parent-index-node))])
        ;         (index-node-excluded-references-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-excluded-references identifier-parent-index-node)
        ;             (private-process identifier-index-node index-node '() document 'syntax-variable)))
        ;         (loop (cdr rest)))))]
        ; [('let-syntax (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
        ;   (guard-for document index-node 'let-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        ;   (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ;     (if (not (null? rest))
        ;       (let* ([identifier-parent-index-node (car rest)]
        ;             [identifier-index-node (car (index-node-children identifier-parent-index-node))])
        ;         (index-node-excluded-references-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-excluded-references identifier-parent-index-node)
        ;             (private-process identifier-index-node index-node '() document 'syntax-variable)))
        ;         (loop (cdr rest)))))]
        ; [('let-values (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
        ;   (guard-for document index-node 'let-values '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        ;   (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
        ;     (if (not (null? rest))
        ;       (let* ([identifier-parent-index-node (car rest)]
        ;             [identifier-index-node (car (index-node-children identifier-parent-index-node))])
        ;         (index-node-excluded-references-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-excluded-references identifier-parent-index-node)
        ;             (private-process identifier-index-node index-node '() document 'syntax-variable)))
        ;         (loop (cdr rest)))))]
        [('let* (((? symbol? identifier) value) ... ) _ **1 ) 
          (guard-for document index-node 'let* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) key-value-index-nodes))))]
        ; [('let*-values (((? symbol? identifier) no-use ... ) **1 ) _ ... ) 
        ;   (guard-for document index-node 'let*-values '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        ;   (let loop ([include '()] 
        ;         [rest (index-node-children (cadr (index-node-children index-node)))])
        ;     (if (not (null? rest))
        ;       (let* ([identifier-parent-index-node (car rest)]
        ;             [identifier-index-node (car (index-node-children identifier-parent-index-node))]
        ;             [reference-list (private-process identifier-index-node index-node '() document 'variable)])
        ;         (index-node-excluded-references-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-excluded-references identifier-parent-index-node)
        ;             reference-list))
        ;         (index-node-references-import-in-this-node-set! 
        ;           identifier-parent-index-node
        ;           (append 
        ;             (index-node-references-import-in-this-node identifier-parent-index-node)
        ;             include))
        ;         (loop (append include reference-list) (cdr rest)))))]
        [('letrec (((? symbol? identifier) value ) ... ) _ **1) 
          (guard-for document index-node 'letrec '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) key-value-index-nodes))))]
        [('letrec-syntax (((? symbol? identifier) value) ... ) _ **1) 
          (guard-for document index-node 'letrec-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) key-value-index-nodes))))]
        [('letrec* (((? symbol? identifier) value) ...) _ **1) 
          (guard-for document index-node 'letrec* '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (private-process-key-value substitutions key-value-index-node)) key-value-index-nodes))))]
        [else '()])
      (except c
        [else '()]))))

(define (private-process-key-value substitutions parent-index-node)
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
