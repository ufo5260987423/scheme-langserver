(library (scheme-langserver analysis type type-inferencer)
  (export type-inference-for)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type rules lambda)
    (scheme-langserver analysis type rules define)

    (scheme-langserver analysis type argument-checker)
    (scheme-langserver analysis type util)
    
    ; (scheme-langserver minikanren)
    )

;; it's acturally the walk procedure in minikanren
;; We regard the indexes and references as a graph of existed variable and values. Of course, 
;; index-nodes denoted themselves, and corresponding actural-have-type/type-expressions denoted values and type notions.
(define type-inference-for 
  (case-lambda
    ([document] (map (lambda(index-node) (type-inference-for index-node document)) (document-index-node-list document)))
    ([document index-node] 
      (let* ([ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)]
          [parent (index-node-parent index-node)]
          [children (index-node-children index-node)]
          [actural-have-type (index-node-actural-have-type index-node)])
        (if (null? children)
        ; Variable Access Rule
        ;x:\sigma
          (if (null? actural-have-type)
            (cond
              [(list? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'list?))]
              [(vector? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'vector?))]
              [(char? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'char?))]
              [(string? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'string?))]
              [(boolean? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'boolean?))]
              [(fixnum? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'fixnum?))]
              [(bignum? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'bignum?))]
              [(integer? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'integer?))]
              [(cflonum? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'cflonum?))]
              [(flonum? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'flonum?))]
              [(rational? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'rational?))]
              [(real? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'real?))]
              [(complex? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'complex?))]
              [(number? expression) (index-node-actural-have-type-set! index-node (construct-type-expression-with-meta 'number?))]
            ;maybe here's a empty type-expression
              [(symbol? expression) 
                (index-node-actural-have-type-set! 
                  index-node 
                  (append '(or)
                    (dedupe 
                      (apply append
                        (map identifier-reference-type-expressions (find-available-references-for document index-node expression))))))]
              [else '()])
            actural-have-type)
          (let* ([head (car expression)]
              [head-node (car children)]
              [head-node-actural-have-type (index-node-actural-have-type head-node)]
              [param-nodes (cdr children)])
            (map 
              (lambda(i) (type-inference-for document i))
              children)
            ;;todo
            ; (if-process document index-node)
            ; (cond-process document index-node)
            (lambda-process document index-node)
            (define-process document index-node)

          ;; Application Rule
            ; (cond
            ;   [(symbol? head) 
            ;     (argument-checker-attach param-node document (find-available-references-for document index-node head))]
            ;   [(and (list? head) (lambda? (index-node-actural-have-type head-node)))
            ;     (argument-checker-attach param-node document (cdr (index-node-actural-have-type head-node)) head-node document)]
            ;   [else '()])
              ))))))
)