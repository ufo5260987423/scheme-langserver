(library (scheme-langserver analysis type type-inferencer)
  (export 
    match)
  (import 
    (chezscheme)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type argument-checker))

(define match
  (case-lambda
    ([document] (map (lambda(index-node) (match index-node document)) (document-index-nodes document)))
    ([index-node document]
      (let* ([ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)])
        (if (null? (index-node-children index-node))
          (cond
            [(list? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'list?)))]
            [(vector? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'vector?)))]
            [(char? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'char?)))]
            [(string? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'string?)))]
            [(boolean? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'boolean?)))]
            [(fixnum? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'fixnum?)))]
            [(bignum? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'bignum?)))]
            [(integer? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'integer?)))]
            [(cflonum? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'cflonum?)))]
            [(flonum? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'flonum?)))]
            [(rational? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'rational?)))]
            [(real? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'real?)))]
            [(complex? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'complex?)))]
            [(number? expression) (index-node-actrual-have-type-set! index-node `(,(construct-type-expression-with-meta 'number?)))]
            [(symbol? expression) 
              (index-node-actrual-have-type-set! 
                current-index-node 
                (dedupe 
                  (apply append
                    (map 
                      (lambda(identifier-reference)
                        (map 
                          (lambda(type-expression)
                            (cond
                              [(and 
                                (equal? 'procedure (identifier-reference-type identifier-reference)) 
                                (> 1 (length type-expression)))
                                (construct-type-expression-with-meta 'procedure?)]
                          ;; according identifier-reference's finding strategy, their expression should be setted previously.
                              [(= 1 (length type-expression)) type-expression]
                          ;; other cases like syntax-transformer would raise invalid syntax exception
                              [else '()]))
                            (identifier-reference-type-expressions identifier-reference)))
                      (find-available-references-for document index-node expression)))))])
          (begin
            (map (lambda(i) (match i document)) (index-node-children index-node))
            ;;todo
            '()
            (if (symbol? (car expression))
              (argument-checker-attach (cdr (index-node-children index-node)) document (find-available-references-for document index-node (car expression))))
          ))))))
)