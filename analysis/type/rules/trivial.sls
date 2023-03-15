(library (scheme-langserver analysis type rules trivial)
  (export trivial-process)
  (import 
    (chezscheme) 

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (trivial-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [variable (make-variable)]
      [new-substitutions (add-to-substitutions substitutions `(,index-node : ,variable))])
    (add-to-substitutions new-substitutions
      (if (null? (index-node-children index-node))
        (cond
          ;todo: detailed literal analysis may be done in the future.
          [(list? expression) `(,variable : ,(construct-type-expression-with-meta 'list?))]
          [(vector? expression) `(,variable : ,(construct-type-expression-with-meta 'vector?))]
          [(char? expression) `(,variable : ,(construct-type-expression-with-meta 'char?))]
          [(string? expression) `(,variable : ,(construct-type-expression-with-meta 'string?))]
          [(boolean? expression) `(,variable : ,(construct-type-expression-with-meta 'boolean?))]
          [(fixnum? expression) `(,variable : ,(construct-type-expression-with-meta 'fixnum?))]
          [(bignum? expression) `(,variable : ,(construct-type-expression-with-meta 'bignum?))]
          [(integer? expression) `(,variable : ,(construct-type-expression-with-meta 'integer?))]
          [(cflonum? expression) `(,variable : ,(construct-type-expression-with-meta 'cflonum?))]
          [(flonum? expression) `(,variable : ,(construct-type-expression-with-meta 'flonum?))]
          [(rational? expression) `(,variable : ,(construct-type-expression-with-meta 'rational?))]
          [(real? expression) `(,variable : ,(construct-type-expression-with-meta 'real?))]
          [(complex? expression) `(,variable : ,(construct-type-expression-with-meta 'complex?))]
          [(number? expression) `(,variable : ,(construct-type-expression-with-meta 'number?))]
          [(symbol? expression) 
          `(,variable : ,(construct-type-expression-with-meta 'number?))
          ]
          [(symbol? expression) 
            (apply 
              append 
              (map 
                (lambda (identifier-reference) 
                  (private-process identifier-reference variable))
                (find-available-references-for document index-node expression)))]
          [else '()])
        '()))))

(define (private-process identifier-reference variable)
  (if (null? (identifier-reference-parent identifier-reference))
    (let* ([target-document (identifier-reference-document identifier-reference)]
        [target-substitution-list (document-substitution-list target-document)]
        [target-index-node (identifier-reference-index-node identifier-reference)])
      ;it's in r6rs librar?
      (if (null? target-index-node)
        (map 
          (lambda (expression)
            `(,variable : ,expression))
          (identifier-reference-type-expressions identifier-reference))
        (apply append
          (map 
            (lambda (reified)
              (if (is-pure-identifier-reference-misture? reified)
                `(,variable : ,reified)
                `(,variable = ,reified)))
            (reify target-substitution-list target-index-node)))))
    (private-process (identifier-reference-parent identifier-reference) variable)))
)
