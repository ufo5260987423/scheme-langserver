(library (scheme-langserver analysis type rules trivial)
  (export trivial-process)
  (import 
    (chezscheme) 

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (trivial-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (cond
      [(list? expression) (list `(,index-node ,(construct-type-expression-with-meta 'list?)))]
      [(vector? expression) (list `(,index-node ,(construct-type-expression-with-meta 'vector?)))]
      [(char? expression) (list `(,index-node ,(construct-type-expression-with-meta 'char?)))]
      [(string? expression) (list `(,index-node ,(construct-type-expression-with-meta 'string?)))]
      [(boolean? expression) (list `(,index-node ,(construct-type-expression-with-meta 'boolean?)))]
      [(fixnum? expression) (list `(,index-node ,(construct-type-expression-with-meta 'fixnum?)))]
      [(bignum? expression) (list `(,index-node ,(construct-type-expression-with-meta 'bignum?)))]
      [(integer? expression) (list `(,index-node ,(construct-type-expression-with-meta 'integer?)))]
      [(cflonum? expression) (list `(,index-node ,(construct-type-expression-with-meta 'cflonum?)))]
      [(flonum? expression) (list `(,index-node ,(construct-type-expression-with-meta 'flonum?)))]
      [(rational? expression) (list `(,index-node ,(construct-type-expression-with-meta 'rational?)))]
      [(real? expression) (list `(,index-node ,(construct-type-expression-with-meta 'real?)))]
      [(complex? expression) (list `(,index-node ,(construct-type-expression-with-meta 'complex?)))]
      [(number? expression) (list `(,index-node ,(construct-type-expression-with-meta 'number?)))]
      [(symbol? expression) 
          (apply 
            append 
            (map 
              (lambda (identifier-reference)
              ;its in r6rs library
                (if (null? (identifier-reference-index-node identifier-reference))
                  (map 
                    (lambda (type-expression)
                      `(,index-node ,type-expression))
                    (identifier-reference-type-expressions identifier-reference))
                  (list `(,index-node ,(identifier-reference-index-node identifier-reference)))))
              (find-available-references-for document index-node expression)))]
      [else '()])))
)
