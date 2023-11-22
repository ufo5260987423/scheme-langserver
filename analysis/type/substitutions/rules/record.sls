(library (scheme-langserver analysis type substitutions rules record)
  (export record-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util cartesian-product)
    (scheme-langserver util try)
    (scheme-langserver util sub-list)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (record-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [('define-record-type dummy0 dummy1 ...) 
        (guard-for document index-node 'define-record-type '(chezscheme) '(rnrs) '(rnrs base) '(scheme) '(rnrs records syntactic)) 
        (let* ([collection (private-collect-identifiers index-node)]
            [predicator (find (lambda(identifier) (equal? (identifier-reference-type identifier) 'predicator)) collection)]
            [constructor (find (lambda(identifier) (equal? (identifier-reference-type identifier) 'constructor)) collection)]
            [getters (filter (lambda(identifier) (equal? (identifier-reference-type identifier) 'getter)) collection)]
            [setters (filter (lambda(identifier) (equal? (identifier-reference-type identifier) 'setter)) collection)])
          (map 
            (lambda (getter)
              (identifier-reference-type-expressions-set! 
                getter
                `((something? <- (inner:list? ,predicator)))))
            getters)
          (map 
            (lambda (setter)
              (identifier-reference-type-expressions-set! 
                setter
                `((void? <- (inner:list? ,predicator something?)))))
            setters)
          (identifier-reference-type-expressions-set! 
            predicator
            `((,(construct-type-expression-with-meta 'boolean?) <- (inner:list? something?))))
          (identifier-reference-type-expressions-set! 
            constructor 
            `((,predicator <- (inner:list? something? ...)))))]
      [else '()])
    substitutions))

(define (private-collect-identifiers index-node)
  (if (null? (index-node-references-export-to-other-node index-node))
    (apply append (map private-collect-identifiers (index-node-children index-node)))
    (index-node-references-export-to-other-node index-node)))
)