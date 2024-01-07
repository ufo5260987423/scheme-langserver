(library (scheme-langserver analysis dependency rules library-import)
  (export 
    library-import-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver util try)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (library-import-process index-node)
  (apply append 
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('library _ **1 ) (map match-import (index-node-children index-node))]
        [('define-library _ **1 ) (map match-import (index-node-children index-node))]
        [else (list (match-import index-node))]))))

(define (match-import index-node)
  (filter 
    (lambda (item) (not (null? item)))
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('import dummy **1 ) (map match-clause (index-node-children index-node))]
        [else '()]))))

(define (match-clause index-node)
  (filter 
    (lambda (item) (not (null? item)))
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression 
        [('only (identifier **1) _ ...) identifier]
        [('except (identifier **1) _ ...) identifier]
        [('prefix (identifier **1) _ ...) identifier]
        [('rename (identifier **1) _ ...) identifier]
        [(identifier **1) identifier]
        [else '()]))))
)