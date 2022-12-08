(library (scheme-langserver analysis dependency rules library-import)
  (export 
    library-import-process
    is-library-identifiers?)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver util try)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (is-library-identifiers? document index-node)
  (try
    (let* ([parent (index-node-parent index-node)]
        [grand-parent (index-node-parent parent)])
      (cond 
        [(not (null? (match-import grand-parent))) 
          (guard-for document grand-parent 'import '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          #t]
        [(not (null? (match-import (index-node-parent grand-parent)))) 
          (guard-for document (index-node-parent grand-parent) 'import '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          #t]
        [else #f]))
    (except c [else #f])))

(define (library-import-process index-node)
  (apply append 
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('library _ **1 ) (map match-import (index-node-children index-node))]
        [else '()]))))

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