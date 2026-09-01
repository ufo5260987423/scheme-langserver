(library (scheme-langserver analysis dependency rules library-import)
  (export 
    library-import-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (library-import-process index-node)
  (apply append 
    (match-index-node index-node
      [('library . clauses) (map match-import clauses)]
      [else (list (match-import index-node))])))

(define (match-import index-node)
  (filter 
    (lambda (item) (not (null? item)))
    (match-index-node index-node
      [('import . clauses) (map match-clause clauses)]
      [else '()])))

(define (match-clause index-node)
  (filter 
    (lambda (item) (not (null? item)))
    (match-index-node index-node 
      [('only ((:= index-node-expression identifier) ...) . rest) identifier]
      [('except ((:= index-node-expression identifier) ...) . rest) identifier]
      [('prefix ((:= index-node-expression identifier) ...) . rest) identifier]
      [('rename ((:= index-node-expression identifier) ...) . rest) identifier]
      [('for ((:= index-node-expression identifier) ...) 'run . rest) identifier]
      [('for ((:= index-node-expression identifier) ...) ((:= index-node-expression (? (lambda (e) (equal? e '(meta 0))) :_))) . rest) identifier]
      [((:= index-node-expression identifier) ...) identifier]
      [else '()])))
)
