(library (scheme-langserver analysis type substitutions rules case)
  (export case-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (case-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ expression clause **1)
        (let* ([clauses (cddr children)]
            [clauses-children (map index-node-children clauses)]
            [previous-index-nodes (map car clauses-children)]
            [last-index-nodes (map (lambda (c) (car (reverse c))) clauses-children)]
            [expression-node (cadr children)])
          (for-each 
            (lambda (t) (extend-index-node-substitution-list expression-node t))
            previous-index-nodes)
          (for-each 
            (lambda (t) (extend-index-node-substitution-list index-node t))
            last-index-nodes)
          (for-each 
            (lambda (t) (extend-index-node-substitution-list t index-node))
            last-index-nodes))]
      [else '()])))
)
