(library (scheme-langserver analysis type substitutions rules case)
  (export case-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (case-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ expression clause **1)
          (let* ([clauses (cddr children)]
              [clauses-children (map index-node-children clauses)]
              [previous-index-nodes (map car clauses-children)]
              [previous (map reverse clauses-children)]
              [latters (map cadr reverse)]
              [expression-node (cadr children)])
            (map 
              (lambda (t) (extend-index-node-substitution-list expression-node t))
              previous-index-nodes)
            (map 
              (lambda (t) (extend-index-node-substitution-list index-node t))
              latters))]
        [else '()])
      (except c
        [else '()]))))
)
