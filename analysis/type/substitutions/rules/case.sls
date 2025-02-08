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
        [(_ (? symbol? expression) clause **1)
          (let* ([clauses (cddr children)]
              [clauses-children (map index-node-children clauses)]
              [previous (map car clauses-children)]
              [latters (map cadr clauses-children)]
              [expression-variable (index-node-variable (cadr children))])
            (append 
              (apply append 
                (map 
                  (lambda (local-expression)
                    (trivial-process document index-node expression-variable local-expression #f #f))
                  (map annotation-stripped (apply append (map index-node-datum/annotations (map index-node-children previous))))))
              (apply append (map (lambda (r) (construct-substitutions-between-index-nodes index-node r '=)) latters))))]
        [(_ expression clause **1)
          (let* ([clauses (cddr children)]
              [clauses-children (map index-node-children clauses)]
              [previous (map car clauses-children)]
              [latters (map cadr clauses-children)])
            (apply append (map (lambda (r) (construct-substitutions-between-index-nodes index-node r '=)) latters)))]
        [else '()])
      (except c
        [else '()]))))
)
