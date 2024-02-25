(library (scheme-langserver analysis type substitutions rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (if-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('if _  clause0) 
          (guard-for document index-node 'if '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let ([condition-index-node (cadr children)]
              [return-index-node (car (reverse children))])
            `((,(index-node-variable condition-index-node) = something?))
            (construct-substitutions-between-index-nodes index-node return-index-node '=)
            (construct-substitutions-between-index-nodes return-index-node index-node '=))]
        [('if _  clause0 clause1) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let ([condition-index-node (cadr children)]
              [return-index-node0 (cadr (reverse children))]
              [return-index-node1 (car (reverse children))])
            (append
              `((,(index-node-variable condition-index-node) = something?))
              (construct-substitutions-between-index-nodes index-node return-index-node0 '=)
              (construct-substitutions-between-index-nodes index-node return-index-node1 '=)
              (construct-substitutions-between-index-nodes return-index-node0 index-node '=)
              (construct-substitutions-between-index-nodes return-index-node1 index-node '=)))]
        [('cond clause **1)
          (guard-for document index-node 'cond '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (apply 
            append 
            (map 
              (lambda (clause-index-node)
                (private-clause-process substitutions index-node clause-index-node))
              (cdr children)))]
        [('case (? symbol? expression) clause **1)
          (guard-for document index-node 'case '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([clauses (cddr children)]
              [clauses-children (map index-node-children clauses)]
              [previous (map car clauses-children)]
              [latters (map cadr clauses-children)]
              [expression-variable (index-node-variable (cadr children))])
            (append 
              (apply append 
                (map 
                  (lambda (local-expression)
                    (trivial-process document index-node expression-variable local-expression '() #f #f))
                  (map annotation-stripped (apply append (map index-node-datum/annotations (map index-node-children previous))))))
              (apply append (map (lambda (r) (construct-substitutions-between-index-nodes index-node r '=)) latters))))]
        [('case expression clause **1)
          (guard-for document index-node 'case '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([clauses (cddr children)]
              [clauses-children (map index-node-children clauses)]
              [previous (map car clauses-children)]
              [latters (map cadr clauses-children)])
            (apply append (map (lambda (r) (construct-substitutions-between-index-nodes index-node r '=)) latters)))]
        [('unless clause expression **1)
          (guard-for document index-node 'unless '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (construct-substitutions-between-index-nodes index-node (car (reverse children)) '=)]
        [else '()])
      (except c
        [else '()]))))

(define (private-clause-process substitutions root-index-node clause-index-node)
  (let* ([ann (index-node-datum/annotations clause-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children clause-index-node)]
      [first-child (car children)]
      [last-child (car (reverse children))])
    (match expression
      [(predicator tail **1) 
        (append
          `((,(index-node-variable first-child) = something?))
          (construct-substitutions-between-index-nodes root-index-node last-child '=)
          (construct-substitutions-between-index-nodes last-child root-index-node '=)) ]
      [else '()])))
)
