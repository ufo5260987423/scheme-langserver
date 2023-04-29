(library (scheme-langserver analysis type rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (if-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('if _  clause0) 
          (guard-for document index-node 'if '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let ([return-index-node (car (reverse children))])
            (append 
              substitutions 
              (construct-substitutions-between-index-nodes substitutions index-node return-index-node '=)
              (construct-substitutions-between-index-nodes substitutions return-index-node index-node '=)))]
        [('if _  clause0 clause1) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let (
              [return-index-node0 (cadr (reverse children))]
              [return-index-node1 (car (reverse children))])
            (fold-left
              add-to-substitutions
              substitutions 
              (append 
                (construct-substitutions-between-index-nodes substitutions index-node return-index-node0 '=)
                (construct-substitutions-between-index-nodes substitutions index-node return-index-node1 '=)
                (construct-substitutions-between-index-nodes substitutions return-index-node0 index-node '=)
                (construct-substitutions-between-index-nodes substitutions return-index-node1 index-node '=))))]
        [('cond clause **1)
          (guard-for document index-node 'cond '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (fold-left
            add-to-substitutions
            substitutions
            (apply 
              append 
              (map 
                (lambda (clause-index-node)
                  (private-clause-process substitutions index-node clause-index-node))
                (cdr children))))]
        [else substitutions])
      (except c
        [else substitutions]))))

(define (private-clause-process substitutions root-index-node clause-index-node)
  (let* ([ann (index-node-datum/annotations clause-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children clause-index-node)]
      [last-child (car (reverse children))])
    (match expression
      [(predicator tail **1) 
        (list 
          (construct-substitutions-between-index-nodes substitutions root-index-node last-child '=)
          (construct-substitutions-between-index-nodes substitutions last-child root-index-node '=)) ]
      [else '()])))
)
