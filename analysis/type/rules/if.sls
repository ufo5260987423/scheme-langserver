(library (scheme-langserver analysis type rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type variable)
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
            (append 
              substitutions 
              (construct-substitutions-between-index-nodes substitutions index-node return-index-node0 '=)
              (construct-substitutions-between-index-nodes substitutions index-node return-index-node1 '=)
              (construct-substitutions-between-index-nodes substitutions return-index-node0 index-node '=)
              (construct-substitutions-between-index-nodes substitutions return-index-node1 index-node '=)))]
        [else substitutions])
      (except c
        [else substitutions]))))
)
