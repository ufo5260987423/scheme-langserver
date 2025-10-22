(library (scheme-langserver analysis type substitutions rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (if-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ condition clause0) 
        (let ([condition-index-node (cadr children)]
            [return-index-node (car (reverse children))])
          (extend-index-node-substitution-list condition-index-node 'something?)
          (extend-index-node-substitution-list index-node return-index-node)
          (extend-index-node-substitution-list return-index-node index-node))]
      [(_ condition clause0 clause1) 
        (let ([condition-index-node (cadr children)]
            [return-index-node0 (cadr (reverse children))]
            [return-index-node1 (car (reverse children))])
          (extend-index-node-substitution-list condition-index-node 'something?)
          (extend-index-node-substitution-list index-node return-index-node0)
          (extend-index-node-substitution-list index-node return-index-node1)
          (extend-index-node-substitution-list return-index-node0 index-node)
          (extend-index-node-substitution-list return-index-node1 index-node))]
      [else '()])))
)
