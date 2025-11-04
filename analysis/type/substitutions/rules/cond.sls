(library (scheme-langserver analysis type substitutions rules cond)
  (export cond-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (cond-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ clause **1)
        (map 
          (lambda (clause-index-node) (private-clause-process index-node clause-index-node))
          (cdr children))]
      [else '()])))

(define (private-clause-process root-index-node clause-index-node)
  (let* ([ann (index-node-datum/annotations clause-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children clause-index-node)]
      [first-child (car children)]
      [last-child (car (reverse children))])
    (match expression
      [(predicator tail **1) 
        (extend-index-node-substitution-list first-child 'something?)
        (extend-index-node-substitution-list root-index-node last-child)
        (extend-index-node-substitution-list last-child root-index-node)]
      [else '()])))
)
