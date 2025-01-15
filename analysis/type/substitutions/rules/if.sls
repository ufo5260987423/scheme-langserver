(library (scheme-langserver analysis type substitutions rules if)
  (export if-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (if-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ condition clause0) 
          (let ([condition-index-node (cadr children)]
              [return-index-node (car (reverse children))])
            (append 
              `((,(index-node-variable condition-index-node) = something?))
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)))]
        [(_ condition clause0 clause1) 
          (let ([condition-index-node (cadr children)]
              [return-index-node0 (cadr (reverse children))]
              [return-index-node1 (car (reverse children))])
            (append
              `((,(index-node-variable condition-index-node) = something?))
              (construct-substitutions-between-index-nodes index-node return-index-node0 '=)
              (construct-substitutions-between-index-nodes index-node return-index-node1 '=)
              (construct-substitutions-between-index-nodes return-index-node0 index-node '=)
              (construct-substitutions-between-index-nodes return-index-node1 index-node '=)))]
        [else '()])
      (except c
        [else '()]))))
)
