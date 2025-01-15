(library (scheme-langserver analysis type substitutions rules cond)
  (export cond-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (cond-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ clause **1)
          (apply 
            append 
            (map 
              (lambda (clause-index-node)
                (private-clause-process index-node clause-index-node))
              (cdr children)))]
        [else '()])
      (except c
        [else '()]))))

(define (private-clause-process root-index-node clause-index-node)
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
