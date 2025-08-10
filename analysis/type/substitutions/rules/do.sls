(library (scheme-langserver analysis type substitutions rules do)
  (export do-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (do-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ ((var init update ...) **1) (test result ...) _ ... ) 
          (let* ([children (index-node-children index-node)]
              [var-index-node (cadr children)])
            (map private-process (index-node-children var-index-node)))]
        [else '()])
      (except c
        [else '()]))))

(define (private-process target-index-node)
  (let* ([ann (index-node-datum/annotations target-index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children target-index-node)])
    (match expression
      [((? symbol? var) init)
        (let* ([var-index-node (car children)]
            [init-index-node (cadr children)])
          (extend-index-node-substitution-list var-index-node init-index-node))]
      [((? symbol? var) init update)
        (let* ([var-index-node (car children)]
            [init-index-node (cadr children)]
            [update-index-node (caddr (children))])
          (extend-index-node-substitution-list var-index-node init-index-node)
          (extend-index-node-substitution-list var-index-node update-index-node))]
      [else '()])))
)