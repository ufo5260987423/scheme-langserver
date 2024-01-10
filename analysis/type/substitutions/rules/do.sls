(library (scheme-langserver analysis type substitutions rules do)
  (export do-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

(define (do-process document index-node substitutions)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [('do ((var init update ...) **1) (test result ...) _ ... ) 
          (guard-for document index-node 'do '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([children (index-node-children index-node)]
              [var-index-node (cadr children)])
            (apply append (map private-process (index-node-children var-index-node))))]
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
          [var-variable (index-node-variable var-index-node)]
          [init-index-node (cadr children)]
          [init-index-variable (index-node-variable init-index-node)])
          `((,var-variable = ,init-index-variable)))]
      [((? symbol? var) init update)
        (let* ([var-index-node (car children)]
          [var-variable (index-node-variable var-index-node)]
          [init-index-node (cadr children)]
          [init-index-variable (index-node-variable init-index-node)]
          [update-index-node (caddr (children))]
          [update-index-variable (index-node-variable update-index-node)])
          `((,var-variable = ,init-index-variable)
            (,var-variable = ,update-index-variable)))]
      [else '()])))
)