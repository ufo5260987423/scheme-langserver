(library (scheme-langserver analysis type substitutions rules letrec)
  (export letrec-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis type substitutions rules let))

(define (letrec-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ (((? symbol? identifier) value ) ... ) fuzzy **1) 
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])

            (extend-index-node-substitution-list index-node return-index-node)
            (extend-index-node-substitution-list return-index-node index-node)
            (map let:private-process-key-value key-value-index-nodes))]
        [else '()])
      (except c
        [else '()]))))
)
