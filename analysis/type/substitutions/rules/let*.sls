(library (scheme-langserver analysis type substitutions rules let*)
  (export let*-process)
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

(define (let*-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (try
      (match expression
        [(_ (((? symbol? identifier) value) ... ) fuzzy **1 ) 
          (let* ([return-index-node (car (reverse children))]

              ;((? symbol? identifier) value ) index-nodes
              [key-value-index-nodes (index-node-children (cadr children))])
            (append 
              ;for let index-node
              (construct-substitutions-between-index-nodes index-node return-index-node '=)
              (construct-substitutions-between-index-nodes return-index-node index-node '=)
              ;for key value index-nodes
              (apply append (map (lambda (key-value-index-node) (let:private-process-key-value key-value-index-node)) key-value-index-nodes))))]
        [else '()])
      (except c
        [else '()]))))
)
