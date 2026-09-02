(library (scheme-langserver analysis type substitutions rules do)
  (export do-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node))

(define (do-process document index-node)
  (match-index-node index-node
    [(:_ (bindings ...) (test . results) . body)
      (for-each private-process bindings)]
    [else '()]))

(define (private-process target-index-node)
  (match-index-node target-index-node
    [((? index-node-symbol? var) init)
      (extend-index-node-substitution-list var init)]
    [((? index-node-symbol? var) init update)
      (extend-index-node-substitution-list var init)
      (extend-index-node-substitution-list var update)]
    [else '()]))
)
