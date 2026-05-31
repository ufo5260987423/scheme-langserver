(library (scheme-langserver analysis type substitutions rules application)
  (export application-process)
  (import 
    (chezscheme) 

    (scheme-langserver virtual-file-system index-node))

(define (application-process document index-node)
  (extend-index-node-substitution-list index-node (index-node-children index-node)))
)