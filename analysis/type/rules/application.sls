(library (scheme-langserver analysis type rules application)
  (export application-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (application-process document index-node substitutions)
  (let* ([variable (index-node-variable index-node)]
      [children (index-node-children index-node)])
    (if (null? children)
      substitutions
      (let* ([head-index-node (car children)]
          [reified-head-result (reify substitutions (index-node-variable head-index-node))]
          [filtered-lambdas (filter lambda? reified-head-result)])
        (if (null? filtered-lambdas)
          substitutions
          ;application rule
          (append 
            substitutions
            (map 
              (lambda (pair) (list (car pair) '= (cadr pair)))
              (cartesian-product 
                `(,variable)
                (map car filtered-lambdas)))))))))
)
