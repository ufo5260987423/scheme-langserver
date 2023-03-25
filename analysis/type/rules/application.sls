(library (scheme-langserver analysis type rules application)
  (export application-process)
  (import 
    (chezscheme) 

    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type util)
    (scheme-langserver analysis type variable)
    (scheme-langserver analysis type walk-engine)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (application-process document index-node substitutions)
  (let* ([variables (walk:index-node->single-variable-list substitutions index-node)]
      [children (index-node-children index-node)])
    (if (null? children)
      substitutions
      (let* ([head-index-node (car children)]
          [rest-index-nodes (cdr children)]
          [reified-head-result (reify substitutions head-index-node)]
          [filtered-lambdas (filter lambda? reified-head-result)])
        (if (null? filtered-lambdas)
          substitutions
          ;application rule
          (let ([applicated-substitution 
                (append 
                  substitutions
                  (map 
                    (lambda (pair) (list (car pair) '= (cadr pair)))
                    (cartesian-product variables (map car filtered-lambdas))))])
            ;gradule typing: unsafe convertion
            (lambda-templates->new-substitution-list 
              applicated-substitution 
              filtered-lambdas 
              rest-index-nodes)))))))
)
