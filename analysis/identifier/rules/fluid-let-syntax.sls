(library (scheme-langserver analysis identifier rules fluid-let-syntax)
  (export fluid-let-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules fluid-let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter variable syntax-variable let-loop
(define (fluid-let-syntax-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and bindings-list (((? index-node-symbol? identifier) :_ ...) **1)) . body)
      (fold-left 
        (lambda (exclude-list identifier-index-node)
          (let ([extended-exclude-list 
                  (append exclude-list (fluid-let-parameter-process index-node identifier-index-node index-node exclude-list document 'syntax-variable))])
            (index-node-excluded-references-set! bindings-list extended-exclude-list)
            extended-exclude-list))
        '()
        identifier)]
    [else '()]))
)
