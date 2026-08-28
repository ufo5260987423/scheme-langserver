(library (scheme-langserver analysis identifier rules let-values)
  (export let-values-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; continuation
(define (let-values-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((or (? index-node-symbol? formals) ((? index-node-symbol? formals) **1)) init) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) (apply append (map (lambda (f) (if (list? f) f `(,f))) formals))))
      (for-each 
        (lambda (formals-group)
          (let ([formals-node (if (list? formals-group) (index-node-parent (car formals-group)) formals-group)]
                [refs (apply append 
                        (map 
                          (lambda (variable-index-node)
                            (let-parameter-process index-node variable-index-node index-node document 'continuation))
                          (if (list? formals-group) formals-group `(,formals-group))))])
            (index-node-excluded-references-set! formals-node refs)))
        formals)]
    [else '()]))
)
