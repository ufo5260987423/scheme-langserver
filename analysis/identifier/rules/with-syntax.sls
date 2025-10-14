(library (scheme-langserver analysis identifier rules with-syntax)
  (export with-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)
    (scheme-langserver analysis identifier rules let-syntax)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (with-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [children (index-node-children index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (((? symbol? syntax-parameter) _ ...) **1) body ...) 
        (let* ([syntax-parameter-_s (cadr children)]
            [syntax-parameters (map car (map index-node-children (index-node-children syntax-parameter-_s)))])
          (map 
            (lambda (current-syntax-parameter-index-node)
              (let* ([expression (annotation-stripped (index-node-datum/annotations current-syntax-parameter-index-node))]
                  [identifier-reference (make-identifier-reference expression document current-syntax-parameter-index-node index-node '() 'syntax-parameter '() '())])
                (append-references-into-ordered-references-for document index-node `(, identifier-reference))
                (index-node-excluded-references-set! syntax-parameter-_s 
                  (append 
                    (index-node-excluded-references syntax-parameter-_s)
                    `(,identifier-reference)))))
            syntax-parameters))]
      [else '()])))
)
