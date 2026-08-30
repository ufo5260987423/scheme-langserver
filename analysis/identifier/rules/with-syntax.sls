(library (scheme-langserver analysis identifier rules with-syntax)
  (export with-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (with-syntax-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? syntax-parameter) . ignore) **1) . body)
      (let ([binding-list (index-node-parent (index-node-parent (car syntax-parameter)))])
        (check-duplicate-syntax-bindings document syntax-parameter)
        (map 
          (lambda (current-syntax-parameter-index-node)
            (let* ([expression (index-node-expression current-syntax-parameter-index-node)]
                [identifier-reference (make-identifier-reference expression document current-syntax-parameter-index-node index-node '() 'syntax-parameter '() '())])
              (append-references-into-ordered-references-for document index-node `(,identifier-reference))
              (index-node-excluded-references-set! binding-list
                (append 
                  (index-node-excluded-references binding-list)
                  `(,identifier-reference)))))
          syntax-parameter))]
    [else '()]))
)
