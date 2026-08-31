(library (scheme-langserver analysis identifier rules s7 define-macro)
  (export define-macro-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (define-macro-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and omg-index-node ((? index-node-symbol? name-node) . params)) . body)
      (let ([reference (make-identifier-reference 
                  (index-node-expression name-node)
                  document 
                  omg-index-node
                  index-node
                  '()
                  'procedure
                  '()
                  '())])
        (index-node-references-export-to-other-node-set! 
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
        (for-each
          (lambda (param-index-node)
            (let ([param-expression (index-node-expression param-index-node)])
              (if (symbol? param-expression)
                (let ([param-reference (make-identifier-reference 
                    param-expression
                    document 
                    omg-index-node
                    index-node
                    '()
                    'parameter
                    '()
                    '())])
                  (index-node-references-export-to-other-node-set! 
                    (identifier-reference-index-node param-reference)
                    (append 
                      (index-node-references-export-to-other-node (identifier-reference-index-node param-reference))
                      `(,param-reference)))
                  (append-references-into-ordered-references-for document index-node `(,param-reference))))))
          params)
        '())]
    [else '()])))
