(library (scheme-langserver analysis identifier rules s7 define-macro)
  (export define-macro-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (define-macro-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ ((? symbol? identifier) dummy0 ... ) dummy1 ... ) 
          (let* ([omg-index-node (cadr (index-node-children index-node))]
            [key-index-nodes (index-node-children omg-index-node)]
            [reference (make-identifier-reference 
              identifier 
              document 
              (car key-index-nodes) 
              index-node
              '()
              'syntax-parameter
              '()
              '())])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))]
        [else '()])
      (except c
        [else '()]))))

)
