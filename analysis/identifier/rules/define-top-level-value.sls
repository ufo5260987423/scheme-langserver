(library (scheme-langserver analysis identifier rules define-top-level-value)
  (export define-top-level-value-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

; reference-identifier-type include 
; variable 
(define (define-top-level-value-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (? index-node-symbol? identifier-node) . dummy)
      (let ([reference (make-identifier-reference 
              (index-node-expression identifier-node)
              document 
              identifier-node
              index-node
              '()
              'variable 
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (document-ordered-reference-list-set!
          document
          (sort-identifier-references
            (append 
              (document-ordered-reference-list document)
              `(,reference)))))]
    [else '()]))
)
