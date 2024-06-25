(library (scheme-langserver analysis identifier rules define-top-level-value)
  (export define-top-level-value-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; variable 
(define (define-top-level-value-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (? symbol? identifier) dummy ...) 
          (let ([reference (make-identifier-reference 
                  (car* identifier)
                  document 
                  (cadr (index-node-children index-node))
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
        [else '()])
      (except c
        [else '()]))))
)
