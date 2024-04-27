(library (scheme-langserver analysis identifier rules define-top-level-syntax)
  (export define-top-level-syntax-process)
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
; syntax-variable 
(define (define-top-level-syntax-process root-file-node root-library-node document index-node)
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
                  'syntax-variable 
                  '()
                  '())])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (document-reference-list-set!
              document
              (sort-identifier-references
                (append 
                  (document-reference-list document)
                  `(,reference)))))]
        [else '()])
      (except c
        [else '()]))))
)
