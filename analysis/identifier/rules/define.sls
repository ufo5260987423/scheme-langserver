(library (scheme-langserver analysis identifier rules define)
  (export define-process)
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
; procedure parameter variable 
(define (define-process root-file-node root-library-node document index-node)
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
                  'procedure
                  '()
                  '())]
              [dummies (cdr key-index-nodes)])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (if (null? (index-node-parent index-node))
              (document-reference-list-set!
                document
                (sort-identifier-references
                  (append 
                    (document-reference-list document)
                    `(,reference))))
              (index-node-references-import-in-this-node-set! 
                (index-node-parent index-node) 
                (sort-identifier-references
                  (append 
                    (index-node-references-import-in-this-node (index-node-parent index-node))
                    `(,reference)))))
            (map 
              (lambda (dummy-index-node)
                (let* ([dummy-ann (index-node-datum/annotations dummy-index-node)]
                    [dummy-expression (annotation-stripped dummy-ann)]
                    [dummy-reference 
                      (make-identifier-reference
                        dummy-expression
                        document
                        dummy-index-node
                        index-node
                        '()
                        'parameter
                        '()
                        '())])
                  (match dummy-expression
                    [(? symbol? dummy-identifier)
                      (index-node-references-export-to-other-node-set!
                        (identifier-reference-index-node dummy-reference)
                        (append 
                          (index-node-references-export-to-other-node (identifier-reference-index-node dummy-reference))
                          `(,dummy-reference)))
                      (index-node-references-import-in-this-node-set!
                        index-node
                        (sort-identifier-references
                          (append 
                            (index-node-references-import-in-this-node index-node)
                            `(,dummy-reference))))

                      (index-node-excluded-references-set! 
                        omg-index-node
                        (append 
                          (index-node-excluded-references omg-index-node)
                          `(,dummy-reference)))]
                    [else '()])))
              dummies))]
        [(_ (? symbol? identifier) dummy ... ) 
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
            (if (null? (index-node-parent index-node))
              (document-reference-list-set!
                document
                (sort-identifier-references
                  (append 
                    (document-reference-list document)
                    `(,reference))))
              (index-node-references-import-in-this-node-set! 
                (index-node-parent index-node) 
                (sort-identifier-references
                  (append 
                    (index-node-references-import-in-this-node (index-node-parent index-node))
                    `(,reference))))))]
        [else '()])
      (except c
        [else '()]))))
)
