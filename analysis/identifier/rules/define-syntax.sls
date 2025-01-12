(library (scheme-langserver analysis identifier rules define-syntax)
  (export define-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter syntax-variable
(define (define-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [library-identifiers (get-nearest-ancestor-library-identifier index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (? symbol? identifier) dummy ... ) 
          (let ([reference (make-identifier-reference 
                  (car* identifier)
                  document 
                  (cadr (index-node-children index-node))
                  index-node
                  library-identifiers
                  'syntax-variable
                  '()
                  '())])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))]
        [(_ ((? symbol? identifier) dummy0 ... ) dummy1 ... ) 
          (let* ([omg-index-node (cadr (index-node-children index-node))]
              [key-index-nodes (index-node-children omg-index-node)]
              [reference (make-identifier-reference 
                  identifier 
                  document 
                  (car key-index-nodes) 
                  index-node
                  library-identifiers
                  'syntax
                  '()
                  '())]
              [dummies (cdr key-index-nodes)])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
            (map 
              (lambda (dummy-index-node)
                (let* ([dummy-ann (index-node-datum/annotations dummy-index-node)]
                    [dummy-expression (annotation-stripped dummy-ann)]
                    [dummy-reference (make-identifier-reference
                        dummy-expression
                        document
                        dummy-index-node
                        index-node
                        '()
                        'syntax-parameter
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
        [(_ ((? symbol? identifier) . dummy0) dummy1 ... )
          (let* ([omg-index-node (cadr (index-node-children index-node))]
              [reference (make-identifier-reference 
                  identifier 
                  document 
                  omg-index-node
                  index-node
                  '()
                  'syntax
                  '()
                  '())])
            (index-node-references-export-to-other-node-set! 
              (identifier-reference-index-node reference)
              (append 
                (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                `(,reference)))
            (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
            (let loop ([rest dummy0])
              (cond 
                [(pair? rest) 
                  (let ([reference (make-identifier-reference 
                      (car rest)
                      document 
                      omg-index-node
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
                    (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))
                  (loop (cdr rest))]
                [(not (null? rest)) 
                  (let ([reference (make-identifier-reference 
                      rest
                      document 
                      omg-index-node
                      index-node
                      '()
                      'parameter
                      '()
                      '())])
                    (index-node-references-export-to-other-node-set! 
                      (identifier-reference-index-node reference)
                      (append 
                        (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                        `(,reference)))
                    (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))]
                [else '()])))]
        [else '()])
      (except c
        [else '()]))))

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
)
