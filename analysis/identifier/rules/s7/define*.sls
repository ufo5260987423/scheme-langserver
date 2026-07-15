(library (scheme-langserver analysis identifier rules s7 define*)
  (export define*-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter variable 
(define (define*-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ ((? symbol? identifier) dummy0 ... ) dummy1 ... ) 
        (let* ([omg-index-node (dereference-index-node (cadr (index-node-children index-node)))]
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
          (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
          (map 
            (lambda (dummy-index-node)
              (let* ([dummy-index-node (dereference-index-node dummy-index-node)]
                  [dummy-ann (index-node-datum/annotations dummy-index-node)]
                  [dummy-expression (annotation-stripped dummy-ann)])
                (match dummy-expression
                  [(? symbol? dummy-identifier)
                    (let ([dummy-reference 
                            (make-identifier-reference
                              dummy-expression
                              document
                              dummy-index-node
                              index-node
                              '()
                              'parameter
                              '()
                              '())])
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
                          `(,dummy-reference))))]
                  [((? symbol? dummy-identifier) :_)
                    (let ([dummy-reference 
                            (make-identifier-reference
                              (car dummy-expression)
                              document
                              dummy-index-node
                              index-node
                              '()
                              'parameter
                              '()
                              '())])
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
                          `(,dummy-reference))))]
                  [else '()])))
            dummies))]
      [(_ ((? symbol? identifier) . dummy0) dummy1 ... )
        (let* ([omg-index-node (dereference-index-node (cadr (index-node-children index-node)))]
            [reference (make-identifier-reference 
                identifier 
                document 
                (car (index-node-children omg-index-node))
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
          (let loop ([rest-node (dereference-index-node (cadr (index-node-children omg-index-node)))])
            (if (index-node-shared-reference rest-node)
              '()
              (let ([rest-expr (annotation-stripped (index-node-datum/annotations rest-node))])
                (cond 
                  [(pair? rest-expr) 
                    (let* ([dummy-index-node (dereference-index-node (car (index-node-children rest-node)))]
                        [dummy-expression (annotation-stripped (index-node-datum/annotations dummy-index-node))]
                        [reference (make-identifier-reference 
                            (if (symbol? dummy-expression)
                              dummy-expression
                              (car dummy-expression))
                            document 
                            dummy-index-node
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
                      (index-node-references-import-in-this-node-set!
                        index-node
                        (sort-identifier-references
                          (append 
                            (index-node-references-import-in-this-node index-node)
                            `(,reference))))
                      (index-node-excluded-references-set! 
                        omg-index-node
                        (append 
                          (index-node-excluded-references omg-index-node)
                          `(,reference))))
                    (let ([next-node (cadr (index-node-children rest-node))])
                      (if (index-node-shared-reference next-node)
                        '()
                        (loop (dereference-index-node next-node))))]
                  [(not (null? rest-expr)) 
                    (let ([reference (make-identifier-reference 
                        rest-expr
                        document 
                        rest-node
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
                      (append-references-into-ordered-references-for document index-node `(,reference)))]
                  [else '()])))))]
      [(_ (? symbol? identifier) dummy ... ) 
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
          (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))]
      [else '()])))
)
