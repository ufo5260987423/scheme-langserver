(library (scheme-langserver analysis identifier rules s7 lambda*)
  (export 
    lambda*-process
    parameter*-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; parameter 
(define (lambda*-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (identifier **1) fuzzy ... ) 
        (let loop ([rest (index-node-children (dereference-index-node (cadr (index-node-children index-node))))])
          (if (not (null? rest))
            (let* ([identifier-index-node (dereference-index-node (car rest))]
                [identifier-index-node-parent (index-node-parent identifier-index-node)])
              (parameter*-process index-node identifier-index-node index-node '() document)
              (loop (cdr rest)))))]
      [(_ (? symbol? identifier) fuzzy ... ) 
        (parameter*-process index-node (cadr (index-node-children index-node)) index-node '() document)]
      [(_ (identifier . rest) fuzzy ... ) 
        (let* ([omg-index-node (dereference-index-node (cadr (index-node-children index-node)))]
            [reference (make-identifier-reference 
                identifier 
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
          (append-references-into-ordered-references-for document index-node `(,reference))
          (let loop ([rest-node (dereference-index-node (cadr (index-node-children omg-index-node)))])
            (if (index-node-shared-reference rest-node)
              '()
              (let ([rest-expr (annotation-stripped (index-node-datum/annotations rest-node))])
                (cond 
                  [(pair? rest-expr) 
                    (let* ([identifier-index-node (dereference-index-node (car (index-node-children rest-node)))]
                        [reference (make-identifier-reference 
                            (annotation-stripped (index-node-datum/annotations identifier-index-node))
                            document 
                            identifier-index-node
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
                      (append-references-into-ordered-references-for document index-node `(,reference)))
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
      [else '()])))

(define (parameter*-process initialization-index-node index-node lambda-node exclude document )
  (let* ([index-node (dereference-index-node index-node)]
      [ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [identifier (cond
                    [(symbol? expression) expression]
                    [(pair? expression) (car expression)]
                    [else #f])])
    (if identifier
      (let ([reference 
            (make-identifier-reference
              identifier
              document
              index-node
              initialization-index-node
              '()
              'parameter
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          index-node
          (append 
            (index-node-references-export-to-other-node index-node)
            `(,reference)))

        (index-node-references-import-in-this-node-set! 
          lambda-node
          (sort-identifier-references 
            (append 
              (index-node-references-import-in-this-node lambda-node)
              `(,reference))))

        (index-node-excluded-references-set! 
          (index-node-parent index-node)
          (append 
            (index-node-excluded-references index-node)
            exclude
            `(,reference)))
        `(,reference))
      '())))
)
