(library (scheme-langserver analysis identifier rules define-syntax)
  (export 
    define-syntax-process
    define-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; For syntax-rules: value-node is the syntax-rules index-node itself.
; For syntax-case: value-node may be (lambda (x) (syntax-case x ...)) or
; the syntax-case index-node directly.  Walk into lambda when needed.
(define (private:find-generator value-index-node)
  (let ([expr (annotation-stripped (index-node-datum/annotations value-index-node))])
    (cond
      [(and (list? expr) (eq? 'syntax-rules (car expr)))
        (index-node-expansion-generator value-index-node)]
      [(and (list? expr) (eq? 'syntax-case (car expr)))
        (index-node-expansion-generator value-index-node)]
      [(and (list? expr) (eq? 'lambda (car expr)))
        (let* ([lambda-children (index-node-children value-index-node)]
            [body-list (cddr lambda-children)])
          (if (and (= (length body-list) 1)
                   (let ([body-expr (annotation-stripped (index-node-datum/annotations (car body-list)))])
                     (and (list? body-expr) (eq? 'syntax-case (car body-expr)))))
            (index-node-expansion-generator (car body-list))
            #f))]
      [else #f])))

(define (define-syntax:attach-generator root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [library-identifiers (get-nearest-ancestor-library-identifier index-node)]
      [expression (annotation-stripped ann)]
      [children (index-node-children index-node)])
    (match expression
      [(_ (? symbol? identifier) only-one) 
        (let ([generator (private:find-generator (car (reverse children)))])
          (if generator
            (map 
              (lambda (id)
                (identifier-reference-syntax-expander-set! id
                  (lambda x (apply generator x))))
              (index-node-references-export-to-other-node (cadr children)))
            '()))]
      [else '()])))
; reference-identifier-type include 
; syntax-parameter syntax-variable syntax parameter
(define (define-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [library-identifiers (get-nearest-ancestor-library-identifier index-node)]
      [expression (annotation-stripped ann)])
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
        (let* ([omg-index-node (dereference-index-node (cadr (index-node-children index-node)))]
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
              (let* ([dummy-index-node (dereference-index-node dummy-index-node)]
                  [dummy-ann (index-node-datum/annotations dummy-index-node)]
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
        (let* ([omg-index-node (dereference-index-node (cadr (index-node-children index-node)))]
            [reference (make-identifier-reference 
                identifier 
                document 
                (car (index-node-children omg-index-node))
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
          (let loop ([rest-node (dereference-index-node (cadr (index-node-children omg-index-node)))])
            (if (index-node-shared-reference rest-node)
              '()
              (let ([rest-expr (annotation-stripped (index-node-datum/annotations rest-node))])
                (cond 
                  [(pair? rest-expr) 
                    (let* ([dummy-index-node (dereference-index-node (car (index-node-children rest-node)))]
                        [dummy-expression (annotation-stripped (index-node-datum/annotations dummy-index-node))]
                        [dummy-reference (make-identifier-reference
                            dummy-expression
                            document
                            dummy-index-node
                            index-node
                            '()
                            'syntax-parameter
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
                      (index-node-excluded-references-set! 
                        omg-index-node
                        (append 
                          (index-node-excluded-references omg-index-node)
                          `(,dummy-reference))))
                    (let ([next-node (cadr (index-node-children rest-node))])
                      (if (index-node-shared-reference next-node)
                        '()
                        (loop (dereference-index-node next-node))))]
                  [(not (null? rest-expr)) 
                    (let ([rest-reference (make-identifier-reference 
                        rest-expr
                        document 
                        rest-node
                        index-node
                        '()
                        'parameter
                        '()
                        '())])
                      (index-node-references-export-to-other-node-set! 
                        (identifier-reference-index-node rest-reference)
                        (append 
                          (index-node-references-export-to-other-node (identifier-reference-index-node rest-reference))
                          `(,rest-reference)))
                      (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,rest-reference)))]
                  [else '()])))))]
      [else '()])))
) ; end library
