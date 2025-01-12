(library (scheme-langserver analysis identifier rules define-record-type)
  (export 
    define-record-type-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)
    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; getter setter constructor predicator syntax
(define (define-record-type-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [target-parent-index-node (index-node-parent index-node)])
    (try
      (match expression
        [(_ name-list) 
          (process-name-list index-node document target-parent-index-node (cadr (index-node-children index-node)) '())]
        [(_ (? symbol? name) (dummy ...) ... ) 
          (process-name-list index-node document target-parent-index-node (cadr (index-node-children index-node)) '())
          (process-define-record-type-tail index-node document target-parent-index-node (cddr (index-node-children index-node)) name) ]
        [(_ ((? symbol? name) dummy0 ...) (dummy1 ...) ... ) 
          (process-name-list index-node document target-parent-index-node (cadr (index-node-children index-node)) '())
          (process-define-record-type-tail index-node document target-parent-index-node (cddr (index-node-children index-node)) name)]
        [else '()])
      (except c
        [else '()]))))

(define (process-define-record-type-tail initialization-index-node document target-parent-index-node index-node-list name)
  (let loop ([body index-node-list])
    (if (not (null? body))
      (let* ([index-node (car body)]
          [ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)])
        (match expression
          [('fields _ **1) (process-fields-list initialization-index-node document target-parent-index-node index-node name '())]
          [('parent (? symbol? parent-name)) 
            (let loop ([references (find-available-references-for document index-node parent-name)])
              (if (not (null? references))
                (let* ([current-reference (car references)]
                    [binding-index-node (cadr (index-node-children index-node))]
                    [current-index-node (identifier-reference-index-node current-reference)]
                    [parent-index-node (index-node-parent current-index-node)]
                    [parent-children-index-node (index-node-children parent-index-node)]
                    [parent-ann (index-node-datum/annotations parent-index-node)]
                    [parent-expression (annotation-stripped parent-ann)]

                    [grand-parent-index-node (index-node-parent parent-index-node)]
                    [grand-parent-children-index-node (index-node-children grand-parent-index-node)])
                  (match parent-expression 
                    [('define-record-type name-list ('fields _ **1) dummy1 ...)
                      (map 
                        (lambda (index-node-tmp)
                          (process-fields-list initialization-index-node document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr parent-children-index-node))]
                    [else 
                      (map 
                        (lambda (index-node-tmp)
                          (process-fields-list initialization-index-node document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr grand-parent-children-index-node))])
                  (loop (cdr references)))))]
          [else '()])))))

(define (process-fields-list initialization-index-node document target-parent-index-node index-node record-name binding-index-node)
  (let loop ([children (cdr (index-node-children index-node))])
    (if (not (null? children))
      (let* ([current-index-node (car children)]
          [ann (index-node-datum/annotations current-index-node)]
          [expression (annotation-stripped ann)]
          [record-name-string (string-append (symbol->string record-name) "-")])
        (match expression 
          [('mutable (? symbol? name) (? symbol? name-get) (? symbol? name-set))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node (if (null? binding-index-node) (cadr current-children) binding-index-node)]
                [set-index-node (if (null? binding-index-node) (caddr current-children) binding-index-node)]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    initialization-index-node
                    '()
                    'getter
                    '()
                    '())]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-set)))
                    document
                    set-index-node
                    initialization-index-node
                    '()
                    'setter
                    '()
                    '())])
              (index-node-references-export-to-other-node-set!
                get-index-node
                (sort-identifier-references 
                  (append (index-node-references-export-to-other-node get-index-node) `(,get-identifier-reference))))
              (append-references-into-ordered-references-for 
                document
                target-parent-index-node
                `(,get-identifier-reference ,set-identifier-reference))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (sort-identifier-references 
                  (append (index-node-references-export-to-other-node set-index-node) `(,set-identifier-reference)))))]
          [('mutable (? symbol? name) (? symbol? name-get))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node (if (null? binding-index-node) (cadr current-children) binding-index-node)]
                [set-index-node name-index-node]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    initialization-index-node
                    '()
                    'getter
                    '()
                    '())]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-set!"))
                    document
                    set-index-node
                    initialization-index-node
                    '()
                    'setter
                    '()
                    '())])
              (index-node-references-export-to-other-node-set!
                get-index-node
                (sort-identifier-references
                  (append (index-node-references-export-to-other-node get-index-node) `(,get-identifier-reference))))
              (append-references-into-ordered-references-for 
                document
                target-parent-index-node
                `(,get-identifier-reference ,set-identifier-reference))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (append (index-node-references-export-to-other-node set-index-node) `(,set-identifier-reference))))]
          [('mutable (? symbol? name))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node name-index-node]
                [set-index-node name-index-node]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    get-index-node
                    initialization-index-node
                    '()
                    'getter
                    '()
                    '())]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-set!"))
                    document
                    set-index-node
                    initialization-index-node
                    '()
                    'setter
                    '()
                    '())])
              (index-node-references-export-to-other-node-set!
                get-index-node
                (sort-identifier-references
                  (append (index-node-references-export-to-other-node get-index-node) `(,get-identifier-reference))))
              (append-references-into-ordered-references-for 
                document
                target-parent-index-node
                `(,get-identifier-reference ,set-identifier-reference))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (sort-identifier-references
                  (append (index-node-references-export-to-other-node set-index-node) `(,set-identifier-reference)))))]
          [('immutable (? symbol? name) (? symbol? name-get))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node (if (null? binding-index-node) (cadr current-children) binding-index-node)]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    initialization-index-node
                    '()
                    'getter
                    '()
                    '())])
              (index-node-references-export-to-other-node-set!
                get-index-node
                (sort-identifier-references
                  (append (index-node-references-export-to-other-node get-index-node) `(,get-identifier-reference))))
              (append-references-into-ordered-references-for 
                document
                target-parent-index-node
                `(,get-identifier-reference)))]
          [('immutable (? symbol? name))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node name-index-node]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    get-index-node
                    initialization-index-node
                    '()
                    'getter
                    '()
                    '())])
              (index-node-references-export-to-other-node-set!
                get-index-node
                (sort-identifier-references
                  (append (index-node-references-export-to-other-node get-index-node) `(,get-identifier-reference))))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (sort-identifier-references
                  (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference)))))]
          [else '()])
        (loop (cdr children))))))

(define (process-name-list initialization-index-node document target-parent-index-node index-node predicator-parents)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression 
      [(? symbol? name) 
        (let* ([name-index-node index-node]
            [constructor-index-node index-node]
            [predicator-index-node index-node]
            [name-identifier-reference
              (make-identifier-reference 
                name
                document
                name-index-node
                initialization-index-node
                '()
                'syntax
                '()
                '())]
            [constructor-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append "make-" (symbol->string name)))
                document
                constructor-index-node
                initialization-index-node
                '()
                'constructor
                '()
                '())]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                initialization-index-node
                '()
                'predicator
                predicator-parents
                '())])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (append-references-into-ordered-references-for 
          document
          target-parent-index-node
          `(,name-identifier-reference ,constructor-identifier-reference ,predicator-identifier-reference))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference))))]
      [((? symbol? name))
        (let* ([children (index-node-children index-node)]
            [name-index-node (car children)]
            [constructor-index-node name-index-node]
            [predicator-index-node name-index-node]
            [name-identifier-reference
              (make-identifier-reference 
                name
                document
                name-index-node
                initialization-index-node
                '()
                'syntax
                '()
                '())]
            [constructor-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append "make-" (symbol->string name)))
                document
                constructor-index-node
                initialization-index-node
                '()
                'constructor
                '()
                '())]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                initialization-index-node
                '()
                'predicator
                predicator-parents
                '())])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (append-references-into-ordered-references-for 
          document
          target-parent-index-node
          `(,name-identifier-reference ,constructor-identifier-reference ,predicator-identifier-reference))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference))))]
      [((? symbol? name) (? symbol? constructor))
        (let* ([children (index-node-children index-node)]
            [name-index-node (car children)]
            [constructor-index-node (cadr children)]
            [predicator-index-node name-index-node]
            [name-identifier-reference
              (make-identifier-reference 
                name
                document
                name-index-node
                initialization-index-node
                '()
                'syntax
                '()
                '())]
            [constructor-identifier-reference
              (make-identifier-reference 
                constructor
                document
                constructor-index-node
                initialization-index-node
                '()
                'constructor
                '()
                '())]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                initialization-index-node
                '()
                'predicator
                predicator-parents
                '())])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (append-references-into-ordered-references-for 
          document
          target-parent-index-node
          `(,name-identifier-reference ,constructor-identifier-reference ,predicator-identifier-reference))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))

        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference))))]
      [((? symbol? name) (? symbol? constructor) (? symbol? predicator))
        (let* ([children (index-node-children index-node)]
            [name-index-node (car children)]
            [constructor-index-node (cadr children)]
            [predicator-index-node (caddr children)]
            [name-identifier-reference
              (make-identifier-reference 
                name
                document
                name-index-node
                initialization-index-node
                '()
                'syntax
                '()
                '())]
            [constructor-identifier-reference
              (make-identifier-reference 
                constructor
                document
                constructor-index-node
                initialization-index-node
                '()
                'constructor
                '()
                '())]
            [predicator-identifier-reference
              (make-identifier-reference 
                predicator
                document
                predicator-index-node
                initialization-index-node
                '()
                'predicator
                predicator-parents
                '())])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (append-references-into-ordered-references-for 
          document
          target-parent-index-node
          `(,name-identifier-reference ,constructor-identifier-reference ,predicator-identifier-reference))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference))))]
      [else '()])))
)