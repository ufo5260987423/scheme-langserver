(library (scheme-langserver analysis identifier rules define-record-type)
  (export define-record-type-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
(define (define-record-type-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [target-parent-index-node (index-node-parent index-node)])
    (match expression
      [('define-record-type name-list) 
      (pretty-print 1)
        (guard-for document index-node 'define-record-type '(chezscheme) '(rnrs) '(rnrs base) '(scheme) '(rnrs records syntactic)) 
      (pretty-print 2)
        (process-name-list document target-parent-index-node (cadr (index-node-children index-node)))]
      ; [('define-record-type (? symbol? name) (_ ...) **1 ) 
      ;   (guard-for index-node 'define-record-type '(chezscheme) '(rnrs) '(rnrs base) '(scheme)) 
      ;   (process-name-list document target-parent-index-node (cadr (index-node-children index-node)))
      ;   (process-define-record-type-tail document target-parent-index-node (cddr (index-node-children index-node)) name)]
      ; [('define-record-type ((? symbol? name) _ **1) (_ ...) **1 ) 
      ;   (guard-for index-node 'define-record-type '(chezscheme) '(rnrs) '(rnrs base) '(scheme)) 
      ;   (process-name-list document target-parent-index-node (cadr (index-node-children index-node)))
      ;   (process-define-record-type-tail document target-parent-index-node (cddr (index-node-children index-node)) name)]
      [else '()])))

(define (process-define-record-type-tail document target-parent-index-node index-node-list name)
  (let loop ([body index-node-list])
    (if (not (null? body))
      (let* ([index-node (car body)]
          [ann (index-node-datum/annotations index-node)]
          [expression (annotation-stripped ann)])
        (match expression
          [('fields _ **1) (process-fields-list document target-parent-index-node index-node name '())]
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
                          (process-fields-list document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr parent-children-index-node))]
                    ; [('define-record-type name-list dummy0 **1 ('fields _ **1) dummy1 ...)
                    ;   (map 
                    ;     (lambda (index-node-tmp)
                    ;       (process-fields-list document target-parent-index-node index-node-tmp name binding-index-node))
                    ;     (cddr parent-children-index-node))]
                    [else 
                      (map 
                        (lambda (index-node-tmp)
                          (process-fields-list document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr grand-parent-children-index-node))])
                  (loop (cdr references)))))]
          [else '()])))))

(define (process-fields-list document target-parent-index-node index-node record-name binding-index-node)
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
                [name-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    name-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-set)))
                    document
                    set-index-node
                    (get-nearest-ancestor-library-identifier index-node))])
              (index-node-references-export-to-other-node-set!
                name-index-node
                (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                get-index-node
                (append (index-node-references-export-to-other-node index-node) `(,get-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (append (index-node-references-export-to-other-node index-node) `(,set-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,set-identifier-reference))))]
          [('mutable (? symbol? name) (? symbol? name-get))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node (if (null? binding-index-node) (cadr current-children) binding-index-node)]
                [set-index-node name-index-node]
                [name-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    name-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-set"))
                    document
                    set-index-node
                    (get-nearest-ancestor-library-identifier index-node))])
              (index-node-references-export-to-other-node-set!
                name-index-node
                (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                get-index-node
                (append (index-node-references-export-to-other-node index-node) `(,get-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (append (index-node-references-export-to-other-node index-node) `(,set-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,set-identifier-reference))))]
          [('mutable (? symbol? name))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node name-index-node]
                [set-index-node name-index-node]
                [name-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    name-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-get"))
                    document
                    get-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [set-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-set"))
                    document
                    set-index-node
                    (get-nearest-ancestor-library-identifier index-node))])
              (index-node-references-export-to-other-node-set!
                name-index-node
                (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                get-index-node
                (append (index-node-references-export-to-other-node index-node) `(,get-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                set-index-node
                (append (index-node-references-export-to-other-node index-node) `(,set-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,set-identifier-reference))))]
          [('immutable (? symbol? name) (? symbol? name-get))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node (if (null? binding-index-node) (cadr current-children) binding-index-node)]
                [name-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    name-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name-get)))
                    document
                    get-index-node
                    (get-nearest-ancestor-library-identifier index-node))])
              (index-node-references-export-to-other-node-set!
                name-index-node
                (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                get-index-node
                (append (index-node-references-export-to-other-node index-node) `(,get-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference))))]
          [('immutable (? symbol? name))
            (let* ([current-children (cdr (index-node-children current-index-node))]
                [name-index-node (if (null? binding-index-node) (car current-children) binding-index-node)]
                [get-index-node name-index-node]
                [name-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name)))
                    document
                    name-index-node
                    (get-nearest-ancestor-library-identifier index-node))]
                [get-identifier-reference
                  (make-identifier-reference
                    (string->symbol (string-append record-name-string (symbol->string name) "-get"))
                    document
                    get-index-node
                    (get-nearest-ancestor-library-identifier index-node))])
              (index-node-references-export-to-other-node-set!
                name-index-node
                (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))

              (index-node-references-export-to-other-node-set!
                get-index-node
                (append (index-node-references-export-to-other-node index-node) `(,get-identifier-reference)))
              (index-node-references-import-in-this-node-set!
                target-parent-index-node
                (append (index-node-references-import-in-this-node target-parent-index-node) `(,get-identifier-reference))))]
          [else '()])
        (loop (cdr children))))))

(define (process-name-list document target-parent-index-node index-node)
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
                (get-nearest-ancestor-library-identifier name-index-node))]
            [constructor-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append "make-" (symbol->string name)))
                document
                constructor-index-node
                (get-nearest-ancestor-library-identifier index-node))]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                (get-nearest-ancestor-library-identifier index-node))])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,predicator-identifier-reference))))]
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
                (get-nearest-ancestor-library-identifier index-node))]
            [constructor-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append "make-" (symbol->string name)))
                document
                constructor-index-node
                (get-nearest-ancestor-library-identifier index-node))]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                (get-nearest-ancestor-library-identifier index-node))])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,predicator-identifier-reference))))]
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
                (get-nearest-ancestor-library-identifier index-node))]
            [constructor-identifier-reference
              (make-identifier-reference 
                constructor
                document
                constructor-index-node
                (get-nearest-ancestor-library-identifier index-node))]
            [predicator-identifier-reference
              (make-identifier-reference 
                (string->symbol (string-append (symbol->string name) "?"))
                document
                predicator-index-node
                (get-nearest-ancestor-library-identifier index-node))])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,constructor-identifier-reference)))

        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,predicator-identifier-reference))))]
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
                (get-nearest-ancestor-library-identifier index-node))]
            [constructor-identifier-reference
              (make-identifier-reference 
                constructor
                document
                constructor-index-node
                (get-nearest-ancestor-library-identifier index-node))]
            [predicator-identifier-reference
              (make-identifier-reference 
                predicator
                document
                predicator-index-node
                (get-nearest-ancestor-library-identifier index-node))])
        (index-node-references-export-to-other-node-set!
          name-index-node
          (append (index-node-references-export-to-other-node index-node) `(,name-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,name-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          constructor-index-node
          (append (index-node-references-export-to-other-node index-node) `(,constructor-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,constructor-identifier-reference)))
          
        (index-node-references-export-to-other-node-set!
          predicator-index-node
          (append (index-node-references-export-to-other-node index-node) `(,predicator-identifier-reference)))
        (index-node-references-import-in-this-node-set!
          target-parent-index-node
          (append (index-node-references-import-in-this-node target-parent-index-node) `(,predicator-identifier-reference))))]
      [else '()])))
)