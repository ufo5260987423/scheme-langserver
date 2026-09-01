(library (scheme-langserver analysis identifier rules define-record-type)
  (export 
    define-record-type-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; getter setter constructor predicator syntax
(define (define-record-type-process root-file-node root-library-node document index-node)
  (let ([target-parent-index-node (index-node-parent index-node)])
    (match-index-node index-node
      [(:_ name-list) 
        (process-name-list index-node document target-parent-index-node name-list '())]
      [(:_ (? index-node-symbol? name-node) . clauses) 
        (process-name-list index-node document target-parent-index-node name-node '())
        (process-define-record-type-tail index-node document target-parent-index-node clauses (index-node-expression name-node))]
      [(:_ ((? index-node-symbol? name-node) . _) . clauses) 
        (let ([name-list (cadr (index-node-children index-node))])
          (process-name-list index-node document target-parent-index-node name-list '())
          (process-define-record-type-tail index-node document target-parent-index-node clauses (index-node-expression name-node)))]
      [else '()])))

(define (process-define-record-type-tail initialization-index-node document target-parent-index-node index-node-list name)
  (let loop ([body index-node-list])
    (if (not (null? body))
      (let ([index-node (dereference-index-node (car body))])
        (match-index-node index-node
          [('fields :_ **1) 
            (process-fields-list initialization-index-node document target-parent-index-node index-node name '())
            (loop (cdr body))]
          [('parent (? index-node-symbol? parent-name-node)) 
            (let loop2 ([references (find-available-references-for document index-node (index-node-expression parent-name-node))])
              (if (not (null? references))
                (let* ([current-reference (car references)]
                    [binding-index-node parent-name-node]
                    [current-index-node (identifier-reference-index-node current-reference)]
                    [parent-index-node (index-node-parent current-index-node)]
                    [parent-children-index-node (index-node-children parent-index-node)]
                    [parent-expression (index-node-expression parent-index-node)]

                    [grand-parent-index-node (index-node-parent parent-index-node)]
                    [grand-parent-children-index-node (index-node-children grand-parent-index-node)])
                  (match-index-node parent-index-node
                    [('define-record-type name-list ('fields :_ **1) . dummy1)
                      (map 
                        (lambda (index-node-tmp)
                          (process-fields-list initialization-index-node document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr parent-children-index-node))]
                    [else 
                      (map 
                        (lambda (index-node-tmp)
                          (process-fields-list initialization-index-node document target-parent-index-node index-node-tmp name binding-index-node))
                        (cddr grand-parent-children-index-node))])
                  (loop2 (cdr references)))))
            (loop (cdr body))]
          [else (loop (cdr body))])))))

(define (private:regist-field-references! initialization-index-node document target-parent-index-node get-index-node set-index-node record-name get-name set-name)
  (let* ([record-name-string (string-append (symbol->string record-name) "-")]
      [get-identifier-reference
        (make-identifier-reference
          (string->symbol (string-append record-name-string (symbol->string get-name)))
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
    (let ([references
            (if set-name
              (let ([set-identifier-reference
                      (make-identifier-reference
                        (string->symbol (string-append record-name-string (symbol->string set-name)))
                        document
                        set-index-node
                        initialization-index-node
                        '()
                        'setter
                        '()
                        '())])
                (index-node-references-export-to-other-node-set!
                  set-index-node
                  (append (index-node-references-export-to-other-node set-index-node) `(,set-identifier-reference)))
                `(,get-identifier-reference ,set-identifier-reference))
              `(,get-identifier-reference))])
      (append-references-into-ordered-references-for document target-parent-index-node references))))

(define (process-fields-list initialization-index-node document target-parent-index-node index-node record-name binding-index-node)
  (let loop ([children (cdr (index-node-children (dereference-index-node index-node)))])
    (if (not (null? children))
      (let ([current-index-node (dereference-index-node (car children))])
        (match-index-node current-index-node
          [('mutable (? index-node-symbol? name-node) (? index-node-symbol? get-node) (? index-node-symbol? set-node))
            (let ([name-index-node (if (null? binding-index-node) name-node binding-index-node)])
              (private:regist-field-references! initialization-index-node document target-parent-index-node
                (if (null? binding-index-node) get-node binding-index-node)
                (if (null? binding-index-node) set-node binding-index-node)
                record-name
                (index-node-expression get-node)
                (index-node-expression set-node)))]
          [('mutable (? index-node-symbol? name-node) (? index-node-symbol? get-node))
            (let ([name-index-node (if (null? binding-index-node) name-node binding-index-node)])
              (private:regist-field-references! initialization-index-node document target-parent-index-node
                (if (null? binding-index-node) get-node binding-index-node)
                name-index-node
                record-name
                (index-node-expression get-node)
                (string->symbol (string-append (symbol->string (index-node-expression name-node)) "-set!"))))]
          [('mutable (? index-node-symbol? name-node))
            (let ([name-index-node (if (null? binding-index-node) name-node binding-index-node)])
              (private:regist-field-references! initialization-index-node document target-parent-index-node
                name-index-node
                name-index-node
                record-name
                (index-node-expression name-node)
                (string->symbol (string-append (symbol->string (index-node-expression name-node)) "-set!"))))]
          [('immutable (? index-node-symbol? name-node) (? index-node-symbol? get-node))
            (private:regist-field-references! initialization-index-node document target-parent-index-node
              (if (null? binding-index-node) get-node binding-index-node)
              #f
              record-name
              (index-node-expression get-node)
              #f)]
          [('immutable (? index-node-symbol? name-node))
            (let ([name-index-node (if (null? binding-index-node) name-node binding-index-node)])
              (private:regist-field-references! initialization-index-node document target-parent-index-node
                name-index-node
                #f
                record-name
                (index-node-expression name-node)
                #f))]
          [else '()])
        (loop (cdr children))))))

(define (private:regist-name-references! initialization-index-node document target-parent-index-node base-index-node name-node constructor-node predicator-node predicator-parents)
  (let* ([name (index-node-expression name-node)]
      [constructor-name
        (if constructor-node
          (index-node-expression constructor-node)
          (string->symbol (string-append "make-" (symbol->string name))))]
      [predicator-name
        (if predicator-node
          (index-node-expression predicator-node)
          (string->symbol (string-append (symbol->string name) "?")))]
      [constructor-target-node (or constructor-node name-node)]
      [predicator-target-node (or predicator-node name-node)]
      [name-identifier-reference
        (make-identifier-reference 
          name
          document
          name-node
          initialization-index-node
          '()
          'syntax
          '()
          '())]
      [constructor-identifier-reference
        (make-identifier-reference 
          constructor-name
          document
          constructor-target-node
          initialization-index-node
          '()
          'constructor
          '()
          '())]
      [predicator-identifier-reference
        (make-identifier-reference 
          predicator-name
          document
          predicator-target-node
          initialization-index-node
          '()
          'predicator
          predicator-parents
          '())])
    (index-node-references-export-to-other-node-set!
      name-node
      (append (index-node-references-export-to-other-node base-index-node) `(,name-identifier-reference)))
    (index-node-references-export-to-other-node-set!
      constructor-target-node
      (append (index-node-references-export-to-other-node base-index-node) `(,constructor-identifier-reference)))
    (index-node-references-export-to-other-node-set!
      predicator-target-node
      (append (index-node-references-export-to-other-node base-index-node) `(,predicator-identifier-reference)))
    (append-references-into-ordered-references-for 
      document
      target-parent-index-node
      `(,name-identifier-reference ,constructor-identifier-reference ,predicator-identifier-reference))))

(define (process-name-list initialization-index-node document target-parent-index-node index-node predicator-parents)
  (match-index-node index-node
    [(? index-node-symbol? name-node)
      (private:regist-name-references! initialization-index-node document target-parent-index-node index-node name-node #f #f predicator-parents)]
    [((? index-node-symbol? name-node))
      (private:regist-name-references! initialization-index-node document target-parent-index-node index-node name-node #f #f predicator-parents)]
    [((? index-node-symbol? name-node) (? index-node-symbol? constructor-node))
      (private:regist-name-references! initialization-index-node document target-parent-index-node index-node name-node constructor-node #f predicator-parents)]
    [((? index-node-symbol? name-node) (? index-node-symbol? constructor-node) (? index-node-symbol? predicator-node))
      (private:regist-name-references! initialization-index-node document target-parent-index-node index-node name-node constructor-node predicator-node predicator-parents)]
    [else '()])))

