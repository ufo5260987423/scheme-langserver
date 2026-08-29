(library (scheme-langserver analysis identifier rules library-import)
  (export 
    library-import-process
    invoke-library-process
    import-process
    import-references
    import-from-external-index-node
    process-library-identifier-excluded-references
    resolve-import-library-identifier)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; pointer 
(define (library-import-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ library-identifiers . import-things)
      (map 
        (lambda (child-node) (match-import index-node root-file-node root-library-node document child-node))
        import-things)]
    [else '()])
  index-node)

(define (invoke-library-process root-file-node root-library-node document index-node)
  (filter-empty-list 
    (let ([parent-index-node (index-node-parent index-node)])
      (match-index-node index-node
        [(:_ quote-node . fuzzy)
          (let ([quote-expression (index-node-expression quote-node)])
            (if (and (list? quote-expression) (eq? 'quote (car quote-expression)))
              (append-references-into-ordered-references-for 
                document 
                parent-index-node 
                (filter identifier-reference? (import-references document root-library-node (cadr quote-expression))))
              '()))]
        [else '()]))))

(define (import-process root-file-node root-library-node document index-node)
  (let* ([biggest (get-biggest-sibling index-node)]
      [flag (if (not biggest) #t (not (meta-for? document biggest 'library)))])
    (if flag 
      (filter-empty-list 
        (match-index-node index-node
          [('import . clauses)
            (map 
              (lambda (child-node) (match-clause index-node root-file-node root-library-node document child-node)) 
              clauses)]
          [else '()]))
      '())))

(define process-library-identifier-excluded-references 
  (case-lambda 
    [(document) 
      (map 
        (lambda (index-node)
          (process-library-identifier-excluded-references document index-node 0))
        (document-index-node-list document))]
    [(document index-node depth) 
      (if (library-identifier? document index-node)
        (index-node-excluded-references-set! index-node (find-available-references-for document index-node))
        (if (< depth 3)
          (map 
            (lambda (current-index-node)
              (process-library-identifier-excluded-references document current-index-node (+ 1 depth)))
            (index-node-children index-node))
          '()))]))

(define (filter-empty-list list-instance)
  (filter 
    (lambda (item) (not (null? item)))
    list-instance))

(define (private:append-identifier-not-exported-diagnose document index-node identifier)
  (append-new-diagnoses document
    `(,(index-node-start index-node) ,(index-node-end index-node) 2
      ,(string-append "Identifier not exported: " (symbol->string identifier))
      "import" "identifier-not-exported")))

; Recursively extract the base library identifier from a nested import spec.
; E.g. (rename (except (rnrs) find filter) (assoc r6rs:assoc)) -> (rnrs)
(define (resolve-import-library-identifier expression)
  (if (and (list? expression) (not (null? expression))
           (memq (car expression) '(only except prefix rename alias for)))
    (if (and (list? (cdr expression)) (not (null? (cdr expression))))
      (resolve-import-library-identifier (cadr expression))
      expression)
    expression))

(define (match-import initialization-index-node root-file-node root-library-node document index-node)
  (filter-empty-list 
    (match-index-node index-node
      [('import . clauses)
        (map 
          (lambda (child-node) (match-clause initialization-index-node root-file-node root-library-node document child-node)) 
          clauses)]
      [else '()])))

(define (symbol-children? children)
  (or (null? children)
      (and (index-node-symbol? (car children))
           (symbol-children? (cdr children)))))

(define (match-clause initialization-index-node root-file-node root-library-node document index-node)
  (let* ([grand-parent-index-node (index-node-parent (index-node-parent index-node))]
      [scope-index-node
        (if (and (not (null? initialization-index-node))
              (let ([expr (index-node-expression initialization-index-node)])
                (and (list? expr) (not (null? expr)) (memq (car expr) '(library define-library)))))
          initialization-index-node
          grand-parent-index-node)]
      [children (index-node-children index-node)])
    (match-index-node index-node
      [('only ((? index-node-symbol? library-identifier) ...) . identifier-index-nodes)
        (if (symbol-children? identifier-index-nodes)
          (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
            [library-identifier-node (car (cdr children))])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (let loop ([importion-index-node identifier-index-nodes]
            [imported-references 
              (filter
                (lambda (reference) 
                  (find (lambda(id) (eq? id (identifier-reference-identifier reference))) (map index-node-expression identifier-index-nodes)))
                (import-references document root-library-node actual-library-identifier))])

          (if (not (null? importion-index-node))
            (let* ([current-index-node (car importion-index-node)]
                [current-identifier (index-node-expression current-index-node)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (eq? current-identifier (identifier-reference-identifier reference)))
                    imported-references)])
              (when (and (or (not (null? (walk-library actual-library-identifier root-library-node)))
                             (meta-library? actual-library-identifier 'r6rs))
                         (null? current-references))
                (private:append-identifier-not-exported-diagnose document current-index-node current-identifier))


              (append-references-into-ordered-references-for document current-index-node current-references)

              (append-references-into-ordered-references-for document scope-index-node current-references)

              (loop 
                (cdr importion-index-node) 
                (filter
                  (lambda (reference) 
                    (not (eq? current-identifier (identifier-reference-identifier reference))))
                  imported-references))))))
          '())]
      [('except ((? index-node-symbol? library-identifier) ...) . identifier-index-nodes)
        (if (symbol-children? identifier-index-nodes)
          (let* ([library-identifier-expression (map index-node-expression library-identifier)]
              [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
              [library-identifier-node (car (cdr children))])
            (if (null? (walk-library actual-library-identifier root-library-node))
              (if (not (meta-library? actual-library-identifier 'r6rs))
                (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
              (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

          (let ([tmp 
                (filter
                  (lambda (reference) 
                    (find (lambda(id) (not (eq? id (identifier-reference-identifier reference)))) (map index-node-expression identifier-index-nodes)))
                  (import-references document root-library-node actual-library-identifier))])
            (if (null? scope-index-node)
              (document-ordered-reference-list-set! 
                document
                (sort-identifier-references (append (document-ordered-reference-list document) tmp)))
              (append-references-into-ordered-references-for document scope-index-node tmp)))

          (let loop ([importion-index-node identifier-index-nodes]
              [imported-references 
                (filter
                  (lambda (reference) 
                    (find (lambda(id) (eq? id (identifier-reference-identifier reference))) (map index-node-expression identifier-index-nodes)))
                  (import-references document root-library-node actual-library-identifier))])
            (if (not (null? importion-index-node))
              (let* ([current-index-node (car importion-index-node)]
                  [current-identifier (index-node-expression current-index-node)]
                  [current-references 
                    (filter
                      (lambda (reference) 
                        (eq? current-identifier (identifier-reference-identifier reference)))
                      imported-references)])
                (when (and (or (not (null? (walk-library actual-library-identifier root-library-node)))
                               (meta-library? actual-library-identifier 'r6rs))
                           (null? current-references))
                  (private:append-identifier-not-exported-diagnose document current-index-node current-identifier))


                (append-references-into-ordered-references-for document current-index-node current-references)
                (loop 
                  (cdr importion-index-node) 
                  (filter
                    (lambda (reference) 
                      (not (eq? current-identifier (identifier-reference-identifier reference))))
                    imported-references))))))
          '())]
      [('prefix ((? index-node-symbol? library-identifier) ...) (? index-node-symbol? prefix-id))
        (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
            [library-identifier-node (car (cdr children))]
            [prefix-id-expression (index-node-expression prefix-id)])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (let* ([imported-references (import-references document root-library-node actual-library-identifier)]
            [prefixed-references 
              (map 
                (lambda (reference) 
                  (make-identifier-reference
                    (string->symbol (string-append (symbol->string prefix-id-expression) (symbol->string (identifier-reference-identifier reference))))
                    (identifier-reference-document reference)
                    (identifier-reference-index-node reference)
                    initialization-index-node 
                    (identifier-reference-library-identifier reference)
                    'pointer
                    `(,reference)
                    (identifier-reference-type-expressions reference))) 
                imported-references)])
          (append-references-into-ordered-references-for document scope-index-node prefixed-references)))]
      [('rename ((? index-node-symbol? library-identifier) ...) . rename-pairs)
        (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
            [library-identifier-node (car (cdr children))])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (let loop ([importion-nodes rename-pairs]
            [imported-references 
              (filter
                (lambda (reference) 
                  (find 
                    (lambda (id) (eq? id (identifier-reference-identifier reference)))
                    (map 
                      (lambda (rename-pair-index-node)
                        (index-node-expression (car (index-node-children rename-pair-index-node))))
                      rename-pairs)))
                (import-references document root-library-node actual-library-identifier))])
          (if (not (null? importion-nodes))
            (let* ([current-importion-pair (index-node-children (car importion-nodes))]
                [current-external-node (car current-importion-pair)]
                [current-internal-node (cadr current-importion-pair)]
                [current-external-name (index-node-expression current-external-node)]
                [current-internal-name (index-node-expression current-internal-node)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (eq? current-external-name (identifier-reference-identifier reference)))
                    imported-references)]
                [renamed-references 
                  (map 
                    (lambda (reference)
                      (make-identifier-reference
                        current-internal-name
                        (identifier-reference-document reference)
                        (identifier-reference-index-node reference)
                        initialization-index-node 
                        (identifier-reference-library-identifier reference)
                        'pointer
                        `(,reference)
                        (identifier-reference-type-expressions reference))) 
                    current-references)])
              (when (and (or (not (null? (walk-library actual-library-identifier root-library-node)))
                             (meta-library? actual-library-identifier 'r6rs))
                         (null? current-references))
                (private:append-identifier-not-exported-diagnose document current-external-node current-external-name))


              (append-references-into-ordered-references-for document current-internal-node current-references)
              (append-references-into-ordered-references-for document current-internal-node renamed-references)
              (append-references-into-ordered-references-for document scope-index-node renamed-references)

              (index-node-references-export-to-other-node-set! 
                current-external-node
                (append 
                  (index-node-references-import-in-this-node current-external-node)
                  renamed-references))
              (loop 
                (cdr importion-nodes)
                (filter
                  (lambda (reference) 
                    (not (eq? current-external-name (identifier-reference-identifier reference))))
                  imported-references))))))]
      [('alias ((? index-node-symbol? library-identifier) ...) . rename-pairs)
        (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
            [library-identifier-node (car (cdr children))])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (let loop ([importion-nodes rename-pairs]
            [imported-references 
              (filter
                (lambda (reference) 
                  (find 
                    (lambda (id) (eq? id (identifier-reference-identifier reference)))
                    (map 
                      (lambda (rename-pair-index-node)
                        (index-node-expression (car (index-node-children rename-pair-index-node))))
                      rename-pairs)))
                (import-references document root-library-node actual-library-identifier))])
          (if (not (null? importion-nodes))
            (let* ([current-importion-pair (index-node-children (car importion-nodes))]
                [current-external-node (car current-importion-pair)]
                [current-internal-node (cadr current-importion-pair)]
                [current-external-name (index-node-expression current-external-node)]
                [current-internal-name (index-node-expression current-internal-node)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (eq? current-external-name (identifier-reference-identifier reference)))
                    imported-references)]
                [renamed-references 
                  (map 
                    (lambda (reference)
                      (make-identifier-reference
                        current-internal-name
                        (identifier-reference-document reference)
                        (identifier-reference-index-node reference)
                        initialization-index-node 
                        (identifier-reference-library-identifier reference)
                        'pointer
                        `(,reference)
                        (identifier-reference-type-expressions reference))) 
                    current-references)])
              (when (and (or (not (null? (walk-library actual-library-identifier root-library-node)))
                             (meta-library? actual-library-identifier 'r6rs))
                         (null? current-references))
                (private:append-identifier-not-exported-diagnose document current-external-node current-external-name))


              (append-references-into-ordered-references-for document current-internal-node current-references)
              (append-references-into-ordered-references-for document current-internal-node renamed-references)
              (append-references-into-ordered-references-for document scope-index-node current-references)
              (append-references-into-ordered-references-for document scope-index-node renamed-references)

              (index-node-references-export-to-other-node-set! 
                current-external-node
                (append 
                  (index-node-references-import-in-this-node current-external-node)
                  renamed-references))
              (loop 
                (cdr importion-nodes)
                (filter
                  (lambda (reference) 
                    (not (eq? current-external-name (identifier-reference-identifier reference))))
                  imported-references))))))]
      [('for ('only ((? index-node-symbol? library-identifier) ...) . (? symbol-children? identifier-index-nodes)) import-level)
        (let ([import-level-expression (index-node-expression import-level)])
          (if (or
              (eq? 'run import-level-expression)
              (equal? '(meta 0) import-level-expression)
              (eq? 'expand import-level-expression)
              ; (equal? '(meta 1) import-level-expression)
              )
            (match-clause initialization-index-node root-file-node root-library-node document (cadr children))))]
      [('for ((? index-node-symbol? library-identifier) ...) import-level)
        (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)]
            [import-level-expression (index-node-expression import-level)]
            [library-identifier-node (cadr children)])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! library-identifier-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (if (or
            (eq? 'run import-level-expression)
            (equal? '(meta 0) import-level-expression)
            (eq? 'expand import-level-expression)
            ; (equal? '(meta 1) import-level-expression)
            )
          (let ([tmp (filter identifier-reference? (import-references document root-library-node actual-library-identifier))])
            (if (null? scope-index-node)
              (document-ordered-reference-list-set! 
                document
                (sort-identifier-references (append (document-ordered-reference-list document) tmp)))
              (append-references-into-ordered-references-for document scope-index-node tmp)))))]
      [((? index-node-symbol? library-identifier) ...)
        (let* ([library-identifier-expression (map index-node-expression library-identifier)]
            [actual-library-identifier (resolve-import-library-identifier library-identifier-expression)])
          (if (null? (walk-library actual-library-identifier root-library-node))
            (if (not (meta-library? actual-library-identifier 'r6rs))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier-expression)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! index-node (library-node-file-nodes (walk-library actual-library-identifier root-library-node))))

        (let ([refs (filter identifier-reference? (import-references document root-library-node actual-library-identifier))])
          ; Attach refs to the library-identifier node itself so that the unused-import
          ; checker can tell whether any binding from this specific import was used.
          (append-references-into-ordered-references-for document index-node refs)
          (append-references-into-ordered-references-for document scope-index-node refs)))]
      [else '()])))

(define (import-references document root-library-node library-identifier)
  (let* ([library-node (walk-library library-identifier root-library-node)]
      [candidate-file-nodes (if (null? library-node) '() (library-node-file-nodes library-node))]
      [candidate-index-node-list (apply append (map document-index-node-list (map file-node-document candidate-file-nodes)))])
    (if (null? candidate-file-nodes)
      (find-meta library-identifier)
      (apply append 
        (map (lambda (x) (import-from-external-index-node document x))
          (filter
            (lambda (index-node)
              (cond 
                [(null? (index-node-children index-node)) #f]
                [else (meta-for? document (car (index-node-children index-node)) 'library)]))
            candidate-index-node-list))))))

(define (import-from-external-index-node document root-index-node)
  (cond 
    [(null? (index-node-children root-index-node)) '()]
    [(meta-for? document (car (index-node-children root-index-node)) 'library)
      (apply append (map 
        (lambda (child-node) (match-export child-node))
        (cddr (index-node-children root-index-node))))]
    [else '()]))

(define (match-export index-node)
  (match-index-node index-node
    [('export . clauses)
      (apply append 
        (map 
          (lambda (child-node) (match-export-clause child-node)) 
          clauses))]
    [else '()]))

(define (match-export-clause index-node) 
  (match-index-node index-node
    [('rename . rename-pairs)
      (let loop ([exportion-nodes rename-pairs]
          [result '()])
        (if (null? exportion-nodes)
          result
          (loop 
            (cdr exportion-nodes)
            (append result (index-node-references-export-to-other-node (cadr (index-node-children (car exportion-nodes))))))))]
    [(? index-node-symbol? identifier) (index-node-references-export-to-other-node index-node)]
    [else '()]))
)
