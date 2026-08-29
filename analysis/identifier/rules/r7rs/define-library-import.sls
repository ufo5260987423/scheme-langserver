(library (scheme-langserver analysis identifier rules r7rs define-library-import)
  (export 
    library-import-process-r7rs
    r7-import-process)
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
(define (library-import-process-r7rs root-file-node root-library-node document index-node)
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

(define (r7-import-process root-file-node root-library-node document index-node)
  (filter-empty-list 
    (match-index-node index-node
      [('import . clauses)
        (map 
          (lambda (child-node) (match-clause index-node root-file-node root-library-node document child-node)) 
          clauses)]
      [else '()])))

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

(define (match-import initialization-index-node root-file-node root-library-node document index-node)
  (filter-empty-list 
    (match-index-node index-node
      [('import . clauses)
        (map 
          (lambda (child-node) (match-clause initialization-index-node root-file-node root-library-node document child-node)) 
          clauses)]
      [else '()])))

(define (match-clause initialization-index-node root-file-node root-library-node document index-node)
  (let* ([grand-parent-index-node (index-node-parent (index-node-parent index-node))]
      [scope-index-node
        (if (and (not (null? initialization-index-node))
              (let ([expr (index-node-expression initialization-index-node)])
                (and (list? expr) (not (null? expr)) (memq (car expr) '(define-library)))))
          initialization-index-node
          grand-parent-index-node)])
    (match-index-node index-node
      [('only ((? index-node-symbol? library-identifier) ...) . identifier-index-nodes)
        (if (symbol-children? identifier-index-nodes)
          (let ([library-identifier (map index-node-expression library-identifier)])
            (if (null? (walk-library library-identifier root-library-node))
              (if (and (not (meta-library? library-identifier 'r7rs))
                  (not (meta-library? library-identifier 's7)))
                (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier)) "import" "library-not-found")))
              (index-node-import-file-nodes-set! (car library-identifier) (library-node-file-nodes (walk-library library-identifier root-library-node))))

          (let loop ([importion-index-node identifier-index-nodes]
              [identifier-index-nodes identifier-index-nodes]
              [imported-references 
                (filter
                  (lambda (reference) 
                    (find (lambda(id) (equal? id (identifier-reference-identifier reference))) (map index-node-expression identifier-index-nodes)))
                  (import-references document root-library-node library-identifier))])

            (if (not (null? importion-index-node))
              (let* ([current-index-node (car importion-index-node)]
                  [current-references 
                    (filter
                      (lambda (reference) 
                        (equal? (index-node-expression current-index-node) (identifier-reference-identifier reference)))
                      imported-references)])

                (append-references-into-ordered-references-for document current-index-node current-references)

                (append-references-into-ordered-references-for document scope-index-node current-references)

                (loop 
                  (cdr importion-index-node) 
                  identifier-index-nodes
                  (filter
                    (lambda (reference) 
                      (not (equal? (index-node-expression current-index-node) (identifier-reference-identifier reference))))
                    imported-references))))))
          '())]
      [('except ((? index-node-symbol? library-identifier) ...) . identifier-index-nodes)
        (if (symbol-children? identifier-index-nodes)
          (let ([library-identifier (map index-node-expression library-identifier)])
            (if (null? (walk-library library-identifier root-library-node))
              (if (and (not (meta-library? library-identifier 'r7rs))
                  (not (meta-library? library-identifier 's7)))
                (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier)) "import" "library-not-found")))
              (index-node-import-file-nodes-set! (car library-identifier) (library-node-file-nodes (walk-library library-identifier root-library-node))))

          (let ([tmp 
                (filter
                  (lambda (reference) 
                    (find (lambda(id) (not (equal? id (identifier-reference-identifier reference)))) (map index-node-expression identifier-index-nodes)))
                  (import-references document root-library-node library-identifier))])
            (if (null? scope-index-node)
              (document-ordered-reference-list-set! 
                document
                (sort-identifier-references (append (document-ordered-reference-list document) tmp)))
              (append-references-into-ordered-references-for document scope-index-node tmp)))

          (let loop ([importion-index-node identifier-index-nodes]
              [identifier-index-nodes identifier-index-nodes]
              [imported-references 
                (filter
                  (lambda (reference) 
                    (find (lambda(id) (equal? id (identifier-reference-identifier reference))) (map index-node-expression identifier-index-nodes)))
                  (import-references document root-library-node library-identifier))])
            (if (not (null? importion-index-node))
              (let* ([current-index-node (car importion-index-node)]
                  [current-references 
                    (filter
                      (lambda (reference) 
                        (equal? (index-node-expression current-index-node) (identifier-reference-identifier reference)))
                      imported-references)])

                (append-references-into-ordered-references-for document current-index-node current-references)
                (loop 
                  (cdr importion-index-node) 
                  identifier-index-nodes
                  (filter
                    (lambda (reference) 
                      (not (equal? (index-node-expression current-index-node) (identifier-reference-identifier reference))))
                    imported-references))))))
          '())]
      [('prefix ((? index-node-symbol? library-identifier) ...) (? index-node-symbol? prefix-id))
        (let ([library-identifier (map index-node-expression library-identifier)])
          (if (null? (walk-library library-identifier root-library-node))
            (if (and (not (meta-library? library-identifier 'r7rs))
                (not (meta-library? library-identifier 's7)))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! (car library-identifier) (library-node-file-nodes (walk-library library-identifier root-library-node))))

        (let* ([imported-references (import-references document root-library-node library-identifier)]
            [prefix-id-expression (index-node-expression prefix-id)]
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
        (let ([library-identifier (map index-node-expression library-identifier)])
          (if (null? (walk-library library-identifier root-library-node))
            (if (and (not (meta-library? library-identifier 'r7rs))
                (not (meta-library? library-identifier 's7)))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! (car library-identifier) (library-node-file-nodes (walk-library library-identifier root-library-node))))

        (let loop ([importion-nodes rename-pairs]
            [imported-references 
              (filter
                (lambda (reference) 
                  (find 
                    (lambda (id) (equal? id (identifier-reference-identifier reference)))
                    (map 
                      (lambda (rename-pair-index-node)
                        (index-node-expression (car (index-node-children rename-pair-index-node))))
                      rename-pairs)))
                (import-references document root-library-node library-identifier))])
          (if (not (null? importion-nodes))
            (let* ([current-importion-pair (index-node-children (car importion-nodes))]
                [current-external-node (car current-importion-pair)]
                [current-internal-node (cadr current-importion-pair)]
                [current-external-name (index-node-expression current-external-node)]
                [current-internal-name (index-node-expression current-internal-node)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (equal? current-external-name (identifier-reference-identifier reference)))
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
                    (not (equal? current-external-name (identifier-reference-identifier reference))))
                  imported-references))))))]
      [((? index-node-symbol? library-identifier) ...)
        (let ([library-identifier (map index-node-expression library-identifier)])
          (if (null? (walk-library library-identifier root-library-node))
            (if (and (not (meta-library? library-identifier 'r7rs))
                (not (meta-library? library-identifier 's7)))
              (append-new-diagnoses document `(,(index-node-start index-node) ,(index-node-end index-node) 2 ,(string-append "Fail to find library: " (library-identifier->string library-identifier)) "import" "library-not-found")))
            (index-node-import-file-nodes-set! index-node (library-node-file-nodes (walk-library library-identifier root-library-node))))

        (append-references-into-ordered-references-for 
          document 
          scope-index-node 
          (filter identifier-reference? (import-references document root-library-node library-identifier))))]
      [else '()])))

(define (symbol-children? children)
  (or (null? children)
      (and (index-node-symbol? (car children))
           (symbol-children? (cdr children)))))

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
                [else (meta-for? document (car (index-node-children index-node)) 'define-library)]))
            candidate-index-node-list))))))

(define (import-from-external-index-node document root-index-node)
  (cond 
    [(null? (index-node-children root-index-node)) '()]
    [(meta-for? document (car (index-node-children root-index-node)) 'define-library)
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
