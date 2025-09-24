(library (scheme-langserver analysis identifier rules library-import)
  (export 
    library-import-process
    invoke-library-process
    import-process
    import-references
    import-from-external-index-node
    process-library-identifier-excluded-references)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; pointer 
(define (library-import-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ fuzzy import-things **1) 
        (map 
          (lambda (child-node) (match-import index-node root-file-node root-library-node document child-node))
          (cddr (index-node-children index-node)))]
      [else '()])
    index-node))

(define (invoke-library-process root-file-node root-library-node document index-node)
  (filter-empty-list 
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [parent-index-node (index-node-parent index-node)])
      (match expression
        [(_ ('quote (library-identifier **1)) fuzzy ...) 
          (append-references-into-ordered-references-for 
            document 
            parent-index-node 
            (filter identifier-reference? (import-references root-library-node library-identifier)))]
        [else '()]))))

(define (import-process root-file-node root-library-node document index-node)
  (filter-empty-list 
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [(_ dummy **1 ) 
          (map 
            (lambda (child-node) (match-clause index-node root-file-node root-library-node document child-node)) 
            (cdr (index-node-children index-node)))]
        [else '()]))))

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
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('import dummy **1 ) 
          (map 
            (lambda (child-node) (match-clause initialization-index-node root-file-node root-library-node document child-node)) 
            (cdr (index-node-children index-node)))]
        [else '()]))))

(define (match-clause initialization-index-node root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [grand-parent-index-node (index-node-parent (index-node-parent index-node))])
    (match expression
      [('only (library-identifier **1) (? symbol? identifier) **1) 
        (let loop ([importion-index-node (cddr (index-node-children index-node))]
            [identifiers identifier]
            [imported-references 
              (filter
                (lambda (reference) 
                  (if (find (lambda(id) (equal? id (identifier-reference-identifier reference))) identifier) #t #f))
                (import-references root-library-node library-identifier))])

          (if (not (null? importion-index-node))
            (let* ([current-index-node (car importion-index-node)]
                [current-identifier (car identifiers)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (equal? current-identifier (identifier-reference-identifier reference)))
                    imported-references)])

              (append-references-into-ordered-references-for document current-index-node current-references)

              (append-references-into-ordered-references-for document grand-parent-index-node current-references)

              (loop 
                (cdr importion-index-node) 
                (cdr identifiers) 
                (filter
                  (lambda (reference) 
                    (not (equal? current-identifier (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('except (library-identifier **1) (? symbol? identifier) **1) 
        (let ([tmp 
              (filter
                (lambda (reference) 
                  (if (find (lambda(id) (not (equal? id (identifier-reference-identifier reference)))) identifier) #t #f))
                (import-references root-library-node library-identifier))])
          (if (null? grand-parent-index-node)
            (document-ordered-reference-list-set! 
              document
              (sort-identifier-references (append (document-ordered-reference-list document) tmp)))
            (append-references-into-ordered-references-for document grand-parent-index-node tmp)))

        (let loop ([importion-index-node (cddr (index-node-children index-node))]
            [identifiers identifier]
            [imported-references 
              (filter
                (lambda (reference) 
                  (if (find (lambda(id) (equal? id (identifier-reference-identifier reference))) identifier) #t #f))
                (import-references root-library-node library-identifier))])
          (if (not (null? importion-index-node))
            (let* ([current-index-node (car importion-index-node)]
                [current-identifier (car identifiers)]
                [current-references 
                  (filter
                    (lambda (reference) 
                      (equal? current-identifier (identifier-reference-identifier reference)))
                    imported-references)])

              (append-references-into-ordered-references-for document current-index-node current-references)
              (loop 
                (cdr importion-index-node) 
                (cdr identifiers) 
                (filter
                  (lambda (reference) 
                    (not (equal? current-identifier (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('prefix (library-identifier **1) (? symbol? prefix-id))
        (let* ([imported-references (import-references root-library-node library-identifier)]
            [prefixed-references 
              (map 
                (lambda (reference) 
                  (make-identifier-reference
                    (string->symbol (string-append (symbol->string prefix-id) (symbol->string (identifier-reference-identifier reference))))
                    (identifier-reference-document reference)
                    (identifier-reference-index-node reference)
                    initialization-index-node 
                    (identifier-reference-library-identifier reference)
                    'pointer
                    `(,reference)
                    (identifier-reference-type-expressions reference))) 
                imported-references)])
          ;;todo: add something to export-to-other-node for current-index-node?
          (append-references-into-ordered-references-for document grand-parent-index-node prefixed-references))]
      [('rename (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1 ) 
        (let loop ([importion-nodes (cddr (index-node-children index-node))]
            [external-names external-name]
            [internal-names internal-name]
            [imported-references 
              (filter
                (lambda (reference) 
                  (if (find (lambda(id) (equal? id (identifier-reference-identifier reference))) external-name) #t #f))
                (import-references root-library-node library-identifier))])
          (if (not (null? importion-nodes))
            (let* ([current-importion-pair (index-node-children (car importion-nodes))]
                [current-external-node (car current-importion-pair)]
                [current-internal-node (cadr current-importion-pair)]
                [current-external-name (car external-names)]
                [current-internal-name (car internal-names)]
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
              (append-references-into-ordered-references-for document grand-parent-index-node renamed-references)

              (index-node-references-export-to-other-node-set! 
                current-external-node
                (append 
                  (index-node-references-import-in-this-node current-external-node)
                  renamed-references))
              (loop 
                (cdr importion-nodes)
                (cdr external-names)
                (cdr internal-names)
                (filter
                  (lambda (reference) 
                    (not (equal? current-external-name (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('alias (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1 ) 
        (let loop ([importion-nodes (cddr (index-node-children index-node))]
            [external-names external-name]
            [internal-names internal-name]
            [imported-references 
              (filter
                (lambda (reference) 
                  (if (find (lambda(id) (equal? id (identifier-reference-identifier reference))) external-name) #t #f))
                (import-references root-library-node library-identifier))])
          (if (not (null? importion-nodes))
            (let* ([current-importion-pair (index-node-children (car importion-nodes))]
                [current-external-node (car current-importion-pair)]
                [current-internal-node (cadr current-importion-pair)]
                [current-external-name (car external-names)]
                [current-internal-name (car internal-names)]
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
              (append-references-into-ordered-references-for document grand-parent-index-node current-references)
              (append-references-into-ordered-references-for document grand-parent-index-node renamed-references)

              (index-node-references-export-to-other-node-set! 
                current-external-node
                (append 
                  (index-node-references-import-in-this-node current-external-node)
                  renamed-references))
              (loop 
                (cdr importion-nodes)
                (cdr external-names)
                (cdr internal-names)
                (filter
                  (lambda (reference) 
                    (not (equal? current-external-name (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('for (library-identifier **1) import-level) 
        (if (or
            (equal? 'run import-level)
            (equal? '(meta 0) import-level)
            ; (equal? 'expand import-level)
            ; (equal? '(meta 1) import-level)
            )
          (let ([tmp (filter identifier-reference? (import-references root-library-node library-identifier))])
            (if (null? grand-parent-index-node)
              (document-ordered-reference-list-set! 
                document
                (sort-identifier-references (append (document-ordered-reference-list document) tmp)))
              (append-references-into-ordered-references-for document grand-parent-index-node tmp))))]
      [(library-identifier **1) 
        (append-references-into-ordered-references-for 
          document 
          grand-parent-index-node 
          (filter identifier-reference? (import-references root-library-node library-identifier)))]
      [else '()])))

(define (import-references root-library-node library-identifier)
  (let* ([library-node (walk-library library-identifier root-library-node)]
      [candidate-file-nodes (if (null? library-node) '() (library-node-file-nodes library-node))]
      [candidate-index-node-list (apply append (map document-index-node-list (map file-node-document candidate-file-nodes)))])
    (if (null? candidate-file-nodes)
      (find-meta library-identifier)
      (apply append 
        (map import-from-external-index-node
          (filter
            (lambda (index-node)
              (match (annotation-stripped (index-node-datum/annotations index-node))
                (['library (identifier **1) _ ... ] (equal? identifier library-identifier))
                (else #f)))
            candidate-index-node-list))))))

(define (import-from-external-index-node root-index-node)
  (let* ([ann (index-node-datum/annotations root-index-node)]
      [expression (annotation-stripped ann)])
    (match expression 
      [('library _ **1 ) 
        (apply append (map 
          (lambda (child-node) (match-export child-node))
          (cddr (index-node-children root-index-node))))]
      [else '()])))

(define (match-export index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('export dummy **1 ) 
        (apply append 
          (map 
            (lambda (child-node) (match-export-clause child-node)) 
            (cdr (index-node-children index-node))))]
      [else '()])))

(define (match-export-clause index-node) 
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('rename ((? symbol? internal-names) (? symbol? external-names)) **1) 
        (let loop ([exportion-nodes (cdr (index-node-children index-node))]
            [result '()])
          (if (null? exportion-nodes)
            result
            (loop 
              (cdr exportion-nodes)
              (append result (index-node-references-export-to-other-node (cadr (index-node-children (car exportion-nodes))))))))]
      [(? symbol? identifier) (index-node-references-export-to-other-node index-node)]
      [else '()])))
)