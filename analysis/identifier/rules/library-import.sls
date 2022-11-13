(library (scheme-langserver analysis identifier rules library-import)
  (export import-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis meta)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (import-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('library _ **1 ) 
        (map 
          (lambda (child-node) (match-import root-file-node root-library-node document child-node))
          (index-node-children index-node))]
      [else '()])
    index-node))

(define (filter-empty-list list-instance)
  (filter 
    (lambda (item) (not (null? item)))
    list-instance))

(define (match-import root-file-node root-library-node document index-node)
  (filter-empty-list 
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('import dummy **1 ) 
          (map 
            (lambda (child-node) (match-clause root-file-node root-library-node document child-node)) 
            (cdr (index-node-children index-node)))]
        [else '()]))))

(define (match-clause root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [grand-parent-index-node (index-node-parent (index-node-parent index-node))])
    (match expression
      [('only (library-identifier **1) identifier **1) 
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

              (index-node-references-import-in-this-node-set! 
                current-index-node
                (append 
                  (index-node-references-import-in-this-node current-index-node)
                  current-references))
              
              (index-node-references-import-in-this-node-set! 
                grand-parent-index-node 
                (append 
                  (index-node-references-import-in-this-node grand-parent-index-node)
                  current-references))

              (loop 
                (cdr importion-index-node) 
                (cdr identifiers) 
                (filter
                  (lambda (reference) 
                    (not (equal? current-identifier (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('except (library-identifier **1) identifier **1) 
        (index-node-references-import-in-this-node-set! 
          grand-parent-index-node 
          (append 
            (index-node-references-import-in-this-node grand-parent-index-node)
            (filter
              (lambda (reference) 
                (if (find (lambda(id) (not (equal? id (identifier-reference-identifier reference)))) identifier) #t #f))
              (import-references root-library-node library-identifier))))

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

              (index-node-references-import-in-this-node-set! 
                current-index-node
                (append 
                  (index-node-references-import-in-this-node current-index-node)
                  current-references))
              (loop 
                (cdr importion-index-node) 
                (cdr identifiers) 
                (filter
                  (lambda (reference) 
                    (not (equal? current-identifier (identifier-reference-identifier reference))))
                  imported-references)))))]
      [('prefix (library-identifier **1) prefix-id)
        (let* ([imported-references (import-references root-library-node library-identifier)]
            [prefixed-references 
              (map 
                (lambda (reference) 
                  (make-identifier-reference
                    (string-append (symbol->string prefix-id) (symbol->string (identifier-reference-identifier reference)))
                    (identifier-reference-document reference)
                    (identifier-reference-index-node reference)
                    (identifier-reference-library-identifier reference))) 
                imported-references)])
          ;;todo: add something to export-to-other-node for current-index-node?
          (index-node-references-import-in-this-node-set! 
            grand-parent-index-node 
            (append 
              (index-node-references-import-in-this-node grand-parent-index-node)
              prefixed-references)))]
      [('rename (library-identifier **1) (external-name internal-name) **1 ) 
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
                        (identifier-reference-library-identifier reference))) 
                    current-references)])

              (index-node-references-import-in-this-node-set! 
                current-internal-node
                (append 
                  (index-node-references-import-in-this-node current-internal-node)
                  current-references))

              (index-node-references-import-in-this-node-set! 
                grand-parent-index-node 
                (append 
                  (index-node-references-import-in-this-node grand-parent-index-node)
                  renamed-references))

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
      [('alias (library-identifier **1) (external-name internal-name) **1 ) 
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
                        (identifier-reference-library-identifier reference))) 
                    current-references)])

              (index-node-references-import-in-this-node-set! 
                current-internal-node
                (append 
                  (index-node-references-import-in-this-node current-internal-node)
                  current-references))

              (index-node-references-import-in-this-node-set! 
                grand-parent-index-node 
                (append 
                  (index-node-references-import-in-this-node grand-parent-index-node)
                  current-references))

              (index-node-references-import-in-this-node-set! 
                grand-parent-index-node 
                (append 
                  (index-node-references-import-in-this-node grand-parent-index-node)
                  renamed-references))

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
      [(library-identifier **1) 
        (index-node-references-import-in-this-node-set! 
          grand-parent-index-node 
          (append 
            (index-node-references-import-in-this-node grand-parent-index-node)
            (filter identifier-reference? (import-references root-library-node library-identifier))))]
      [else '()])))

(define (import-references root-library-node library-identifier)
  (let* ([library-node (walk-library library-identifier root-library-node)]
      [candidate-file-nodes (if (null? library-node) '() (library-node-file-nodes library-node))]
      [candidate-index-node-list (apply append (map document-index-node-list (map file-node-document candidate-file-nodes)))])
    (if (null? candidate-file-nodes)
      (find-meta library-identifier)
      (apply append 
        (map import-from-external-file 
          (filter
            (lambda (index-node)
              (match (annotation-stripped (index-node-datum/annotations index-node))
                (['library (identifier **1) _ ... ] (equal? identifier library-identifier))
                (else #f)))
            candidate-index-node-list))))))

(define (import-from-external-file root-index-node)
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
      [('rename (internal-names external-names) **1) 
        (let loop ([exportion-nodes (cdr (index-node-children index-node))]
            [result '()])
          (if (null? exportion-nodes)
            result
            (loop 
              (cdr exportion-nodes)
              (append result (index-node-references-export-to-other-node (cadr (index-node-children (car exportion-nodes))))))))]
      [identifier (index-node-references-export-to-other-node index-node)]
      [else '()])))
)