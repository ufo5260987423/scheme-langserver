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

(define (match-import root-file-node root-library-node document index-node)
  (filter 
    (lambda (item) (not (null? item)))
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('import dummy **1 ) 
          (map 
            (lambda (child-node) (match-clause root-file-node root-library-node document child-node)) 
            (index-node-children index-node))]
        [else '()]))))

(define (match-clause root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [grand-parent-index-node (index-node-parent (index-node-parent index-node))])
    (match expression
      [('only (library-identifier **1) identifier **1) 
        (index-node-references-import-in-this-node-set! 
          grand-parent-index-node 
          (append 
            (index-node-references-import-in-this-node grand-parent-index-node)
            (filter
              (lambda (reference) 
                (if (find (lambda(id) (equal? id (identifier-reference-identifier reference))) identifier) #t #f))
              (import-references root-library-node library-identifier))))]
      [('except (library-identifier **1) identifier **1) 
        (index-node-references-import-in-this-node-set! 
          grand-parent-index-node 
          (append 
            (index-node-references-import-in-this-node grand-parent-index-node)
            (filter
              (lambda (reference) 
                (if (find (lambda(id) (not (equal? id (identifier-reference-identifier reference)))) identifier) #t #f))
              (import-references root-library-node library-identifier))))]
      [('prefix (library-identifier **1) prefix-id)
        (let* ([imported-references (import-references root-library-node library-identifier)]
              [prefixed-references 
                (map 
                  (lambda (reference) 
                    (make-identifier-reference
                      (string->symbol (string-append (symbol->string prefix-id) (symbol->string (identifier-reference-identifier reference))))
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
        (let* ([imported-references (import-references root-library-node library-identifier)]
              [find-pre-reference 
                (lambda (external-index-node)
                  (find 
                    (lambda (reference) 
                      (equal? 
                        (identifier-reference-identifier reference) 
                        (string->symbol (annotation-stripped (index-node-datum/annotations external-index-node)))))
                    imported-references))])
          (let loop ([children-index-nodes (cdr (index-node-children index-node))]
                  [external-index-node (caar (cdr (index-node-children index-node)))]
                  [internal-index-node (cadar (cdr (index-node-children index-node)))])

            (index-node-references-import-in-this-node-set! 
              internal-index-node
              (append 
                (index-node-references-import-in-this-node internal-index-node)
                (list (find-pre-reference external-index-node))))

            (index-node-references-export-to-other-node-set! 
              internal-index-node
              (append 
                (index-node-references-export-to-other-node internal-index-node)
                `(,(make-identifier-reference 
                    (string->symbol (annotation-stripped (index-node-datum/annotations internal-index-node)))
                    document
                    internal-index-node
                    library-identifier))))

            (if (not (null? (cdr children-index-nodes)))
              (loop 
                (cdr children-index-nodes)
                (caar (cdr children-index-nodes))
                (cadar (cdr children-index-nodes))))))]
      [(library-identifier **1) 
        (index-node-references-import-in-this-node-set! 
          grand-parent-index-node 
          (append 
            (index-node-references-import-in-this-node grand-parent-index-node)
            (import-references root-library-node library-identifier)))]
      [else #f])))

(define (import-references root-library-node library-identifier)
  (let* ([library-node (walk-library library-identifier root-library-node)]
      [candidate-file-nodes (if (null? library-node) '() (library-node-file-nodes library-node))])
    (if (null? candidate-file-nodes)
      (find-meta library-identifier)
      (map 
        (lambda(n)
          (import-from-external-file (document-index-node (file-node-document n))))
        candidate-file-nodes))))

(define (import-from-external-file root-index-node)
  (let* ([ann (index-node-datum/annotations root-index-node)]
        [expression (annotation-stripped ann)])
    (match expression 
      [('library _ **1 ) 
        (map 
          (lambda (child-node) (match-export child-node))
          (index-node-children root-index-node))]
      [else '()])))


(define (match-export index-node)
  (apply append '()
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [('export dummy **1 ) 
          (map 
            (lambda (child-node) (match-export-clause child-node)) 
            (index-node-children index-node))]
        [else '()]))))

(define (match-export-clause index-node) 
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('rename (internal-names external-names) **1) 
        (let loop ([children-index-nodes (cdr (index-node-children index-node))]
                [external-index-node (cadar (cdr (index-node-children index-node)))]
                [result '()])
          (if (null? children-index-nodes)
            result
            (loop 
              (cdr children-index-nodes)
              (caar (cdr children-index-nodes))
              (apply append result (index-node-references-export-to-other-node external-index-node)))))]
      [(identifier) (index-node-references-export-to-other-node index-node)]
      [else '()])))
)