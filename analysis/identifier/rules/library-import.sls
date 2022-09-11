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
      [else 
        ; (map 
        ;   (lambda (child-node) (library-define-process root-file-node document child-node))
        ;   (index-node-children index-node))
        '()])
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
      [('only (library-identifier **1) _ ...) identifier]
      [('except (library-identifier **1) _ ...) identifier]
      [('prefix (library-identifier **1) _ ...) identifier]
      [('rename (library-identifier **1) _ ...) identifier]
      [(library-identifier **1) 
        (let* ([candidate-file-nodes (library-node-file-nodes (walk-library library-identifier root-library-node))]
              [candidate-count (length candidate-file-nodes)])
          (cond
            [(zero? candidate-count) 
              (if (null? (find-meta library-identifier))
                (raise "Candidats not Exist")
                (index-node-references-import-in-this-node-set! 
                  grand-parent-index-node 
                  (append 
                    (index-node-references-import-in-this-node grand-parent-index-node)
                    (find-meta library-identifier))))]
            [(> candidate-count 1) (raise "Too many candidats")]
            [(= candidate-count 1) 
              (index-node-references-import-in-this-node-set! 
                grand-parent-index-node 
                (apply append 
                  (index-node-references-import-in-this-node grand-parent-index-node)
                  (import-from-external-file (document-index-node (file-node-document (car candidate-file-nodes))))))])]
      [else #f]))))

(define (import-from-external-file root-index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
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
            (lambda (child-node) (match-clause root-file-node document library-identifiers child-node)) 
            (index-node-children index-node))]
        [else '()]))))

(define (match-clause index-node) 
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('rename (internal-names external-names) **1) 
        (let* loop ([children-index-nodes (cdr (index-node-children index-node))]
                [external-index-node (cadar children-index-nodes)]
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