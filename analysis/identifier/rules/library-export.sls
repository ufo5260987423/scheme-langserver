(library (scheme-langserver analysis identifier rules library-export)
  (export export-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (export-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('library (library-identifiers **1) _ **1 ) 
        (map 
          (lambda (child-node) (match-export root-file-node document library-identifiers child-node))
          (index-node-children index-node))]
      [else 
        ; (map 
        ;   (lambda (child-node) (library-define-process root-file-node document child-node))
        ;   (index-node-children index-node))
        '()])
    index-node))

(define (match-export root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('export dummy **1 ) 
        (map 
          (lambda (child-node) (match-clause root-file-node document library-identifiers child-node)) 
          (index-node-children index-node))]
      [else '()])))

(define (match-clause root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('rename (internal-names external-names) **1) 
        (let loop ([children-index-nodes (cdr (index-node-children index-node))]
                [internal-index-node (caar (cdr (index-node-children index-node)))]
                [external-index-node (cadar (cdr (index-node-children index-node)))])

          (index-node-references-import-in-this-node-set! 
            external-index-node
            (append 
              (index-node-references-import-in-this-node external-index-node)
              (find-available-references-for 
                internal-index-node 
                (string->symbol 
                  (annotation-stripped (index-node-datum/annotations internal-index-node))))))

          (index-node-references-export-to-other-node-set! 
            external-index-node
            (append 
              (index-node-references-export-to-other-node external-index-node)
              `(,(make-identifier-reference
                  (string->symbol (annotation-stripped (index-node-datum/annotations external-index-node)))
                  document
                  external-index-node
                  library-identifiers))))

          (if (not (null? (cdr children-index-nodes)))
            (loop 
              (cdr children-index-nodes)
              (caar (cdr children-index-nodes))
              (cadar (cdr children-index-nodes)))))]
      [(identifier) 
        (let* ([references (find-available-references-for index-node identifier)]
            [reference-count (length references)])
          (cond 
            ;todo
            [(zero? reference-count) '()]
            [(> reference-count 1) (raise "Multi-defined")]
            [(= reference-count 1) 
              (index-node-references-export-to-other-node-set! 
                index-node
                (append 
                  (index-node-references-export-to-other-node index-node)
                  `(,(car references))))]))]
      [else #f])))
)