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
        [root-index-node (document-index-node document)])
    (match expression
      [('only (library-identifier **1) _ ...) identifier]
      [('except (library-identifier **1) _ ...) identifier]
      [('prefix (library-identifier **1) _ ...) identifier]
      [(library-identifier **1) 
        (let* ([candidate-file-nodes (library-node-file-nodes (walk-library library-identifier root-library-node))]
              [candidate-count (length candidate-file-nodes)])
          (cond
            [(zero? candidate-count) 
              (if (null? (find-meta library-identifier))
                (raise "Candidats not Exist")
                (index-node-references-import-in-this-node-set! 
                  root-index-node 
                  (append 
                    (index-node-references-import-in-this-node root-index-node)
                    (find-meta library-identifier))))]
            [(> candidate-count 1) (raise "Too many candidats")]
            [(= candidate-count 1) 
              (index-node-references-import-in-this-node-set! 
                root-index-node 
                (append 
                  (index-node-references-import-in-this-node root-index-node)
                  (document-index-node (file-node-document (car candidate-file-nodes))
                  )))
            ]
            )]
      [else #f])))
)