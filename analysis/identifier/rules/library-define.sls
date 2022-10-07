(library (scheme-langserver analysis identifier rules library-define)
  (export library-define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (library-define-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('library (library-identifiers **1) _ **1 ) 
        (map 
          (lambda (child-node) (match-define root-file-node document library-identifiers child-node))
          (index-node-children index-node))]
      [else 
        (map 
          (lambda (child-node) (match-define root-file-node document '() child-node))
          (index-node-children index-node))])
    index-node))

(define (match-define root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('define (identifier _ ... ) _ ... ) 
        (let ([reference (make-identifier-reference 
                identifier 
                document 
                (car (index-node-children (cadr (index-node-children index-node)))) 
                library-identifiers)])
          (index-node-references-export-to-other-node-set! 
            (identifier-reference-index-node reference)
            (append 
              (index-node-references-export-to-other-node (identifier-reference-index-node reference))
              `(,reference)))
          (index-node-references-import-in-this-node-set! 
            (index-node-parent index-node) 
            (append 
              (index-node-references-import-in-this-node (index-node-parent index-node))
              `(,reference))))]
      [('define identifier _ ... ) 
        (let ([reference (make-identifier-reference 
                identifier 
                document 
                (cadr (index-node-children index-node))
                library-identifiers)])
          (index-node-references-export-to-other-node-set! 
            (identifier-reference-index-node reference)
            (append 
              (index-node-references-export-to-other-node (identifier-reference-index-node reference))
              `(,reference)))
          (index-node-references-import-in-this-node-set! 
            (index-node-parent index-node)
            (append 
              (index-node-references-import-in-this-node (index-node-parent index-node))
              `(,reference))))]
      [else '()])))
)