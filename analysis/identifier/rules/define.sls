(library (scheme-langserver analysis identifier rules library-define)
  (export define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis util)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (define-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [library-identifiers (get-nearest-ancestor-library-identifier index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [('define (identifier dummy0 ... ) dummy1 ... ) 
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
      [('define identifier dummy ... ) 
        (let ([reference (make-identifier-reference 
                (car* identifier)
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

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
)