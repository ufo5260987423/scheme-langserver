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
      [('library _ **1 ) 
        (map 
          (lambda (child-node) (match-define root-file-node document child-node))
          (index-node-children index-node))]
      [else 
        ; (map 
        ;   (lambda (child-node) (library-define-process root-file-node document child-node))
        ;   (index-node-children index-node))
        '()])
    index-node))

(define (match-define root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('define (identifier _ ... )_ ... ) 
        (let ([reference (make-identifier-reference document (car (index-node-children (car (index-node-children index-node)))))])
    (display (annotation-stripped (index-node-datum/annotations (identifier-reference-index-node reference))))
    (display (newline))
          (index-node-references-export-to-other-node-set! 
            index-node
            (append 
              (index-node-references-export-to-other-node index-node)
              `(,reference)))
          (index-node-references-import-in-this-node-set! 
            (index-node-parent index-node) 
            (append 
              (index-node-references-import-in-this-node (index-node-parent index-node))
              `(,reference))))]
      [('define identifier _ ... ) 
        (let ([reference (make-identifier-reference document (car (index-node-children index-node)))])
          (index-node-references-export-to-other-node-set! 
            index-node
            (append 
              (index-node-references-export-to-other-node index-node)
              `(,reference)))
          (index-node-references-import-in-this-node-set! 
            (index-node-parent index-node)
            (append 
              (index-node-references-import-in-this-node (index-node-parent index-node))
              `(,reference))))]
      [else #f])))
)