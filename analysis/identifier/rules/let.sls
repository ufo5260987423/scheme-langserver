(library (scheme-langserver analysis identifier rules let)
  (export 
    let-process
    generate-naive-let-process-with
    let-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (generate-naive-let-process-with type)
  (lambda (root-file-node root-library-node document index-node)
    (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
      (match expression
        [(_ (fuzzy0 **1 ) fuzzy1 ... ) 
          (fold-left 
            (lambda (exclude-list identifier-parent-index-node)
              (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                  [target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document type)]
                  [extended-exclude-list (append exclude-list target-identifier-reference)])
                (index-node-excluded-references-set! (cadr (index-node-children index-node)) extended-exclude-list)
                extended-exclude-list))
            '()
            (filter 
              (lambda (i) (not (null? (index-node-children i)))) 
              (index-node-children (cadr (index-node-children index-node)))))]
        [else '()]))))

; reference-identifier-type include 
; procedure variable 
(define (let-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (? symbol? loop-identifier) (fuzzy0 **1) fuzzy ... ) 
        (fold-left 
          (lambda (exclude-list identifier-parent-index-node)
            (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                [extended-exclude-list (append exclude-list (let-parameter-process index-node identifier-index-node index-node document 'variable))])
              (index-node-excluded-references-set! (caddr (index-node-children index-node)) extended-exclude-list)
              extended-exclude-list))
          (let-parameter-process index-node (cadr (index-node-children index-node)) index-node document 'procedure)
          (filter 
            (lambda (i) (not (null? (index-node-children i))))
            (index-node-children (caddr (index-node-children index-node)))))]
      [(_ (? symbol? loop-identifier) fuzzy **1 ) 
        (let-parameter-process index-node (cadr (index-node-children index-node)) index-node document 'procedure)]
      [else ((generate-naive-let-process-with 'variable) root-file-node root-library-node document index-node)])))

(define (let-parameter-process initialization-index-node index-node let-node document type)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (not (symbol? expression))
      '()
      (let (
        [reference 
          (make-identifier-reference
            expression
            document
            index-node
            initialization-index-node
            '()
            type
            '()
            '())])
      (index-node-references-export-to-other-node-set! 
        index-node
        (append 
          (index-node-references-export-to-other-node index-node)
            `(,reference)))

      (append-references-into-ordered-references-for document let-node `(,reference))

      `(,reference)))))
)
