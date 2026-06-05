(library (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda)
  (export 
    typed-lambda-process
    typed-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; parameter 
(define (typed-lambda-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (identifier **1) fuzzy ... )
        (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
          (if (not (null? rest))
            (let* ([identifier-index-node (car rest)]
                [identifier-index-node-parent (index-node-parent identifier-index-node)])
              (let* ([ann (index-node-datum/annotations identifier-index-node)]
                [expression (annotation-stripped ann)])
                (match expression
                  [(? symbol? x)
                    (typed-parameter-process index-node identifier-index-node index-node '() document)]
                  [(? pair? y)
                    (let* ([sub-identifier-index-node (car (index-node-children identifier-index-node))]
                      [sub-identifier-index-node-parent (index-node-parent sub-identifier-index-node)])
                      (typed-parameter-process index-node sub-identifier-index-node index-node '() document))]))
              (loop (cdr rest)))))]
      
      [(_ (? symbol? identifier) fuzzy ... ) 
        (typed-parameter-process index-node (cadr (index-node-children index-node)) index-node '() document)]
      [(_ (identifier . rest) fuzzy ... ) 
        (let* ([formals-index-node (cadr (index-node-children index-node))]
            [formals-children (index-node-children formals-index-node)])
          (let loop ([children formals-children])
            (if (not (null? children))
              (let* ([identifier-index-node (car children)]
                  [identifier-index-node-parent (index-node-parent identifier-index-node)])
                (let* ([ann (index-node-datum/annotations identifier-index-node)]
                    [expression (annotation-stripped ann)])
                  (match expression
                    [(? symbol? x)
                      (typed-parameter-process index-node identifier-index-node index-node '() document)]
                    [(? pair? y)
                      (let* ([sub-identifier-index-node (car (index-node-children identifier-index-node))]
                          [sub-identifier-index-node-parent (index-node-parent sub-identifier-index-node)])
                        (typed-parameter-process index-node sub-identifier-index-node index-node '() document))]))
                (loop (cdr children))))))]
      [else '()])))

(define (typed-parameter-process initialization-index-node index-node lambda-node exclude document )
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (let ([reference 
            (make-identifier-reference
              expression
              document
              index-node
              initialization-index-node
              '()
              'parameter
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          index-node
          (append 
            (index-node-references-export-to-other-node index-node)
            `(,reference)))

        (index-node-references-import-in-this-node-set! 
          lambda-node
          (sort-identifier-references 
            (append 
              (index-node-references-import-in-this-node lambda-node)
              `(,reference))))

        (index-node-excluded-references-set! 
          (index-node-parent (index-node-parent index-node))
          (append 
            (index-node-excluded-references index-node)
            exclude
            `(,reference)))
        `(,reference))
      '())))
)
