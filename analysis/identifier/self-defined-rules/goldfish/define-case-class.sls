(library (scheme-langserver analysis identifier self-defined-rules goldfish define-case-class)
  (export 
    define-case-class-process
    define-case-class-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (ufo-try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; parameter 
(define (define-case-class-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ _name (identifier **1) fuzzy ... )
        (let loop ([rest (index-node-children (caddr (index-node-children index-node)))])
          (if (not (null? rest))
            (let* ([identifier-index-node (car rest)]
                [identifier-index-node-parent (index-node-parent identifier-index-node)])
              (let* ([ann (index-node-datum/annotations identifier-index-node)]
                [expression (annotation-stripped ann)])
                (match expression
                  [(? symbol? x)
                    (define-case-class-parameter-process index-node identifier-index-node index-node '() document)]
                  [(? pair? y)
                    (let* ([sub-identifier-index-node (car (index-node-children identifier-index-node))]
                      [sub-identifier-index-node-parent (index-node-parent sub-identifier-index-node)])
                      (define-case-class-parameter-process index-node sub-identifier-index-node index-node '() document))]))
              (loop (cdr rest)))))]
      [else '()])))

(define (define-case-class-parameter-process initialization-index-node index-node lambda-node exclude document )
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
          (index-node-parent index-node)
          (append 
            (index-node-excluded-references index-node)
            exclude
            `(,reference)))
        `(,reference))
      '())))
)
