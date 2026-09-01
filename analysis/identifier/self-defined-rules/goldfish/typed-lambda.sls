(library (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda)
  (export 
    typed-lambda-process
    typed-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

(define (typed-lambda-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(_ formals-node . rest)
      (let ([formals-expression (index-node-expression formals-node)]
          [formals-children (index-node-children formals-node)])
        (cond
          [(symbol? formals-expression)
            (typed-parameter-process index-node formals-node index-node '() document)]
          [(pair? formals-expression)
            (let loop ([children formals-children])
              (if (not (null? children))
                (let* ([identifier-index-node (car children)]
                    [expression (index-node-expression identifier-index-node)])
                  (cond
                    [(symbol? expression)
                      (typed-parameter-process index-node identifier-index-node index-node '() document)]
                    [(pair? expression)
                      (let* ([sub-identifier-index-node (car (index-node-children identifier-index-node))])
                        (typed-parameter-process index-node sub-identifier-index-node index-node '() document))])
                  (loop (cdr children)))))]))]
    [else '()]))

(define (typed-parameter-process initialization-index-node index-node lambda-node exclude document )
  (let ([expression (index-node-expression index-node)])
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
