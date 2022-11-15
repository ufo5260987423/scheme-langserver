(library (scheme-langserver analysis identifier rules lambda)
  (export lambda-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (lambda-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('lambda ((? symbol? identifier) **1) _ ... ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (private-process identifier-index-node index-node '() document)))
                (loop (cdr rest)))))]
        [('case-lambda (_ ...) _ ... ) 
          (guard-for document index-node 'case-lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (cdr (index-node-children index-node))])
            (if (not (null? rest))
              (let* ([identifier-grand-parent-index-node (car rest)]
                    [identifier-parent-index-node (car (index-node-children identifier-grand-parent-index-node))])
                (let param-loop ([exclude '()]
                      [params (index-node-children (identifier-parent-index-node))])
                  (if (null? params)
                    (index-node-excluded-references-set! 
                      identifier-parent-index-node
                      (append 
                        (index-node-excluded-references identifier-parent-index-node)
                        exclude))
                    (param-loop 
                      (append (private-process (car params) identifier-grand-parent-index-node '() document)) 
                      (cdr params)))
                (loop (cdr rest))))))]
        [else '()])
      (except c
        [else '()]))))

(define (private-process index-node lambda-node exclude document )
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (symbol? expression)
      (let ([reference 
            (make-identifier-reference
              expression
              document
              index-node
              '())])
        (index-node-references-export-to-other-node-set! 
          index-node
          (append 
            (index-node-references-export-to-other-node index-node)
            `(,reference)))

        (index-node-references-import-in-this-node-set! 
          lambda-node
          (append 
            (index-node-references-import-in-this-node lambda-node)
            `(,reference)))

        (index-node-excluded-references-set! 
          (index-node-parent index-node)
          (append 
            (index-node-excluded-references index-node)
            exclude))
        `(,reference))
      '())))
)