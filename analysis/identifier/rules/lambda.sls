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

; reference-identifier-type include 
; parameter 
(define (lambda-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('lambda (identifier **1) _ ... ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-index-node (car rest)]
                  [identifier-index-node-parent (index-node-parent identifier-index-node)])
                (private-process identifier-index-node index-node '() document)
                (loop (cdr rest)))))]
        [('lambda (? symbol? identifier) _ ... ) 
          (guard-for document index-node 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (private-process (cadr (index-node-children index-node)) index-node '() document)]
        [('case-lambda (dummy0 ...) dummy1 ... ) 
          (guard-for document index-node 'case-lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([rest (cdr (index-node-children index-node))])
            (if (not (null? rest))
              (let* ([identifier-index-node-grand-parent (car rest)]
                  [grand-parent-expression (annotation-stripped (index-node-datum/annotations identifier-index-node-grand-parent))])
                (match grand-parent-expression 
                  [((param-identifier **1) body ...)
                    (let* ([identifier-index-node-parent (car (index-node-children identifier-index-node-grand-parent))])
                      (let param-loop ([exclude '()] [param-identifier-index-node-list (index-node-children identifier-index-node-parent)])
                        (if (null? param-identifier-index-node-list)
                          (loop (cdr rest))
                          (param-loop 
                            (append exclude (private-process (car param-identifier-index-node-list) identifier-index-node-grand-parent exclude document)) 
                            (cdr param-identifier-index-node-list)))))]
                  [else '()]
                ))))]
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
              '()
              'parameter)])
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
            exclude
            `(,reference)))
        `(,reference))
      '())))
)
