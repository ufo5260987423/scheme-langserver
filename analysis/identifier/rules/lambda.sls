(library (scheme-langserver analysis identifier rules lambda)
  (export let-process)
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
        [('lambda (identifier **1) _ ... ) 
          (guard-for 'lambda '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
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
        [else '()])
      (except c
        [else '()]))))

(define (private-process index-node lambda-node exclude document )
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [reference 
          (make-identifier-reference
            (string->symbol expression)
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

    `(,reference)))
)