(library (scheme-langserver analysis identifier rules let)
  (export 
    let-process
    let-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; procedure variable 
(define (let-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (? symbol? loop-identifier) (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (let ([loop-reference-list (let-parameter-process index-node (cadr (index-node-children index-node)) index-node '() document 'procedure)])
            (let loop ([rest (index-node-children (caddr (index-node-children index-node)))])
              (if (not (null? rest))
                (let* ([identifier-parent-index-node (car rest)]
                      [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                  (index-node-excluded-references-set! 
                    identifier-parent-index-node
                    (append 
                      (index-node-excluded-references identifier-parent-index-node)
                      (let-parameter-process index-node identifier-index-node index-node loop-reference-list document 'variable)))
                  (loop (cdr rest))))))]
        [(_ (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (let-parameter-process index-node identifier-index-node index-node '() document 'variable)))
                (loop (cdr rest)))))]
        [else '()])
      (except c
        [else '()]))))

(define (let-parameter-process initialization-index-node index-node let-node exclude document type)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
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

    (index-node-references-import-in-this-node-set! 
      let-node
      (sort-identifier-references
        (append 
          (index-node-references-import-in-this-node let-node)
            `(,reference))))

    (index-node-excluded-references-set! 
      (index-node-parent index-node)
      (append 
        (index-node-excluded-references index-node)
        exclude))

    `(,reference)))
)
