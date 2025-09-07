(library (scheme-langserver analysis identifier self-defined-rules goldfish let1)
  (export 
    let1-process
    let1-parameter-process)
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
; procedure variable 
(define (let1-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (? symbol? identifier) fuzzy ... )
          (let* ([identifier-index-node (cadr (index-node-children index-node))]
              [exclude-list (let1-parameter-process index-node identifier-index-node index-node '() document 'variable)])
            (index-node-excluded-references-set! identifier-index-node exclude-list)
            exclude-list)]
        [else '()])
      (except c
        [else '()]))))

(define (let1-parameter-process initialization-index-node index-node let-node exclude document type)
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

    (append-references-into-ordered-references-for document let-node `(,reference))

    `(,reference)))
)
