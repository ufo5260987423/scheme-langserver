(library (scheme-langserver analysis identifier self-defined-rules ufo-try try)
  (export try-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)
    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (try-process root-file-node root-library-node document index-node step-without-document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [parent-index-node (index-node-parent index-node)]
      [current-absolute-path (uri->path (document-uri document))])
    (try
      (match expression
        [(_ something ... ('except (? symbol? c) branch **1))
          (let* ([children (index-node-children index-node)]
              [except-index-node (car (reverse children))]
              [except-children (index-node-children except-index-node)]
              [c-index-node (cadr except-children)]
              [reference (make-identifier-reference c document c-index-node index-node '() 'variable '() '())])
            (index-node-references-export-to-other-node-set! 
              c-index-node 
              (append 
                (index-node-references-export-to-other-node c-index-node)
                  `(,reference)))
            (append-references-into-ordered-references-for document except-index-node `(,reference)))]
        [else '()])
      (except c
        [else '()]))))
)