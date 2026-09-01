(library (scheme-langserver analysis identifier self-defined-rules ufo-try try)
  (export try-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

(define (try-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(_ _ ... ('except (:= index-node-expression (? symbol? c)) . branches))
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
    [else '()]))
)
