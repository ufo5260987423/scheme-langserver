(library (scheme-langserver analysis identifier rules body)
  (export body-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
(define (body-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ fuzzy ... ) 
          (let* ([parent (index-node-parent index-node)]
              [children (index-node-children index-node)]
              [pre-target (map index-node-references-import-in-this-node children)]
              [target `(,@pre-target ,(index-node-references-import-in-this-node index-node))])
            (debug:print-expression index-node)
            (pretty-print (length (apply append target)))
            (append-references-into-ordered-references-for document parent (apply append target)))]
        [else '()])
      (except c
        [else '()]))))
)
