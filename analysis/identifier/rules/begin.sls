(library (scheme-langserver analysis identifier rules begin)
  (export begin-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
(define (begin-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ fuzzy ... ) 
        (let* ([parent (index-node-parent index-node)]
            [children (index-node-children index-node)]
            [pre-target (map index-node-references-import-in-this-node children)]
            [target `(,@pre-target ,(index-node-references-import-in-this-node index-node))])
          (append-references-into-ordered-references-for document parent (apply append target)))]
      [else '()])))
)
