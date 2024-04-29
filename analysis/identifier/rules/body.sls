(library (scheme-langserver analysis identifier rules body)
  (export body-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util dedupe)

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
            (if (null? parent)
              (document-reference-list-set! document (dedupe (apply append (document-reference-list document) target)))
              (index-node-references-import-in-this-node-set! parent (dedupe (apply append (index-node-references-import-in-this-node parent) target)))))]
        [else '()])
      (except c
        [else '()]))))
)
