(library (scheme-langserver analysis identifier rules body)
  (export body-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; variable 
(define (body-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ fuzzy ... ) 
          (let* ([parent (index-node-parent index-node)]
              [children (index-node-children index-node)])
            ((lambda (t) (if (null? parent) (document-reference-list-set! document t) (index-node-references-import-in-this-node-set! index-node t)))
              (apply append 
                (if (null? parent) (document-reference-list document) parent)
                (map index-node-references-import-in-this-node children))))]
        [else '()])
      (except c
        [else '()]))))
)
