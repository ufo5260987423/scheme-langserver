(library (scheme-langserver analysis identifier rules begin)
  (export begin-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
(define (begin-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ bodies **1)
      (let* ([parent (index-node-parent index-node)]
          [pre-target (map index-node-references-import-in-this-node bodies)]
          [target `(,@pre-target ,(index-node-references-import-in-this-node index-node))])
        (append-references-into-ordered-references-for document parent (apply append target)))]
    [else '()]))
)
