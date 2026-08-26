(library (scheme-langserver analysis identifier rules do)
  (export do-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; variable 
(define (do-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? var-index-nodes) . fuzzy) **1) body ... )
      (check-duplicate-identifiers document (map (lambda (v) (cons (index-node-expression v) v)) var-index-nodes))
      (map 
        (lambda (var-index-node)
          (index-node-references-export-to-other-node-set! var-index-node
            `(,(make-identifier-reference 
                (index-node-expression var-index-node)
                  document
                  var-index-node
                  index-node
                  '()
                  'variable
                  '()
                  '()))))
        var-index-nodes)
      (let ([all-references (apply append (map index-node-references-export-to-other-node var-index-nodes))])
        (append-references-into-ordered-references-for document index-node all-references)
        (map 
          (lambda (s)
            (index-node-excluded-references-set! s all-references))
          (map car (filter (lambda (s) (not (null? s))) fuzzy))))]
    [else '()]))
)
