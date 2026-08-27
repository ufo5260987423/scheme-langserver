(library (scheme-langserver analysis identifier rules letrec)
  (export letrec-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules define)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; variable 
(define (letrec-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? vars) . vals) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) vars))
      (map 
        (lambda (var) 
          (index-node-references-import-in-this-node-set! (index-node-parent var)
            (list (index-node:regist-as-identifier-reference var index-node var #f index-node document 'variable)))) 
        vars)
      (index-node-excluded-references-set! (index-node-parent (index-node-parent (car vars)))
        (apply append (map index-node-references-export-to-other-node vars))) ]
    [else '()]))
)
