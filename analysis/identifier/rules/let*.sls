(library (scheme-langserver analysis identifier rules let*)
  (export let*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; variable
(define (let*-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? vars) . vals) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) vars))
      (fold-left
        (lambda (exclude-set var)
          (index-node:regist-as-identifier-reference var index-node var #f index-node document 'variable)
          (let ([extended-exclude-set (append exclude-set (index-node-references-export-to-other-node var))]
              [binding-node (index-node-parent var)])
            (index-node-excluded-references-set! binding-node extended-exclude-set)
            extended-exclude-set))
        '()
        (reverse vars))]
    [else '()]))
)
