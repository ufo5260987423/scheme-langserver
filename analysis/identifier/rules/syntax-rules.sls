(library (scheme-langserver analysis identifier rules syntax-rules)
  (export syntax-rules-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules syntax-case)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (syntax-rules-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ ((? index-node-symbol? literal) ...) . clauses)
      (let ([literals (map index-node-expression literal)])
        (map 
          (lambda (clause-index-node)
            (clause-process index-node document clause-index-node (car (index-node-children clause-index-node)) literals))
          clauses))]
    [else '()]))
)
