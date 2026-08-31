(library (scheme-langserver analysis identifier rules identifier-syntax)
  (export identifier-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules syntax-case)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (identifier-syntax-process root-file-node library-file-node document index-node)
  (match-index-node index-node
    [(:_ ((? index-node-symbol? id0) . id-templates)
        (and set!-clause
          (('set! (? index-node-symbol? id1) expression0) . set!-templates)))
      (clause-process index-node document set!-clause expression0 '())]
    [else '()]))
) ; end library
