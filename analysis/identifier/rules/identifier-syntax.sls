(library (scheme-langserver analysis identifier rules identifier-syntax)
  (export identifier-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (identifier-syntax-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [('identifier-syntax ((? symbol? id0) template1 ...) (('set! (? symbol? id1) expression0) template2 ...))
          (guard-for document index-node 'identifier-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([children (index-node-children index-node)]
              [third-index-node (caddr children)]
              [third-head-index-node (car (index-node-children third-index-node))]
              [expression0-index-node (caddr (index-node-children third-head-index-node))])
            (clause-process index-node document third-index-node expression0-index-node '()))]
        [else '()])
      (except c
        [else '()]))))
)
