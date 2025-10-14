(library (scheme-langserver analysis identifier rules syntax-rules)
  (export syntax-rules-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util contain)
    (scheme-langserver util dedupe)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules syntax-case)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (syntax-rules-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (literals ...) (a b ...) **1) 
      ; https://www.scheme.com/tspl4/syntax.html
      ; Any syntax-rules form can be expressed with syntax-case by making the lambda expression and syntax expressions explicit.
        (let* ([children (index-node-children index-node)]
            [rest (cddr children)])
          (map 
            (lambda (clause-index-node)
              (clause-process index-node document clause-index-node (car (index-node-children clause-index-node)) literals))
            rest))]
      [else '()])))
)
