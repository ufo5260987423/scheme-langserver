(library (scheme-langserver analysis identifier rules with-syntax)
  (export with-syntax-process)
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
(define (with-syntax-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ ((pattern expression) **1) body ...)
          (let* ([pattern-expression-index-node (cadr (index-node-children index-node))])
            (let loop ([pattern-expressions (index-node-children pattern-expression-index-node)])
              (if (not (null? pattern-expressions))
                (begin
                  (clause-process 
                    index-node
                    document 
                    (cadr (index-node-children (car pattern-expressions))) 
                    (car (index-node-children (car pattern-expressions))) 
                    '())
                  (loop (cdr pattern-expressions))))))]
        [else '()])
      (except c
        [else '()]))))
)
