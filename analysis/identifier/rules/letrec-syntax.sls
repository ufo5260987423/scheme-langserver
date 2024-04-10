(library (scheme-langserver analysis identifier rules letrec-syntax)
  (export letrec-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-variable 
(define (letrec-syntax-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (guard-for document index-node 'letrec-syntax '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let loop ([exclude '()]
                [rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (loop (append exclude (let-parameter-process index-node identifier-index-node index-node exclude document 'syntax-variable)) (cdr rest)))))]
        [else '()])
      (except c
        [else '()]))))
)
