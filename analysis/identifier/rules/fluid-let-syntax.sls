(library (scheme-langserver analysis identifier rules fluid-let-syntax)
  (export fluid-let-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; procedure parameter variable syntax-variable let-loop
(define (fluid-let-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (let loop ([rest (index-node-children (cadr (index-node-children index-node)))])
            (if (not (null? rest))
              (let* ([identifier-parent-index-node (car rest)]
                    [identifier-index-node (car (index-node-children identifier-parent-index-node))])
                (index-node-excluded-references-set! 
                  identifier-parent-index-node
                  (append 
                    (index-node-excluded-references identifier-parent-index-node)
                    (let-parameter-process index-node identifier-index-node index-node '() document 'syntax-variable)))
                (loop (cdr rest)))))]
        [else '()])
      (except c
        [else '()]))))
)
