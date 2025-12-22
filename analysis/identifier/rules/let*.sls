(library (scheme-langserver analysis identifier rules let*)
  (export let*-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; variable
(define (let*-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (fuzzy0 **1 ) fuzzy1 ... ) 
        (fold-left 
          (lambda (exclude-list identifier-parent-index-node)
            (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                [extended-exclude-list (append exclude-list (let-parameter-process index-node identifier-index-node index-node exclude-list document 'variable))])
              (index-node-excluded-references-set! identifier-parent-index-node extended-exclude-list)
              extended-exclude-list))
          '()
          (filter 
            (lambda (i) (not (null? (index-node-children i)))) 
            (reverse (index-node-children (cadr (index-node-children index-node))))))]
      [else '()])))
)
