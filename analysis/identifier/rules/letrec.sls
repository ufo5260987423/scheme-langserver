(library (scheme-langserver analysis identifier rules letrec)
  (export letrec-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; variable 
(define (letrec-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (fuzzy0 **1 ) fuzzy1 ... ) 
        (let ([binding-nodes (filter 
                  (lambda (i) (not (null? (index-node-children i)))) 
                  (index-node-children (cadr (index-node-children index-node))))])
          (check-duplicate-bindings document binding-nodes)
          (fold-left 
            (lambda (exclude-list identifier-parent-index-node)
              (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                  [target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'variable)]
                  [extended-exclude-list (append exclude-list target-identifier-reference)])
                (index-node-excluded-references-set! (cadr (index-node-children index-node)) extended-exclude-list)
                (append-references-into-ordered-references-for document identifier-index-node target-identifier-reference)
                extended-exclude-list))
            '()
            binding-nodes))]
      [else '()])))
)
