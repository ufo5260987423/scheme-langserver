(library (scheme-langserver analysis identifier rules let*-values)
  (export let*-values-process)
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
; continuation
(define (let*-values-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (fold-left 
            (lambda (exclude-list identifier-parent-index-node)
              (fold-left 
                (lambda (exclude-list identifier-index-node)
                  (let ([extended-exclude-list (append exclude-list (let-parameter-process index-node identifier-index-node index-node exclude-list document 'continuation))])
                    (index-node-excluded-references-set! (index-node-parent identifier-parent-index-node) extended-exclude-list)
                    extended-exclude-list))
                exclude-list
                (index-node-children (car (index-node-children identifier-parent-index-node)))))
            '()
            (reverse (index-node-children (cadr (index-node-children index-node)))))]
        [else '()])
      (except c
        [else '()]))))
)
