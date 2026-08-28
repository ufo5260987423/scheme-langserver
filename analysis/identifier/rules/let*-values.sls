(library (scheme-langserver analysis identifier rules let*-values)
  (export let*-values-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; continuation
(define (let*-values-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((or (? index-node-symbol? formals) ((? index-node-symbol? formals) **1)) init) **1) . body)
      (fold-left 
        (lambda (exclude-list formals-group)
          (let ([formals-node (if (list? formals-group) (index-node-parent (car formals-group)) formals-group)]
                [variables (if (list? formals-group) formals-group `(,formals-group))])
            (fold-left
              (lambda (exclude-list variable-index-node)
                (let ([extended-exclude-list (append exclude-list (let-parameter-process index-node variable-index-node index-node document 'continuation))])
                  (index-node-excluded-references-set! formals-node extended-exclude-list)
                  extended-exclude-list))
              exclude-list
              variables)))
        '()
        (reverse formals))]
    [else '()]))
)
