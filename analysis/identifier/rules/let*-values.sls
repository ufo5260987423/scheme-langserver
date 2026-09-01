(library (scheme-langserver analysis identifier rules let*-values)
  (export let*-values-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; continuation
(define (private:process-formals-group document index-node formals-group exclude-list)
  (let ([formals-node (if (list? formals-group) (index-node-parent (car formals-group)) formals-group)]
      [variables (if (list? formals-group) formals-group `(,formals-group))])
    (fold-left
      (lambda (exclude-list variable-index-node)
        (let ([extended-exclude-list (append exclude-list (let-parameter-process index-node variable-index-node index-node document 'continuation))])
          (index-node-excluded-references-set! formals-node extended-exclude-list)
          extended-exclude-list))
      exclude-list
      variables)))

(define (let*-values-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? formals) init) **1) . body)
      (fold-left 
        (lambda (exclude-list formals-group)
          (private:process-formals-group document index-node formals-group exclude-list))
        '()
        (reverse formals))]
    [(:_ ((((? index-node-symbol? formals) **1) init) **1) . body)
      (fold-left 
        (lambda (exclude-list formals-group)
          (private:process-formals-group document index-node formals-group exclude-list))
        '()
        (reverse formals))]
    [else '()]))
)
