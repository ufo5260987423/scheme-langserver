(library (scheme-langserver analysis identifier rules let-values)
  (export let-values-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; continuation
(define (private:process-formals-group document index-node formals-group)
  (let ([formals-node (if (list? formals-group) (index-node-parent (car formals-group)) formals-group)]
      [variables (if (list? formals-group) formals-group `(,formals-group))])
    (index-node-excluded-references-set! formals-node
      (apply append
        (map 
          (lambda (variable-index-node)
            (let-parameter-process index-node variable-index-node index-node document 'continuation))
          variables)))))

(define (let-values-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? formals) init) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) formals))
      (for-each 
        (lambda (formals-group)
          (private:process-formals-group document index-node formals-group))
        formals)]
    [(:_ ((((? index-node-symbol? formals) **1) init) **1) . body)
      (check-duplicate-identifiers document
        (apply append (map (lambda (group) (map (lambda (p) (cons (index-node-expression p) p)) group)) formals)))
      (for-each 
        (lambda (formals-group)
          (private:process-formals-group document index-node formals-group))
        formals)]
    [else '()]))
)
