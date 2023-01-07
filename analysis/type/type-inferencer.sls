(library (scheme-langserver analysis type type-inferencer)
  (export 
    match)
  (import 
    (chezscheme)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis type meta-type)
    (scheme-langserver analysis type rnrs-meta-rules))

(define (match index-node document)
)

(define (match-single index-node document rule)
  (let* ([rule-head (car rule)]
      [return-type (cadr rule)]
      [param-type (caddr rule)]
      [ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [head (car expression)])
    (if (equal? head rule-head)

      '())))
)