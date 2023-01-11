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
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [current-head (car expression)]
      [basic (find (lambda(r) (equal? current-head (car r))) rnrs-chez-rules)])
    (if basic
      (try
        (match-single index-node document basic)
        (except c
          [else 
            (cond
            ;;todo
              [else ]
            )
          ])))))

(define (match-single index-node document rule)
  (let* ([return-type (cadr rule)]
      [param-types (caddr rule)]
      [ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [current-head (car expression)]
      [params (cdr expression)])
    (guard-for document index-node current-head 
      '(rnrs)'(scheme)'(chezscheme)'(rnrs condition)'(rnrs base)'(rnrs files)'(rnrs syntax-case)'(rnrs exception)'(rnrs lists)'(rnrs bytevectors)'(rnrs control)'(rnrs unicode)'(rnrs enums)'(rnrs r5rs)'(rnrs eval)'(rnrs hashtables)'(rnrs sorting)'(rnrs programs)'(rnrs mutable-pairs)'(rnrs mutable-strings)'(rnrs io ports)'(rnrs io simple)'(rnrs arithmetic flonums)'(rnrs arithmetic bitwise)'(rnrs arithmetic fixnums)'(rnrs records syntactic)'(rnrs records procedure)'(rnrs records inspection)'(chezscheme csv7)'(scheme csv7))
))
)