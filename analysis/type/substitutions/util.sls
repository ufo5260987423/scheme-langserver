(library (scheme-langserver analysis type substitutions util)
  (export 
    construct-lambdas-with
    construct-parameter-index-nodes-products-with
    construct-rest-parameter-template
    do-nothing)
  (import 
    (chezscheme)
    (scheme-langserver util cartesian-product)
    (scheme-langserver virtual-file-system index-node))

(define (do-nothing . _) '())

(define (construct-parameter-index-nodes-products-with parameter-index-nodes)
  (apply cartesian-product `((inner:list?) ,@(map list parameter-index-nodes))))

(define (construct-lambdas-with return-index-nodes parameter-index-nodes-products)
  (cartesian-product return-index-nodes '(<-) parameter-index-nodes-products))

;Collect the parameter template for a REST-bearing formal list, shared
;by the lambda / define / case-lambda rules. `children` is the parameter
;index-node list: for lambda and case-lambda it is the formals node's
;children ((a b . rest) or a bare-symbol clause formals, whose children
;are empty); for define it is the signature node's children AFTER the
;name. The rest parameter binds the remaining arguments, so the result
;is an elastic something? tail on the parameter template -- NOT a
;nested (inner:list? something? ...) item (that shape reads "one list
;argument" and does not even satisfy inner:lambda?). The tail marker
;makes the arrow variadic: (inner:list? fixed... something? ...).
(define (construct-rest-parameter-template children)
  (let loop ([rest children] [acc '()])
    (cond
      [(or (null? rest)
          ;;the last child of a dotted formals list is the rest symbol;
          ;;an empty list covers the bare-symbol clause formals
          (and (null? (cdr rest))
            (symbol? (annotation-stripped (index-node-datum/annotations (car rest))))))
        (cons 'inner:list? (append (reverse acc) '(something? ...)))]
      [else (loop (cdr rest) (cons (car rest) acc))])))
)