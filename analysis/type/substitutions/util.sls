(library (scheme-langserver analysis type substitutions util)
  (export 
    construct-lambdas-with
    construct-parameter-variable-products-with
    construct-substitutions-between-index-nodes
    substitution-compare
    add-to-substitutions
    remove-from-substitutions
    debug:pretty-print-substitution
    substitution->string)
  (import 
    (chezscheme)

    (scheme-langserver util try)
    (scheme-langserver util cartesian-product)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver virtual-file-system index-node))

(define (debug:pretty-print-substitution substitutions)
  (pretty-print (map 
    (lambda (substitution)
      (let* ([l (car substitution)]
          [r (car (reverse substitution))]
          [m (cadr substitution)]
          [r-o 
            (if (identifier-reference? r)
              (identifier-reference-identifier r)
              r)])
        `(,l ,m ,r-o)))
    substitutions)))

(define (substitution->string substitution)
  (fold-left 
    (lambda (left right)
      (string-append left (inner:type->string right) " "))
    ""
    substitution))

(define (construct-substitutions-between-index-nodes left-index-node right-index-node symbol)
  (cartesian-product `(,(index-node-variable left-index-node)) `(,symbol) `(,(index-node-variable right-index-node))))

(define (construct-parameter-variable-products-with parameter-index-nodes)
  (apply cartesian-product `((inner:list?) ,@(map list (map index-node-variable parameter-index-nodes)))))

(define (construct-lambdas-with return-variables parameter-variable-products)
  (cartesian-product return-variables '(<-) parameter-variable-products))

(define (substitution-compare item0 item1)
  (string<=?
    (variable-uuid (car item0))
    (variable-uuid (car item1))))

(define add-to-substitutions 
  (case-lambda 
    [(target) (list target)]
    [(substitutions target)
      (if (null? target)
        substitutions
        (merge substitution-compare substitutions (list target)))]))

(define (remove-from-substitutions substitutions predicator)
  (filter (lambda (substitution) (not (predicator substitution))) substitutions))
)