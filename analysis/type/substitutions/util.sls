(library (scheme-langserver analysis type substitutions util)
  (export 
    construct-lambdas-with
    construct-parameter-index-nodes-products-with
    do-nothing)
  (import 
    (chezscheme)

    (ufo-try)
    (scheme-langserver util cartesian-product)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver virtual-file-system index-node))

(define (do-nothing . fuzzy) '())

(define (construct-parameter-index-nodes-products-with parameter-index-nodes)
  (apply cartesian-product `((inner:list?) ,@(map list parameter-index-nodes))))

(define (construct-lambdas-with return-index-nodes parameter-index-nodes-products)
  (cartesian-product return-index-nodes '(<-) parameter-index-nodes-products))
)