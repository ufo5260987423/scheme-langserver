(library (scheme-langserver analysis type domain-specific-language keywords)
  (export 
    keyword:apply)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define (keyword:apply . rest)
  (match rest
    [((? inner:lambda? lambda-template) params ...) 
      (let ([param-type (inner:lambda-param lambda-template)]
          [return-type (inner:lambda-return lambda-template)])
        (if (candy-matchable? param-type params)
          return-type
          (raise "keyword:apply error!")
        ))]
    [else (raise "keyword:apply error!")]))
)