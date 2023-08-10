(library (scheme-langserver analysis type domain-specific-language interpreter)
  (export type:interpret)
  (import 
    (chezscheme)
    (ufo-match)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type domain-specific-language syntax-candy))

(define type:interpret 
  (case-lambda 
    [(expression substitution-list)
      (cond
        [(inner:executable? expression)
          (match expression
            [((? inner:lambda? l) params ...)
              (if (inner:list? (inner:lambda-param l))
                  (if (candy:matchable? (cdr (inner:lambda-param l)) (map type:interpret params))
                    (inner:lambda-return l)
                    '())
                (inner:lambda-return l))]
            ;todo
            ; [((? inner:record-lambda? l) params ...) ]
            [else expression])]
        [(list? expression) (map type:interpret expression)]
        [(inner:trivial? expression) expression]
        [(equal? 'list? expression) 'list?]
        [(equal? 'record? expression) 'reocrd?]
        [(equal? 'vector? expression) 'vector?]
        [(equal? 'pair? expression) 'pair?])]
    [(expression)
      (type:interpret expression '()) ]))
)