(library (scheme-langserver analysis identifier util)
  (export car*)
  (import 
    (chezscheme))

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
)