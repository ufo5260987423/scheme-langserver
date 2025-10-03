(library (scheme-langserver analysis identifier util)
  (export 
    car*
    library-identifier->string)
  (import 
    (chezscheme)
    (only (srfi :13) string-trim))

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
(define (library-identifier->string l)
  (string-trim (with-output-to-string (lambda () (pretty-print l)))))
)