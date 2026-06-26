(library (scheme-langserver analysis package-manager txt-filter)
  (export generate-txt-file-filter)
  (import 
    (chezscheme)
    (only (srfi :13 strings) string-suffix?))

(define (generate-txt-file-filter)
  (lambda (path)
    (cond
      [(file-directory? path) #t]
      [(string-suffix? ".scm.txt" path) #t]
      [else #f])))
)