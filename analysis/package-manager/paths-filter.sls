(library (scheme-langserver analysis package-manager paths-filter)
  (export 
    generate-paths-file-filter)
  (import 
    (rnrs)
    (only (srfi :13 strings) string-prefix?))

(define (generate-paths-file-filter paths)
  (lambda (path) (not (not (find (lambda (s) (string-prefix? s path)) paths)))))
)