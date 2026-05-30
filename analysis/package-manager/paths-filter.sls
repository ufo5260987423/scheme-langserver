(library (scheme-langserver analysis package-manager paths-filter)
  (export 
    generate-paths-file-filter)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system file-node))

(define (generate-paths-file-filter root paths)
  (lambda (path) (not (not (find (lambda (s) (equal? s path)) `(,root . ,paths))))))
)