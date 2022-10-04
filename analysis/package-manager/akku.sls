(library (scheme-langserver analysis package-manager akku)
  (export akku-acceptable-file?)
  (import 
    (rnrs)
    (scheme-langserver virtual-file-system file-node)
    (only (srfi :13 strings) string-contains))

(define (akku-acceptable-file? path)
  (and 
    (not 
      (or
        (string-contains path "/.akku/src/")
        (string-contains path "/.akku/notices/")
        (string-contains path "/.akku/bin/")))
    (folder-or-scheme-file? path)))
)