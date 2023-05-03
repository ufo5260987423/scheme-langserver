(library (scheme-langserver analysis package-manager akku)
  (export akku-acceptable-file?)
  (import 
    (rnrs)
    (scheme-langserver virtual-file-system file-node)
    (only (srfi :13 strings) string-contains string-index-right string-index string-take string-drop))

(define (akku-acceptable-file? path)
  (if (string-contains path "/.akku/")
    (if (string-contains path "/.akku/lib/")
      (let loop ([current-start 0] [previous ""])
        (let* ([next-/-index (string-index path (string-ref "/" 0) (+ 1 current-start))]
            [maybe-project-name (substring path current-start next-/-index)]
            [tail (string-drop path next-/-index)])
          (if (string-contains tail "/.akku/")
            (loop next-/-index maybe-project-name)
            (not (string-contains path (string-append previous "/.akku/lib" previous))))))
      #f)
    (folder-or-scheme-file? path)))
)