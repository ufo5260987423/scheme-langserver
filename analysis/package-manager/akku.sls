(library (scheme-langserver analysis package-manager akku)
  (export akku-acceptable-file?)
  (import 
    (rnrs)
    (scheme-langserver virtual-file-system file-node)
    (only (srfi :13 strings) string-contains string-index-right string-take string-drop))

(define (akku-acceptable-file? path)
  (and 
    (not 
      (or
        (string-contains path "/.akku/src/")
        (string-contains path "/.akku/notices/")
        (string-contains path "/.akku/bin/")
        (if (string-contains path "/.akku")
          (let loop ([current-path path])
            (let* ([char (string-ref "/" 0)]
                [index (string-index-right current-path char)]
                [tail (string-drop current-path index)]
                [rest (string-take current-path index)]
                [maybe-project-name (string-drop rest (string-index-right rest char))])
              (if (string-contains tail ".akku")
                (or 
                  (string-contains path (string-append "/.akku/" maybe-project-name "/"))
                  (string-contains path (string-append "/.akku/" maybe-project-name ".")))
                #f))) 
          #f)))
    (folder-or-scheme-file? path)))
)