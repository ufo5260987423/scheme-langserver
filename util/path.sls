(library (scheme-langserver util path)
    (export 
      path->uri 
      uri->path 
      uri-is-path? 
      uri->name)
    (import 
      ; (rnrs) 
      ; (only (chezscheme) path-absolute? current-directory)
      (chezscheme)
      (scheme-langserver util environment) 
      ; (only (scheme-langserver util environment) windows?)
      (only (srfi :13 strings) string-prefix? string-suffix? string-drop)
      )

(define (path->uri path)
  (string-append
   "file://"
   (if (path-absolute? path)
     path
     ;; FIXME: breaks for any non-trivial relative path i.e. ones with dot(s)
     ;;        also assumes (getcwd) to be absolute (which might not be true?)
     (let ((pwd (current-directory)))
       (if (equal? path ".") ;; special case "." so at least that works
         pwd
         (string-append pwd "/" path))))))

(define (uri->path uri)
  (cond
    [(windows?)
     ;; If a file URI begins with file:// or file:////, Windows translates it
     ;; as a UNC path. If it begins with file:///, it's translated to an MS-DOS
     ;; path. (https://en.wikipedia.org/wiki/File_URI_scheme#Windows_2)
     (cond
       [(string-prefix? uri "file:////") (substring uri 0 7)]
       [(string-prefix? uri "file:///") (substring uri 0 8)]
       [else (string-append "//" (substring uri 0 7))])]
    [else (substring uri 0 7)]))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->name uri path)
  ;; Find longest matching prefix of uri in path and use remaining part of uri
  (string-drop
   uri
   (+ 1 (string-length
         (fold-left
          (lambda (prefix best-prefix)
            (if (and (> (string-length prefix) (string-length best-prefix))
                     (string-prefix? prefix uri))
              prefix
              best-prefix))
          "file://"
          path)))))
)