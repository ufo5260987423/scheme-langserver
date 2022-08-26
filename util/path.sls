(library (scheme-langserver util path)
    (export 
      path->uri 
      uri->path 
      uri-is-path? 
      uri->name
      path->name)
    (import 
      ; (rnrs) 
      ; (only (chezscheme) path-absolute? current-directory)
      (only (chibi pathname) path-strip-directory)
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
       [(string-prefix? "file:////" uri) (string-drop uri 8)]
       [(string-prefix? "file:///" uri) (string-drop uri 7)])]
    [else (string-drop uri 7)]))

(define (uri-is-path? str)
  (string-prefix? str "file://"))

(define (uri->name uri)
  (path->name (uri->path uri)))

(define (path->name path)
  (path-strip-directory path))
)