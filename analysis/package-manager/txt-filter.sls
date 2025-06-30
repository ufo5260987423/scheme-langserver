(library (scheme-langserver analysis package-manager txt-filter)
  (export generate-txt-file-filter)
  (import 
    (chezscheme)
    (scheme-langserver util io)
    (scheme-langserver virtual-file-system file-node)
    (only (srfi :13 strings) string-suffix? string-prefix? string-contains string-index-right string-index string-take string-drop string-drop-right))

(define (generate-txt-file-filter list-path)
  (lambda (path)
    (pretty-print `(DEBUG: ,path)) 
    (cond
      [(string-contains path "akku") #f]
      [(string-suffix? ".scm.txt" path) #t]
      [(file-directory? path) #t]
      [else #f])))
)