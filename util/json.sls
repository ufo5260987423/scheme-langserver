(library (scheme-langserver util json)
  (export read-json generate-json)
  (import 
    (chezscheme)
    (srfi-180))

(define (read-json string)
  (json-read (open-input-string string)))

(define (generate-json alist)
  (let ([port (open-output-string)])
    (json-write alist port)
    (get-output-string port)))
)