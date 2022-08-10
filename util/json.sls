(library (scheme-langserver util json)
    (export read-json generate-json)
    (import (arew json) (chezscheme))

(define (read-json string)
    (json-read (open-input-string string)))

(define (generate-json alist)
    (let ([port (open-output-string)])
        (json-write alist port)
        (get-output-string port)))
)