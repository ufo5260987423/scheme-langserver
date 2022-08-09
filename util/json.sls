(library (scheme-langserver util json)
    (export read-json generate-json)
    (import (arew json))

(define (read-json string)
    (json-read (open-input-string string)))

(define (generate-json alist)
    (json-write (open-output-string alist)))
)