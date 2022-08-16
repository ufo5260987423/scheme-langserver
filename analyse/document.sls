(library (scheme-langserver analyse document)
  (export 
    collect-document-libraries
    init-document)
  (import (rnrs) )

(define-record-type document 
  (fileds 
    (immutable uri)
    (mutable text)))
)