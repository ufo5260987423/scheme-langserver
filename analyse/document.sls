(library (scheme-langserver analyse document)
  (export 
    make-document
    document?
    document-uri
    document-text)
  (import (rnrs) )

(define-record-type document 
  (fields 
    (immutable uri)
    (mutable text)))
)