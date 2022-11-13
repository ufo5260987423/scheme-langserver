(library (scheme-langserver virtual-file-system document)
  (export 
    make-document
    document?
    document-uri
    document-text
    document-text-set!
    document-index-node-list
    document-index-node-list-set!
    document-reference-list
    document-reference-list-set!)
  (import (rnrs))

(define-record-type document 
  (fields 
    (immutable uri)
    (mutable text)
    (mutable index-node-list)
    (mutable reference-list)))
)