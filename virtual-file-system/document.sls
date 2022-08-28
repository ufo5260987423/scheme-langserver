(library (scheme-langserver virtual-file-system document)
  (export 
    make-document
    document?
    document-uri
    document-text
    document-index-node
    document-text-set!
    document-index-node-set!)
  (import (rnrs))

(define-record-type document 
  (fields 
    (immutable uri)
    (mutable text)
    (mutable index-node)))
)