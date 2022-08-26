(library (scheme-langserver analyse document)
  (export 
    init-document
    document?
    document-uri
    document-text
    document-index-node
    document-text-set!
    document-index-node-set!)
  (import (rnrs)
    (scheme-langserver util io)
    (scheme-langserver util path)
    (scheme-langserver analyse index))

(define-record-type document 
  (fields 
    (immutable uri)
    (mutable text)
    (mutable index-node)))

(define (init-document uri)
  (let ([path (uri->path uri)])
    (make-document 
      uri 
      (read-string path) 
      (init-index-node '() (source-file->annotation path)))))
)