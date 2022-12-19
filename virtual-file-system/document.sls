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
    document-reference-list-set!

    with-document-read
    with-document-write)
  (import 
    (rnrs)
    (scheme-langserver util synchronize))

(define-syntax with-document-write
    (syntax-rules () [(_ document e0 e1 ...) (with-lock-write(document-lock document) e0 e1 ...) ]))

(define-syntax with-document-read
    (syntax-rules () [(_ document e0 e1 ...) (with-lock-read (document-lock document) e0 e1 ...) ]))

(define-record-type document 
  (fields 
    (immutable uri)
    (mutable text)
    (mutable index-node-list)
    (mutable reference-list)
    (immutable lock))
  (protocol
    (lambda (new)
      (lambda (uri text index-node-list reference-list)
        (new uri text index-node-list reference-list(make-reader-writer-lock))))))
)
