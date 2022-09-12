(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for

    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-index-node)
  (import 
    (rnrs)
    (scheme-langserver virtual-file-system index-node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable identifier)
    (immutable document)
    (immutable index-node)
    (immutable library-identifier)))

(define find-available-references-for
  (case-lambda
    [(current-index-node)
      (if (not (null? (index-node-parent current-index-node)))
        (append 
          (index-node-references-import-in-this-node current-index-node) 
          (find-available-references-for (index-node-parent current-index-node))))]
    [(current-index-node identifier)
      (let ([candiate-references (find-available-references-for current-index-node)])
        (filter
          (lambda (reference)
            (equal? identifier (identifier-reference-identifier reference)))
          candiate-references))]))
)