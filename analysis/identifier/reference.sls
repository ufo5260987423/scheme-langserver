(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for
    guard-for

    identifier-reference?
    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-index-node)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable identifier)
    (immutable document)
    (immutable index-node)
    (immutable library-identifier)))

(define (guard-for document current-index-node target-identifier . library-identifier-rest)
  (let ([candidates (find-available-references-for document current-index-node target-identifier)])
    (if (null? candidates)
      (raise "no such identifier")
      (let ([candidate (car candidates)])
        (if (null? library-identifier-rest)
          candidate
          (if (find (lambda (r) (equal? r (identifier-reference-library-identifier candidate))) library-identifier-rest)
            candidate
            (raise "no such identifier for specific libraries")))))))

(define find-available-references-for
  (case-lambda
    [(document current-index-node)
        (filter
          (lambda (reference)
            (not (find 
                  (lambda (er) (equal? er reference))
                  (index-node-excluded-references current-index-node))))
          (if (null? (index-node-parent current-index-node))
            (append (document-reference-list document) (index-node-references-import-in-this-node current-index-node))
            (append 
              (index-node-references-import-in-this-node current-index-node) 
              (find-available-references-for document (index-node-parent current-index-node)))))]
    [(document current-index-node identifier)
      (let ([candidate-references (find-available-references-for document current-index-node)])
        (filter
          (lambda (reference)
            (equal? identifier (identifier-reference-identifier reference)))
          candidate-references))]))
)
