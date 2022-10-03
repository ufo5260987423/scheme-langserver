(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for
    guard-for

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

(define (guard-for current-index-node identifier . rest)
  (let ([candidates (find-available-references-for current-index-node identifier)])
    (if (null? candidates)
      (raise "no such identifier")
      (let ([candidate (car candidates)])
        (if (null? rest)
          candidate
          (if (find (lambda (r) (equal? r (identifier-reference-library-identifier candidate))) rest)
            candidate
            (raise "no such identifier")))))))

(define find-available-references-for
  (case-lambda
    [(current-index-node)
        (filter
          (lambda (reference)
            (not (find 
                  (lambda (er) (equal? (identifier-reference-identifier er) (identifier-reference-identifier reference)))
                  (index-node-excluded-references current-index-node))))
          (if (null? (index-node-parent current-index-node))
            (index-node-references-import-in-this-node current-index-node) 
            (append 
              (index-node-references-import-in-this-node current-index-node) 
              (find-available-references-for (index-node-parent current-index-node)))))]
    [(current-index-node identifier)
      (let ([candiate-references (find-available-references-for current-index-node)])
        (filter
          (lambda (reference)
            (equal? identifier (identifier-reference-identifier reference)))
          candiate-references))]))
)