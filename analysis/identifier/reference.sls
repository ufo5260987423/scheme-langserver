(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for
    guard-for

    identifier-reference?
    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-type-expressions
    identifier-reference-type
    identifier-reference-type-expressions-set!
    identifier-reference-index-node
    
    sort-identifier-references)
  (import 
    (chezscheme)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver util binary-search)
    (scheme-langserver util natural-order-compare))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable identifier)
    (immutable document)
    (immutable index-node)
    (immutable library-identifier)
    (immutable type)
    ;; each type-expression is an alist consists of identifier-references and 'or 'something? 'void? ...
    ;; NOTE: it must be index-node's type expression collection, because of case-lambda
    (mutable type-expressions)))

(define (sort-identifier-references identifier-references)
  (sort 
    (lambda (target1 target2) 
      (natural-order-compare 
        (symbol->string (identifier-reference-identifier target1))
        (symbol->string (identifier-reference-identifier target2))))
    identifier-references))

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
      (find-available-references-for document current-index-node identifier '())]
    [(document current-index-node identifier exclude)
      (let* ([current-exclude (append exclude (index-node-excluded-references current-index-node))]
          [tmp-result0
            (binary-search
              (list->vector (index-node-references-import-in-this-node current-index-node))
              (lambda (reference0 reference1)
                (natural-order-compare 
                  (symbol->string (identifier-reference-identifier reference0))
                  (symbol->string (identifier-reference-identifier reference1))))
              (make-identifier-reference identifier '() '() '() '() '()))]
          [tmp-result 
            (filter
              (lambda (reference)
                (not 
                  (find 
                    (lambda (ex-reference)
                      (equal? ex-reference reference))
                    current-exclude)))
              tmp-result0)])
        (if (null? tmp-result)
          (if (not (null? (index-node-parent current-index-node)))
            (find-available-references-for document (index-node-parent current-index-node) identifier current-exclude)
            '())
          tmp-result))]))
)
