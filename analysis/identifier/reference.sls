(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for
    find-references-in
    guard-for

    identifier-reference?
    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-type-expressions
    identifier-reference-type
    identifier-reference-parent
    identifier-reference-type-expressions-set!
    identifier-reference-index-node
    
    sort-identifier-references
    is-pure-identifier-reference-misture?)
  (import 
    (chezscheme)

    (ufo-match)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver util binary-search)
    (scheme-langserver util contain)
    (scheme-langserver util natural-order-compare))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable identifier)
    (immutable document)
    (immutable index-node)
    (immutable library-identifier)
    (immutable type)
    (immutable parent)
    ;; each type-expression is an alist consists of identifier-references and 'or 'something? 'void? ...
    ;; NOTE: it must be index-node's type expression collection, because of case-lambda
    (mutable type-expressions)))

(define (is-pure-identifier-reference-misture? expression)
  (if (list? expression)
    (not (contain? (map is-pure-identifier-reference-misture? expression) #f))
    (identifier-reference? expression)))

(define (sort-identifier-references identifier-references)
  (sort 
    (lambda (target1 target2) 
      (natural-order-compare 
        (symbol->string (identifier-reference-identifier target1))
        (symbol->string (identifier-reference-identifier target2))))
    identifier-references))

(define (guard-for document current-index-node target-identifier . library-identifier-rest)
  (let ([candidates (find-available-references-for document current-index-node target-identifier)])
    (if (private-check-library-identifier? candidates library-identifier-rest)
      #t
      (let loop ([body 
            (filter (lambda (identifier-reference) (not (null? identifier-reference))) 
              (map identifier-reference-parent (find-available-references-for document current-index-node)))])
        (if (null? body)
          (raise "no such identifier for specific libraries")
          (if (private-check-library-identifier? body library-identifier-rest)
            #t
            (loop 
              (filter (lambda (identifier-reference) (not (null? identifier-reference))) 
                (map identifier-reference-parent body)))))))))

(define (private-check-library-identifier? candidates library-identifier-rest)
  (if (null? candidates)
    #f
    (let ([candidate (car candidates)])
      (if (null? library-identifier-rest)
        candidate
        (if (find (lambda (r) (equal? r (identifier-reference-library-identifier candidate))) library-identifier-rest)
        candidate
          #f)))))

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
              (make-identifier-reference identifier '() '() '() '() '() '()))]
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

(define (find-references-in document index-node available-references predicate?)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-expression ann)]
      [children (index-node-children index-node)])
    (match expression
      [(? predicate? maybe-symbol) 
        (let ([result 
              (find 
                (lambda (candidate-reference) 
                  (if (find (lambda (cr) (equal? cr candidate-reference)) available-references)
                    #t
                    #f))
                (find-available-references-for document index-node maybe-symbol))])
          (if result
            `(,(make-location
              (document-uri document) 
              (make-range
                (int+text->position (index-node-start index-node) (document-text document))
                (int+text->position (index-node-end index-node) (document-text document)))))
            '()))]
      [else 
        (if (null? children)
          '()
          (apply append
            (map (lambda (child-index-node) (find-references-in document child-index-node available-references predicate?)) children)))])))
)
