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

    identifier-reference-initialization-index-node
    
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
    ;only mutable for transform
    (mutable index-node)
    (immutable initialization-index-node)
    (immutable library-identifier)
    (immutable type)
    (immutable parent)
    ;; each type-expression is an alist consists of identifier-references and 'or 'something? 'void? ...
    ;; NOTE: it must be index-node's type expression collection, because of case-lambda
    (mutable type-expressions)))

(define (transform document origin-index-node-list target-index-node-list mapper-vector target-index-node-blacklist)
  (if (null? origin-index-node-list)
    '()
    (let* ([head (car origin-index-node-list)]
        [children (index-node-children head)]
        [imported-reference (index-node-references-import-in-this-node head)]
        [exported-reference (index-node-references-export-to-other-node head)]
        [exclude-reference (index-node-excluded-references head)])
      (document-reference-list-set! document '())
      (if (and 
          (null? imported-reference)
          (null? exported-reference)
          (null? exclude-reference))
        '()
        (let ([target-index-node (pick-index-node-with-mapper head target-index-node-list mapper-vector)])
          (if (index-node? target-index-node)
            (begin
              (map 
                (lambda (child) (transform child target-index-node-list mapper-vector))
                children)
              (transform (cdr origin-index-node-list) target-index-node-list mapper-vector)
              (index-node-excluded-references-set!
                target-index-node
                (append 
                  (index-node-excluded-references target-index-node)
                  (filter 
                    (lambda (item)
                      (let ([tmp (pick-index-node-with-mapper (identifier-reference-index-node item) target-index-node-list mapper-vector)])
                        (and 
                          (index-node? tmp) 
                          (not (contain? target-index-node-blacklist tmp)) 
                          (find (lambda (p) (is-ancestor? p item)) target-index-node-blacklist))))
                    exclude-reference)))
              (index-node-references-import-in-this-node-set!
                target-index-node
                (append 
                  (index-node-references-import-in-this-node target-index-node)
                  (filter 
                    (lambda (item)
                      (let ([tmp (pick-index-node-with-mapper (identifier-reference-index-node item) target-index-node-list mapper-vector)])
                        (and 
                          (index-node? tmp) 
                          (not (contain? target-index-node-blacklist tmp)) 
                          (find (lambda (p) (is-ancestor? p item)) target-index-node-blacklist))))
                    imported-reference)))
              (map 
                (lambda (item) (private-export-transform item document target-index-node-list mapper-vector))
                exported-reference))
            '()))))))

;only for export identifier-references
(define (private-export-transform identifier-reference location-document target-index-node-list mapper-vector)
  (let ([document (identifier-reference-document identifier-reference)]
      [index-node (identifier-reference-index-node identifier-reference)])
    (let ([target-index-node (pick-index-node-with-mapper index-node target-index-node-list mapper-vector)])
      (if (index-node? target-index-node)
        (begin
          (identifier-reference-index-node-set! identifier-reference target-index-node)
          (index-node-references-export-to-other-node-set! 
            target-index-node
            (append 
              (index-node-references-export-to-other-node target-index-node)
              identifier-reference)))))))

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
              (make-identifier-reference identifier '() '() '() '() '() '() '()))]
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
