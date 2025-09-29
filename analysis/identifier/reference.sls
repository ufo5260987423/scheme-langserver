(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for
    find-references-in
    guard-for

    meta?
    meta-for?

    append-references-into-ordered-references-for 

    identifier-reference?
    make-identifier-reference
    identifier-reference-identifier
    identifier-reference-document
    identifier-reference-library-identifier
    identifier-reference-type-expressions
    identifier-reference-type
    identifier-reference-parents
    identifier-reference-parents-set!
    identifier-reference-type-expressions-set!
    identifier-reference-index-node
    identifier-reference-initialization-index-node
    identifier-reference-top-environment

    identifier-compare?

    transform

    root-ancestor
    
    sort-identifier-references
    pure-identifier-reference-misture?
    is-ancestor-of?
    library-identifier?)
  (import 
    (chezscheme)

    (ufo-match)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver util binary-search)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable identifier)
    (immutable document)
    ;these two only mutable for transform
    (mutable index-node)
    (mutable initialization-index-node)

    (immutable library-identifier)
    (immutable type)
    ;parent can be used for two cases: 
    ;(1) rename/prefix a identifier-reference in library importion/exportion
    ;(2) record-type inherent
    (mutable parents)
    ;; each type-expression is an alist consists of identifier-references and 'or 'something? 'void? ...
    ;; NOTE: it must be index-node's type expression collection, because of case-lambda
    (mutable type-expressions)
    (mutable top-environment))
  (protocol
    (lambda (new)
      (case-lambda
        [(identifier document index-node initialization-index-node library-identifier type parents type-expressions)
          (new identifier document index-node initialization-index-node library-identifier type parents type-expressions '())]
        [(identifier document index-node initialization-index-node library-identifier type parents type-expressions top-environment)
          (new identifier document index-node initialization-index-node library-identifier type parents type-expressions top-environment)]))))

(define (is-ancestor-of? identifier-reference0 identifier-reference1)
  (if (equal? identifier-reference0 identifier-reference1)
    #t
    (if (find (lambda (parent) (is-ancestor-of? identifier-reference0 parent)) (identifier-reference-parents identifier-reference1))
      #t
      #f)))

(define (transform document origin-index-node-list target-index-node-list mapper-vector target-index-node-blacklist)
  (if (null? origin-index-node-list)
    '()
    (let* ([head (car origin-index-node-list)]
        [children (index-node-children head)]
        [imported-reference (index-node-references-import-in-this-node head)]
        [exported-reference (index-node-references-export-to-other-node head)]
        [exclude-reference (index-node-excluded-references head)])
      (if (and 
          (null? imported-reference)
          (null? exported-reference)
          (null? exclude-reference))
        '()
        (let ([target-index-node (pick-index-node-with-mapper head target-index-node-list mapper-vector)])
          (if (index-node? target-index-node)
            (begin
              (transform document children target-index-node-list mapper-vector target-index-node-blacklist)
              (transform document (cdr origin-index-node-list) target-index-node-list mapper-vector target-index-node-blacklist)
              (index-node-excluded-references-set!
                target-index-node
                (append 
                  (index-node-excluded-references target-index-node)
                  (filter 
                    (lambda (item)
                      (let ([tmp (pick-index-node-with-mapper (identifier-reference-initialization-index-node item) target-index-node-list mapper-vector)])
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
                      (let ([tmp (pick-index-node-with-mapper (identifier-reference-initialization-index-node item) target-index-node-list mapper-vector)])
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
      [initialization-index-node (identifier-reference-initialization-index-node identifier-reference)]
      [index-node (identifier-reference-index-node identifier-reference)])
    (let ([target-initialization-index-node (pick-index-node-with-mapper initialization-index-node target-index-node-list mapper-vector)]
        [target-index-node (pick-index-node-with-mapper index-node target-index-node-list mapper-vector)])
      (if (and (index-node? target-index-node) (index-node? target-initialization-index-node))
        (begin
          (identifier-reference-initialization-index-node-set! identifier-reference target-initialization-index-node)
          (identifier-reference-index-node-set! identifier-reference target-index-node)
          (index-node-references-export-to-other-node-set! 
            target-index-node
            (append 
              (index-node-references-export-to-other-node target-index-node)
              identifier-reference)))))))

(define (pure-identifier-reference-misture? expression)
  (if (list? expression) 
    (not (contain? (map pure-identifier-reference-misture? expression) #f))
    (or 
      (equal? '<- expression) 
      (equal? '<-record-set!  expression) 
      (equal? '<-record-ref  expression) 
      (equal? '<-record-constructor expression) 
      (equal? '**1 expression) 
      (equal? '... expression) 
      (equal? 'something? expression) 
      (equal? 'void? expression) 
      (equal? 'inner:list? expression) 
      (equal? 'inner:pair? expression) 
      (equal? 'inner:vector? expression) 
      (identifier-reference? expression))))

(define (meta? identifier) (not (null? (identifier-reference-top-environment identifier))))
(define (meta-for? document index-node identifier)
  (let ([e (annotation-stripped (index-node-datum/annotations index-node))])
    (if (symbol? e)
      (let* ([as (find-available-references-for document index-node e)]
          [ras (apply append (map root-ancestor as))]
          [metas (filter meta? ras)])
        (find (lambda (i) (equal? identifier (identifier-reference-identifier i))) metas)))))

(define (identifier-compare? target1 target2)
  (string<=?
    (symbol->string (identifier-reference-identifier target1))
    (symbol->string (identifier-reference-identifier target2))))

(define (append-references-into-ordered-references-for document index-node list)
  (if (null? index-node)
    (document-ordered-reference-list-set! document 
      (ordered-dedupe 
        (sort-identifier-references 
          (append (document-ordered-reference-list document) list))))
    (index-node-references-import-in-this-node-set! index-node
      (ordered-dedupe 
        (sort-identifier-references 
          (append (index-node-references-import-in-this-node index-node) list))))))

(define (sort-identifier-references identifier-references)
  (sort identifier-compare? identifier-references))

(define (guard-for document current-index-node target-identifier . library-identifier-rest)
  (let ([candidates (find-available-references-for document current-index-node target-identifier)])
    (if (private-check-library-identifier? candidates library-identifier-rest)
      #t
      (let loop ([body 
            (filter (lambda (identifier-reference) (not (null? identifier-reference))) 
              (apply append (map identifier-reference-parents (find-available-references-for document current-index-node))))])
        (if (null? body)
          (raise "no such identifier for specific libraries")
          (if (private-check-library-identifier? body library-identifier-rest)
            #t
            (loop 
              (filter (lambda (identifier-reference) (not (null? identifier-reference))) 
                (apply append (map identifier-reference-parents body))))))))))

(define (private-check-library-identifier? candidates library-identifier-rest)
  (if (null? candidates)
    #f
    (let ([candidate (car candidates)])
      (if (null? library-identifier-rest)
        candidate
        (if (find (lambda (r) (equal? r (identifier-reference-library-identifier candidate))) library-identifier-rest)
        candidate
          #f)))))

(define (library-identifier? document index-node)
  (let* ([parent (index-node-parent index-node)]
      [check? 
        (lambda () 
          (let ([grandparent (index-node-parent parent)]
              [sibling (index-node-children parent)]
              [bigest-sibling (car (index-node-children parent))])
            (if (null? grandparent)
              #f
              (and 
                (not (equal? bigest-sibling index-node))
                (contain? sibling index-node)
                (match (annotation-stripped (index-node-datum/annotations grandparent))
                  [('library _ ...) (not (equal? (cadr (index-node-children grandparent)) parent))]
                  [('define-library _ ...) (not (equal? (cadr (index-node-children grandparent)) parent))]
                  [else #f])))))])
    (if (null? parent)
      #f
      (match (annotation-stripped (index-node-datum/annotations parent))
        [('library identifier _ ...) 
          (and 
            (equal? (cadr (index-node-children parent)) index-node)
            (not (check?)))]
        [('define-library identifier _ ...) 
          (and 
            (equal? (cadr (index-node-children parent)) index-node)
            (not (check?)))]
        [('import identifier **1) (check?)]
        [('only identifier _ ...) (check?)]
        [('rename identifier _ ...) (check?)]
        [('prefix identifier _ ...) (check?)]
        [('except identifier _ ...) (check?)]
        [('alias identifier _ ...) (check?)]
        [else #f]))))

(define find-available-references-for
  (case-lambda
    [(document current-index-node)
      (let* ([local (index-node-references-import-in-this-node current-index-node)]
          [local-identifiers (map identifier-reference-identifier local)]
          [exclude (index-node-excluded-references current-index-node)])
        (filter
          (lambda (reference) (not (member reference exclude)))
          (append 
            local
            (filter 
              (lambda (reference)
                (not (member (identifier-reference-identifier reference) local-identifiers)))
              (if (null? (index-node-parent current-index-node))
                (document-ordered-reference-list document) 
                (find-available-references-for document (index-node-parent current-index-node)))))))]
    [(document current-index-node identifier) 
      (let ([expression (annotation-stripped (index-node-datum/annotations current-index-node))]
          [export-list (index-node-references-export-to-other-node current-index-node)])
        (if (and 
            (find (lambda (i) (equal? identifier (identifier-reference-identifier i))) export-list)
            (equal? expression identifier))
          '()
          (find-available-references-for document current-index-node identifier '())))]
    [(document current-index-node identifier exclude)
      (let* ([current-exclude (append exclude (index-node-excluded-references current-index-node))]
          [tmp-result
            (private-binary-search 
              (index-node-references-import-in-this-node current-index-node) 
              identifier 
              current-exclude)])
        (if (null? tmp-result)
          (if (null? (index-node-parent current-index-node))
            (private-binary-search (document-ordered-reference-list document) identifier current-exclude)
            (find-available-references-for document (index-node-parent current-index-node) identifier current-exclude))
          tmp-result))]))

(define (private-binary-search reference-list identifier exclude)
  (let ([prev
        (binary-search
          (list->vector reference-list)
          identifier-compare?
          (make-identifier-reference identifier '() '() '() '() '() '() '()))])
    (filter
      (lambda (reference)
        (not 
          (find 
            (lambda (ex-reference)
              (equal? ex-reference reference))
            exclude)))
        prev)))

(define (root-ancestor identifier-reference)
  (if (null? (identifier-reference-parents identifier-reference))
    `(,identifier-reference)
    (apply append (map root-ancestor (identifier-reference-parents identifier-reference)))))

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
                (apply make-position (document+bias->position-list document (index-node-start index-node)))
                (apply make-position (document+bias->position-list document (index-node-end index-node))))))
            '()))]
      [else 
        (if (null? children)
          '()
          (apply append
            (map (lambda (child-index-node) (find-references-in document child-index-node available-references predicate?)) children)))])))
)
