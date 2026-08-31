(library (scheme-langserver analysis identifier rules define-syntax)
  (export 
    define-syntax-process
    define-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; For syntax-rules: value-node is the syntax-rules index-node itself.
; For syntax-case: value-node may be (lambda (x) (syntax-case x ...)) or
; the syntax-case index-node directly.  Walk into lambda when needed.
(define (private:find-generator value-index-node)
  (match-index-node value-index-node
    [('syntax-rules . rest)
      (index-node-expansion-generator value-index-node)]
    [('syntax-case . rest)
      (index-node-expansion-generator value-index-node)]
    [('lambda params (and inner-node (or ('syntax-rules . fuzzy) ('syntax-case . fuzzy))))
      (index-node-expansion-generator inner-node)]
    [else #f]))

(define (define-syntax:attach-generator root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (? index-node-symbol? identifier) value)
      (let ([generator (private:find-generator value)])
        (when (procedure? generator)
          (for-each
            (lambda (ref)
              (identifier-reference-syntax-expander-set! ref
                (lambda x (apply generator x))))
            (index-node-references-export-to-other-node identifier))))]
    [else '()]))

(define (private:regist-syntax-variable! document identifier-node init-node import-node library-identifiers)
  (let ([reference (make-identifier-reference
                     (index-node-expression identifier-node)
                     document
                     identifier-node
                     init-node
                     library-identifiers
                     'syntax-variable
                     '()
                     '())])
    (index-node-references-export-to-other-node-set!
      identifier-node
      (append (index-node-references-export-to-other-node identifier-node) `(,reference)))
    (append-references-into-ordered-references-for document import-node `(,reference))))

(define (private:regist-syntax! document identifier-node init-node import-node library-identifiers)
  (let ([reference (make-identifier-reference
                     (index-node-expression identifier-node)
                     document
                     identifier-node
                     init-node
                     library-identifiers
                     'syntax
                     '()
                     '())])
    (index-node-references-export-to-other-node-set!
      identifier-node
      (append (index-node-references-export-to-other-node identifier-node) `(,reference)))
    (append-references-into-ordered-references-for document import-node `(,reference))))

(define (private:regist-syntax-parameter! document param-node init-node exclude-node import-node)
  (let ([reference (make-identifier-reference
                     (index-node-expression param-node)
                     document
                     param-node
                     init-node
                     '()
                     'syntax-parameter
                     '()
                     '())])
    (index-node-references-export-to-other-node-set!
      param-node
      (append (index-node-references-export-to-other-node param-node) `(,reference)))
    (index-node-references-import-in-this-node-set!
      import-node
      (sort-identifier-references
        (append (index-node-references-import-in-this-node import-node) `(,reference))))
    (index-node-excluded-references-set!
      exclude-node
      (append (index-node-excluded-references exclude-node) `(,reference)))))

(define (private:regist-parameter! document param-node init-node exclude-node import-node)
  (let ([reference (make-identifier-reference
                     (index-node-expression param-node)
                     document
                     param-node
                     init-node
                     '()
                     'parameter
                     '()
                     '())])
    (index-node-references-export-to-other-node-set!
      param-node
      (append (index-node-references-export-to-other-node param-node) `(,reference)))
    (index-node-references-import-in-this-node-set!
      import-node
      (sort-identifier-references
        (append (index-node-references-import-in-this-node import-node) `(,reference))))
    (index-node-excluded-references-set!
      exclude-node
      (append (index-node-excluded-references exclude-node) `(,reference)))
    (append-references-into-ordered-references-for document import-node `(,reference))))

(define (private:process-improper-formals formals-list-node init-node import-node document)
  (let loop ([current-node formals-list-node])
    (let ([expr (index-node-expression current-node)])
      (cond
        [(pair? expr)
          (let ([head-node (car (index-node-children current-node))]
                [tail-node (cadr (index-node-children current-node))])
            (private:regist-syntax-parameter! document head-node init-node formals-list-node import-node)
            (loop tail-node))]
        [(not (null? expr))
          (private:regist-parameter! document current-node init-node formals-list-node import-node)]
        [else '()]))))

(define (define-syntax-process root-file-node root-library-node document index-node)
  (let ([library-identifiers (get-nearest-ancestor-library-identifier index-node)])
    (match-index-node index-node
      [(:_ ((? index-node-symbol? identifier-node) (? index-node-symbol? param-nodes) ...) . body)
        (private:regist-syntax! document identifier-node index-node (index-node-parent index-node) library-identifiers)
        (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) param-nodes))
        (for-each
          (lambda (param-node)
            (private:regist-syntax-parameter! document param-node index-node (index-node-parent identifier-node) index-node))
          param-nodes)]
      [(:_ ((? index-node-symbol? identifier-node) . rest) . body)
        (private:regist-syntax! document identifier-node index-node (index-node-parent index-node) '())
        (private:process-improper-formals (index-node-parent identifier-node) index-node index-node document)]
      [(:_ (? index-node-symbol? identifier-node) . rest)
        (private:regist-syntax-variable! document identifier-node index-node (index-node-parent index-node) library-identifiers)]
      [else '()])))
) ; end library
