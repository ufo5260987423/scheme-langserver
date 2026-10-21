(library (scheme-langserver analysis identifier rules define-syntax)
  (export 
    define-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

(define (private:regist-identifier-reference! document identifier-node init-node import-node exclude-node library-identifiers type ordered-target)
  (let ([reference (make-identifier-reference
                     (index-node-expression identifier-node)
                     document
                     identifier-node
                     init-node
                     library-identifiers
                     type
                     '()
                     '())])
    (index-node-references-export-to-other-node-set!
      identifier-node
      (append (index-node-references-export-to-other-node identifier-node) `(,reference)))
    (when exclude-node
      (index-node-excluded-references-set!
        exclude-node
        (append (index-node-excluded-references exclude-node) `(,reference))))
    (when import-node
      (index-node-references-import-in-this-node-set!
        import-node
        (sort-identifier-references
          (append (index-node-references-import-in-this-node import-node) `(,reference)))))
    (when ordered-target
      (append-references-into-ordered-references-for document ordered-target `(,reference)))
    reference))

(define (private:process-improper-formals formals-list-node init-node import-node document)
  (let loop ([current-node formals-list-node])
    (let ([expr (index-node-expression current-node)])
      (cond
        [(pair? expr)
          (let ([head-node (car (index-node-children current-node))]
                [tail-node (cadr (index-node-children current-node))])
            (private:regist-identifier-reference! document head-node init-node import-node formals-list-node '() 'syntax-parameter #f)
            (loop tail-node))]
        [(not (null? expr))
          (private:regist-identifier-reference! document current-node init-node import-node formals-list-node '() 'parameter import-node)]
        [else '()]))))

(define (define-syntax-process root-file-node root-library-node document index-node)
  (let ([library-identifiers (get-nearest-ancestor-library-identifier index-node)])
    (match-index-node index-node
      [(:_ ((? index-node-symbol? identifier-node) (? index-node-symbol? param-nodes) ...) . body)
        (private:regist-identifier-reference! document identifier-node index-node #f #f library-identifiers 'syntax (index-node-parent index-node))
        (check-duplicate-identifiers document (collect-parameter-pairs (index-node-parent identifier-node)))
        (for-each
          (lambda (param-node)
            (private:regist-identifier-reference! document param-node index-node index-node (index-node-parent identifier-node) '() 'syntax-parameter #f))
          param-nodes)]
      [(:_ ((? index-node-symbol? identifier-node) . rest) . body)
        (private:regist-identifier-reference! document identifier-node index-node #f #f '() 'syntax (index-node-parent index-node))
        (private:process-improper-formals (index-node-parent identifier-node) index-node index-node document)]
      [(:_ (? index-node-symbol? identifier-node) . rest)
        (private:regist-identifier-reference! document identifier-node index-node #f #f library-identifiers 'syntax-variable (index-node-parent index-node))]
      [else '()])))
) ; end library
