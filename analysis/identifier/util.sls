(library (scheme-langserver analysis identifier util)
  (export 
    car*
    library-identifier->string

    check-duplicate-identifiers
    collect-parameter-pairs
    dereference-index-node

    check-duplicate-bindings
    check-duplicate-values-bindings
    check-duplicate-syntax-bindings)
  (import 
    (chezscheme)
    (only (srfi :13) string-trim-both)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node))

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
(define (library-identifier->string l)
  (string-trim-both (with-output-to-string (lambda () (pretty-print l)))))

(define (check-duplicate-identifiers document identifier-index-node-pairs)
  (let loop ([rest identifier-index-node-pairs] [seen '()])
    (if (null? rest)
      '()
      (let* ([pair (car rest)]
          [sym (car pair)]
          [node (cdr pair)])
        (if (and (symbol? sym) (find (lambda (s) (eq? s sym)) seen))
          (append-new-diagnoses document 
            `(,(index-node-start node) ,(index-node-end node) 1 
              ,(string-append "Duplicate identifier: " (symbol->string sym)) 
              "identifier" "duplicate-identifier")))
        (loop (cdr rest) (if (symbol? sym) (cons sym seen) seen))))))

(define (dereference-index-node index-node)
  (or (index-node-shared-reference index-node) index-node))

(define (collect-parameter-pairs param-list-node)
  (let ([param-list-node (dereference-index-node param-list-node)])
    (let ([expression (annotation-stripped (index-node-datum/annotations param-list-node))])
      (cond
        [(symbol? expression) `(,(cons expression param-list-node))]
        [(and (pair? expression) (list? expression))
          (fold-left
            (lambda (acc child)
              (let ([sym (annotation-stripped (index-node-datum/annotations child))])
                (if (symbol? sym) (cons (cons sym child) acc) acc)))
            '()
            (index-node-children param-list-node))]
        [(pair? expression)
          (let ([children (index-node-children param-list-node)])
            (if (= (length children) 2)
              (append 
                (collect-parameter-pairs (car children))
                (if (index-node-shared-reference (cadr children))
                  '()
                  (collect-parameter-pairs (cadr children))))
              '()))]
        [else '()]))))

(define (check-duplicate-bindings document binding-nodes)
  (check-duplicate-identifiers document
    (map (lambda (b)
           (let* ([b (dereference-index-node b)]
                  [ident-node (car (index-node-children b))])
             (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node)))
         binding-nodes)))

(define (check-duplicate-values-bindings document binding-nodes)
  (check-duplicate-identifiers document
    (apply append
      (map 
        (lambda (b)
          (let* ([b (dereference-index-node b)]
                 [formals-node (dereference-index-node (car (index-node-children b)))])
            (map (lambda (ident-node) (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node))
              (index-node-children formals-node))))
          binding-nodes))))

(define (check-duplicate-syntax-bindings document syntax-parameter-index-nodes)
  (check-duplicate-identifiers document
    (map (lambda (ident-node)
           (let ([ident-node (dereference-index-node ident-node)])
             (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node)))
         syntax-parameter-index-nodes)))
)
