(library (scheme-langserver analysis identifier util)
  (export 
    car*
    library-identifier->string

    check-duplicate-bindings
    check-duplicate-values-bindings
    check-duplicate-syntax-bindings)
  (import 
    (chezscheme)
    (only (srfi :13) string-trim-both)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver virtual-file-system index-node))

(define (car* pair)
  (if (pair? pair)
    (car* (car pair))
    pair))
(define (library-identifier->string l)
  (string-trim-both (with-output-to-string (lambda () (pretty-print l)))))

(define (check-duplicate-bindings document binding-nodes)
  (check-duplicate-identifiers document
    (map (lambda (b)
           (let ([ident-node (car (index-node-children b))])
             (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node)))
         binding-nodes)))

(define (check-duplicate-values-bindings document binding-nodes)
  (check-duplicate-identifiers document
    (apply append
      (map 
        (lambda (b)
          (let ([formals-node (car (index-node-children b))])
            (map (lambda (ident-node) (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node))
              (index-node-children formals-node))))
          binding-nodes))))

(define (check-duplicate-syntax-bindings document syntax-parameter-index-nodes)
  (check-duplicate-identifiers document
    (map (lambda (ident-node)
           (cons (annotation-stripped (index-node-datum/annotations ident-node)) ident-node))
         syntax-parameter-index-nodes)))
)
