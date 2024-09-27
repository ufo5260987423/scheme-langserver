(library (scheme-langserver analysis identifier macro-expander)
  (export expand:step-by-step)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util path)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis local-expand)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (expand:step-by-step identifier-reference callee-index-node callee-document)
  (map 
    (lambda (target-index-node)
      (private:dispatch target-index-node callee-index-node callee-document))
    (private:unwrap identifier-reference)))

(define (private:dispatch index-node callee-index-node callee-document)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)]
      [first-child (car children)]
      [callee-expression (annotation-stripped (index-node-datum/annotations callee-index-node))])
    (match expression
      [(syntax-rules-head (keywords ...) clauses **1) 
        (if (root-meta-check callee-document first-child 'syntax-rules)
          (syntax->datum (eval `(syntax-case ',callee-expression ,keywords clauses)))
          '())]
      [(lambda-head ((? symbol? parameter)) _ ... (syntax-case-head like-parameter (keywords ...) clauses))
        (if (and 
            (equal? parameter like-parameter)
            (root-meta-check callee-document first-child 'lambda)
            (root-meta-check callee-document (car (index-node-children (car (reverse children)))) 'syntax-case))
          (syntax->datum (eval `(_ ... (syntax-case ,like-parameter ,keywords ,clauses))))
          '())]
      [else '()])))

(define (private:unwrap identifier-reference)
  (let* ([initial (identifier-reference-initialization-index-node identifier-reference)]
      [initial-expression (annotation-stripped (index-node-datum/annotations initial))]
      [initial-children (index-node-children initial)]
      [first-child (car initial-children)]
      [first-child-expression (annotation-stripped (index-node-datum/annotations first-child))]
      [document (identifier-reference-document identifier-reference)]
      [first-child-identifiers (find-available-references-for document initial first-child-expression)]
      [first-child-top-identifiers (map root-ancestor first-child-identifiers)])
    (map 
      (lambda (first-child-identifier)
        (case (identifier-reference-identifier first-child-identifier)
          ['define-syntax (caddr initial-children)]
          ['let-syntax (cadr (index-node-parent (identifier-reference-index-node identifier-reference)))]))
      (filter (lambda (identifier) (meta-library? (identifier-reference-library-identifier identifier))) first-child-top-identifiers))))
)
