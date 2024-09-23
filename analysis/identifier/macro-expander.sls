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

(define (expand:step-by-step identifier-reference callee)
  (map 
    (lambda (current-identifier)
      (private:dispatch current-identifier callee))
    (private:unwrap identifier-reference)))

(define (private:dispatch identifier)
  '())

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
