(library (scheme-langserver analysis identifier expanders syntax-case)
  (export 
    syntax-case->generator:map+expansion)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver analysis identifier expanders pattern)
    (scheme-langserver analysis identifier expanders syntax-rules)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver analysis identifier reference))

; syntax-case templates are wrapped in (syntax ...) or (quasisyntax ...).
; Unwrap layers so make-pattern sees the bare template datum.
(define (private:unwrap-syntax template-expr)
  (cond
    [(and (list? template-expr) 
          (memq (car template-expr) '(syntax quasisyntax unsyntax)))
      (private:unwrap-syntax (cadr template-expr))]
    [else template-expr]))

; input-index-node is supposed have the form of
; `(syntax-case to-match (literals ...) clauses ...)`
(define (syntax-case->generator:map+expansion root-file-node root-library-node document input-index-node)
  (match (annotation-stripped (index-node-datum/annotations input-index-node))
    [(_ to-match (literals ...) clauses **1) 
      (let ([clause-index-nodes (cdddr (index-node-children input-index-node))])
        (make-generator-for-clauses input-index-node literals clause-index-nodes
          (lambda (clause-expression) 
            (private:unwrap-syntax (car (reverse clause-expression))))))]
    [else #f]))
) ; end library
