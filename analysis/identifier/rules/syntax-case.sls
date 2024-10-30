(library (scheme-langserver analysis identifier rules syntax-case)
  (export 
    syntax-case-process
    clause-process
    get-all-symbols)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (syntax-case-process root-file-node root-librar-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ to-match (literals ...) (a b ...) **1) 
          (let ([rest (cdddr (index-node-children index-node))])
            (map (lambda (clause-index-node)
              (clause-process index-node document clause-index-node (car (index-node-children clause-index-node)) literals))
              rest))]
        [else '()])
      (except c
        [else '()]))))

(define (clause-process initialization-index-node document index-node template-index-node literals)
  (let* ([ann (index-node-datum/annotations template-index-node)]
      [expression (annotation-stripped ann)]
      [symbols 
        (filter 
          (lambda (symbol)
            (not (contain? literals symbol)))
          (get-all-symbols expression))])
    (map 
      (lambda (symbol)
        (let ([reference 
              (make-identifier-reference
                symbol
                document
                template-index-node
                initialization-index-node
                '()
                'syntax-parameter
                '()
                '())])
          (index-node-references-export-to-other-node-set! 
            template-index-node
            (append 
              (index-node-references-export-to-other-node template-index-node)
              `(,reference)))

          (append-references-into-ordered-references-for document index-node `(,reference))
          reference))
      symbols)))

(define (get-all-symbols s-expression)
  (if (symbol? s-expression)
    `(,s-expression)
    (cond
      [(list? s-expression) (apply append (map get-all-symbols s-expression))]
      [(pair? s-expression) (get-all-symbols `(,(car s-expression) ,(cdr s-expression)))]
      [(vector? s-expression) (apply append (vector->list (vector-map get-all-symbols s-expression)))]
      [else '()])))
)
