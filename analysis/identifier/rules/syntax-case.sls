(library (scheme-langserver analysis identifier rules syntax-case)
  (export 
    syntax-case-process
    clause-process
    get-all-symbols)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (syntax-case-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ to-match ((? index-node-symbol? literal) ...) . clauses)
      (let ([literals (map index-node-expression literal)])
        (map (lambda (clause-index-node)
            (clause-process index-node document clause-index-node (car (index-node-children clause-index-node)) literals))
          clauses))]
    [else '()]))

(define (clause-process initialization-index-node document index-node template-index-node literals)
  (let* ([expression (index-node-expression template-index-node)]
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
  (private:get-all-symbols s-expression (make-eq-hashtable)))

(define (private:get-all-symbols s-expression visited)
  (cond
    [(symbol? s-expression) `(,s-expression)]
    [(and (or (pair? s-expression) (vector? s-expression))
          (hashtable-ref visited s-expression #f))
     '()]
    [(list? s-expression)
     (begin
       (hashtable-set! visited s-expression #t)
       (apply append (map (lambda (e) (private:get-all-symbols e visited)) s-expression)))]
    [(pair? s-expression)
     (begin
       (hashtable-set! visited s-expression #t)
       (private:get-all-symbols `(,(car s-expression) ,(cdr s-expression)) visited))]
    [(vector? s-expression)
     (begin
       (hashtable-set! visited s-expression #t)
       (apply append (vector->list (vector-map (lambda (e) (private:get-all-symbols e visited)) s-expression))))]
    [else '()]))
)
