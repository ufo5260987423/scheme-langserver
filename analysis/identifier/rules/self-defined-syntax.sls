(library (scheme-langserver analysis identifier rules self-defined-syntax)
  (export 
    self-defined-syntax-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)
    (scheme-langserver util path)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis local-expand)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-parameter 
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (self-defined-syntax-process root-file-node root-library-node document index-node file-linkage stepper)
  (let* ([candidates (private-get-candidates index-node)]
      [origin-annotation-list (private-expand root-library-node document index-node file-linkage (annotation-stripped (index-node-datum/annotations index-node)) stepper)]
      [origin-import-in-this-node (map identifier-reference-identifier (index-node-references-import-in-this-node index-node))]
      )
    (index-node-references-import-in-this-node-set! index-node origin-import-in-this-node)))

(define (private-expand root-library-node document index-node to-eval file-linkage stepper)
  (try
    (let* ([tmp-result (local-expand to-eval document root-library-node file-linkage)]
        [annotation-list (map (lambda (e) (source-file->annotations e (uri->path (document-uri document)))) tmp-result)])
      (map (lambda (a) (index-node-parent-set! a index-node)) annotation-list)
      (map stepper annotation-list)
      annotation-list)
    (except c [else '()])))

(define (private-get-corresponding-identifiers document origin-index-node origin-identifiers current-index-node stepper)
  (let* ([origin-start (index-node-start origin-index-node)]
      [origin-end (index-node-start origin-index-node)]
      [text (document-text document)]
      [current-start (index-node-start current-index-node)]
      [current-end (index-node-end current-index-node)]
      [current-text (substring text current-start current-end)]
      [pre-text (substring text origin-start current-start)]
      [tail-text (substring text current-end origin-end)]
      [new-text (string-append pre-text "prefix-" current-text tail-text)]
      [port (open-string-input-port new-text)]
      [to-eval (read port)]
      [current-annotation-list (private-expand document origin-index-node to-eval stepper)])
    (let loop (
        [origin-identifiers (filter (lambda (i) (equal? (symbol->string (identifier-reference-identifier i)) current-text)) origin-identifiers)]
        [current-identifiers 
          (filter (lambda (i) (equal? (symbol->string (identifier-reference-identifier i)) new-text)) 
            (apply append (map private-get-all-identifiers current-annotation-list)))])
      (cond 
        [(null? origin-identifiers) '()]
        [(null? current-identifiers) origin-identifiers]
        [(equal? 
          (identifier-reference-identifier (car origin-identifiers)) 
          (identifier-reference-identifier (car current-identifiers)) ) 
          (loop (cdr origin-identifiers) (cdr current-identifiers))]
        [else `(,(car origin-identifiers) . ,(loop (cdr origin-identifiers) (cdr current-identifiers)))]))))

(define (private-get-candidates index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (cond 
      [(equal? 'quote expression) '()]
      [(equal? 'quasiquote expression) '()]
      [(equal? 'unquote expression) '()]
      [(equal? 'unquote-splicing expression) '()]
      [(equal? 'unsyntax-splicing expression) '()]
      [(equal? 'unsyntax expression) '()]
      [(equal? 'syntax expression) '()]
      [(equal? 'quasisyntax expression) '()]

      [(symbol? expression) `(,index-node)]
      [(not (null? (index-node-children index-node))) (apply append (map private-get-candidates (index-node-children index-node)))]
      [else '()])))

(define (private-get-all-identifiers index-node method)
  (apply append (method index-node) (map (lambda (i) (private-get-all-identifiers i method)) (index-node-children index-node))))

(define (pick-isomorphism-by-step index-node structure)
  (let ([tmp-result (pick-index-node-with index-node structure)])
    (cond 
      [(not (null? tmp-result))  `((,structure . ,tmp-result))]
      [(list? structure) (apply append (map (lambda (s) (pick-isomorphism-by-step index-node s)) structure))]
      [(vector? structure) (apply append (vector->list (vector-map (lambda (s) (pick-isomorphism-by-step index-node s)) structure)))]
      [else '()])))

(define (pick-index-node-with index-node structure)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (equal? structure expression)
      `(,index-node)
      (apply append (map (lambda (i) (pick-index-node-with i structure)) (index-node-children index-node))))))
)
