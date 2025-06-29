(library (scheme-langserver analysis syntax rules define-syntax)
  (export 
    define-syntax-process)
  (import 
    (chezscheme)
    (ufo-match)
    (ufo-try)

    (scheme-langserver analysis syntax util)
    (scheme-langserver analysis syntax rules syntax-rules)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis tokenizer)


    (scheme-langserver util contain)
    (scheme-langserver util path)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (define-syntax-process document index-node syntax-stepper)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(d (? symbol? literals) (maybe-syntax-rules fuzzy ...)) 
          (guard-for document index-node d '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let* ([children (index-node-children index-node)]
              [target-index-node (cadr children)]
              [target-identifier-list (index-node-references-export-to-other-node target-index-node)]
              [rule-index-node (caddr children)])
            (map 
              (lambda (identifier)
                (identifier-reference-syntax-callee-transformers-set! 
                  identifier
                  (syntax-rules-process document rule-index-node)))
              (filter
                (lambda (identifier)
                  (equal? (identifier-reference-type identifier 'syntax-variable)))
                target-identifier-list)))]
        [else '()])
      (except c
        [else '()]))))
)