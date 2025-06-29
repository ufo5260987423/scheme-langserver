(library (scheme-langserver analysis syntax rules let-syntax)
  (export 
    let-syntax-process)
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

(define (let-syntax-process document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(l (((? symbol? identifier) no-use ... ) **1 ) fuzzy ... ) 
          (guard-for docuemnt index-node l '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (map
            (lambda (identifier-parent-index-node)
              (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                  [identifier-references (index-node-references-export-to-other-node identifier-index-node)]
                  [syntax-rule-index-node (car (reverse (index-node-children identifier-parent-index-node)))]
                  [r (syntax-rules-process document index-node)])
                (map 
                  (lambda (id)
                    (identifier-reference-syntax-callee-transformers-set! id r))
                  identifier-references)))
            (index-node-children (cadr (index-node-children index-node))))]
        [else '()])
      (except c
        [else '()]))))
)