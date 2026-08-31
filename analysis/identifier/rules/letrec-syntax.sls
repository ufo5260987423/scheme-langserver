(library (scheme-langserver analysis identifier rules letrec-syntax)
  (export 
    letrec-syntax-process
    letrec-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-variable 
(define (letrec-syntax-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and bindings-list (((? index-node-symbol? identifier) value) **1)) . body)
      (fold-left 
        (lambda (exclude-list identifier-index-node)
          (let* ([target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'syntax-variable)]
              [extended-exclude-list (append exclude-list target-identifier-reference)])
            (index-node-excluded-references-set! bindings-list exclude-list)
            (append-references-into-ordered-references-for document identifier-index-node target-identifier-reference)
            extended-exclude-list))
        '()
        identifier)]
    [else '()]))

; Same logic as let-syntax:attach-generator; the binding shape is identical.
(define (letrec-syntax:attach-generator root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ ((vars . val-groups) **1) . body)
      (fold-left
        (lambda (rest-val-groups var)
          (if (not (null? (car rest-val-groups)))
            (let ([generator (index-node-expansion-generator (caar rest-val-groups))])
              (if (procedure? generator)
                (for-each
                  (lambda (ref)
                    (identifier-reference-syntax-expander-set! ref
                      (lambda x (apply generator x))))
                  (index-node-references-export-to-other-node var)))))
          (cdr rest-val-groups))
        val-groups
        vars)]
    [else '()]))
) ; end library
