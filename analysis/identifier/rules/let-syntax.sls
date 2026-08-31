(library (scheme-langserver analysis identifier rules let-syntax)
  (export 
    let-syntax-process
    let-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-variable 
(define (let-syntax-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and bindings-list (((? index-node-symbol? identifier) . values) **1)) . body)
      (fold-left 
        (lambda (exclude-list identifier-index-node)
          (let* ([target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'syntax-variable)]
              [extended-exclude-list (append exclude-list target-identifier-reference)])
            (if (not (null? target-identifier-reference))
              (index-node-excluded-references-set! bindings-list extended-exclude-list))
            extended-exclude-list))
        '()
        identifier)]
    [else '()]))

; Mirror of define-syntax:attach-generator for let-syntax bindings.
; Each binding has the form (name syntax-rules-form).
; The expansion generator created by syntax-rules->generator:map+expansion
; is attached to the syntax-rules index-node; we copy it to the
; identifier-references created by let-parameter-process.
(define (let-syntax:attach-generator root-file-node root-library-node document index-node)
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
