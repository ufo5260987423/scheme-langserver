(library (scheme-langserver analysis identifier rules let-syntax)
  (export 
    let-syntax-process
    let-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules let)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; syntax-variable 
(define (let-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (fuzzy0 **1) fuzzy1 ... ) 
        (fold-left 
          (lambda (exclude-list identifier-parent-index-node)
            (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                [target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'syntax-variable)]
                [extended-exclude-list (append exclude-list target-identifier-reference)])
              (if (not (null? target-identifier-reference)) (index-node-excluded-references-set! (index-node-parent identifier-parent-index-node) extended-exclude-list))
              extended-exclude-list))
          '()
          (filter 
            (lambda (i) (not (null? (index-node-children i)))) 
            (index-node-children (cadr (index-node-children index-node)))))]
      [else '()])))

; Mirror of define-syntax:attach-generator for let-syntax bindings.
; Each binding has the form (name syntax-rules-form).
; The expansion generator created by syntax-rules->generator:map+expansion
; is attached to the syntax-rules index-node; we copy it to the
; identifier-references created by let-parameter-process.
(define (let-syntax:attach-generator root-file-node root-library-node document index-node)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [children (index-node-children index-node)])
    (match expression
      [(_ (bindings ...) body ...) 
        (for-each
          (lambda (binding-parent-index-node)
            (let ([binding-children (index-node-children binding-parent-index-node)])
              (if (>= (length binding-children) 2)
                (let* ([name-index-node (car binding-children)]
                    [syntax-rules-index-node (cadr binding-children)]
                    [generator (index-node-expansion-generator syntax-rules-index-node)])
                  (if (procedure? generator)
                    (for-each
                      (lambda (ref)
                        (identifier-reference-syntax-expander-set! ref
                          (lambda x (apply generator x))))
                      (index-node-references-export-to-other-node name-index-node)))))))
          (filter 
            (lambda (i) (not (null? (index-node-children i)))) 
            (index-node-children (cadr children))))]
      [else '()])))
) ; end library
