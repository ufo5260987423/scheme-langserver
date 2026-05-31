(library (scheme-langserver analysis identifier rules letrec-syntax)
  (export 
    letrec-syntax-process
    letrec-syntax:attach-generator)
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
(define (letrec-syntax-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ (fuzzy0 **1 ) fuzzy1 ... ) 
        (fold-left 
          (lambda (exclude-list identifier-parent-index-node)
            (let* ([identifier-index-node (car (index-node-children identifier-parent-index-node))]
                [target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'syntax-variable)]
                [extended-exclude-list (append exclude-list target-identifier-reference)])
              (index-node-excluded-references-set! (cadr (index-node-children index-node)) exclude-list)
              (append-references-into-ordered-references-for document identifier-index-node target-identifier-reference)
              extended-exclude-list))
          '()
          (filter 
            (lambda (i) (not (null? (index-node-children i)))) 
            (index-node-children (cadr (index-node-children index-node)))))]
      [else '()])))

; Same logic as let-syntax:attach-generator; the binding shape is identical.
(define (letrec-syntax:attach-generator root-file-node root-library-node document index-node)
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
