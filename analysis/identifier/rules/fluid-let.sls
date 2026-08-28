(library (scheme-langserver analysis identifier rules fluid-let)
  (export 
    fluid-let-process
    fluid-let-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; variable 
(define (fluid-let-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (((? index-node-symbol? variable) init) **1) . body)
      (fold-left 
        (lambda (exclude-list variable-index-node)
          (let ([binding-list-node (index-node-parent (index-node-parent variable-index-node))]
                [extended-exclude-list 
                  (append exclude-list (fluid-let-parameter-process index-node variable-index-node index-node exclude-list document 'variable))])
            (index-node-excluded-references-set! binding-list-node extended-exclude-list)
            extended-exclude-list))
        '()
        variable)]
    [else '()]))

(define (fluid-let-parameter-process initialization-index-node index-node let-node exclude document type)
  (let* ([expression (index-node-expression index-node)]
      [upper (find-available-references-for document index-node expression)]
      [reference 
        (make-identifier-reference
          expression
          document
          index-node
          initialization-index-node
          '()
          type
          upper
          '())])

    (if (not (null? upper))
      (begin
        (index-node-references-export-to-other-node-set! 
          index-node
          (append 
            (index-node-references-export-to-other-node index-node)
              `(,reference)))

        (append-references-into-ordered-references-for document let-node `(,reference))

        `(,reference))
      '())))
)
