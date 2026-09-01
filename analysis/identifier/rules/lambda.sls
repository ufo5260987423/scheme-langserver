(library (scheme-langserver analysis identifier rules lambda)
  (export 
    lambda-process
    parameter-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; parameter 
(define (lambda-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ ((? index-node-symbol? parameters) **1) . body)
      (let ([pairs (collect-parameter-pairs (cadr (index-node-children index-node)))])
        (check-duplicate-identifiers document pairs)
        (map 
          (lambda (parameter)
            (parameter-process index-node parameter (index-node-parent parameter) index-node document))
          parameters))]
    [(:_ (? index-node-symbol? parameter) . body)
      (parameter-process index-node parameter parameter index-node document)]
    [else '()]))

(define (parameter-process initialization-node export-node exclude-node import-node document)
  (let* ([expression (index-node-expression export-node)])
    (let ([reference 
          (make-identifier-reference
            expression
            document
            export-node
            initialization-node
            '()
            'parameter
            '()
            '())])
      (index-node-references-export-to-other-node-set! 
        export-node
        (append 
          (index-node-references-export-to-other-node export-node)
          `(,reference)))

      (index-node-excluded-references-set! 
        exclude-node
        (append 
          (index-node-excluded-references exclude-node)
          `(,reference)))
      (index-node-references-import-in-this-node-set! 
        import-node
        (sort-identifier-references 
          (append 
            (index-node-references-import-in-this-node import-node)
            `(,reference)))))))
)
