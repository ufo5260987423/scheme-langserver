(library (scheme-langserver analysis identifier rules define)
  (export 
    define-process
    index-node:regist-as-identifier-reference)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter variable 
(define (define-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ ((? index-node-symbol? name-node) (? index-node-symbol? params) ...) . body)
      (index-node:regist-as-identifier-reference name-node index-node name-node #f (index-node-parent index-node) document 'procedure)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) params))
      (map 
        (lambda (p)
          (index-node:regist-as-identifier-reference p index-node p (index-node-parent p) index-node document 'parameter))
        params)]
    [(:_ (? index-node-symbol? name-node) . body)
      (index-node:regist-as-identifier-reference name-node index-node name-node #f (index-node-parent index-node) document 'variable)]
    [else '()]))

(define (index-node:regist-as-identifier-reference target-index-node initilization-index-node export-index-node exclude-index-node import-index-node document type)
  (let ([reference (make-identifier-reference 
          (index-node-expression target-index-node)
          document 
          target-index-node
          initilization-index-node
          '()
          type
          '()
          '())])
    (index-node-references-export-to-other-node-set! 
      export-index-node
      (append 
        (index-node-references-export-to-other-node export-index-node)
        `(,reference)))
    (if exclude-index-node 
      (index-node-excluded-references-set!
        exclude-index-node
        (append 
          (index-node-excluded-references exclude-index-node)
          `(,reference))))
    (append-references-into-ordered-references-for document import-index-node `(,reference))
  reference))
)
