(library (scheme-langserver analysis identifier rules r7rs define)
  (export define-r7rs-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter
(define (define-r7rs-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and signature-node
              (? index-node-proper-list?)
              ((? index-node-symbol? name-node) . param-nodes))
        . body)
      (let ([reference (make-identifier-reference 
              (index-node-expression name-node)
              document 
              name-node
              index-node
              '()
              'procedure
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
        (for-each
          (lambda (param-node)
            (define-r7rs-parameter-process index-node signature-node param-node document))
          param-nodes))]
    [(:_ (and signature-node
              (? (lambda (n) (pair? (annotation-stripped (index-node-datum/annotations n)))))
              ((? index-node-symbol? name-node) . param-nodes))
        . body)
      (let ([reference (make-identifier-reference 
              (index-node-expression name-node)
              document 
              name-node
              index-node
              '()
              'procedure
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference))
        (let loop ([rest param-nodes])
          (if (not (null? rest))
            (begin
              (define-r7rs-parameter-process index-node signature-node (car rest) document)
              (loop (cdr rest))))))]
    [else '()]))

(define (define-r7rs-parameter-process define-node signature-node param-node document)
  (let ([param-expression (annotation-stripped (index-node-datum/annotations param-node))])
    (if (symbol? param-expression)
      (let ([reference (make-identifier-reference
                param-expression
                document
                param-node
                define-node
                '()
                'parameter
                '()
                '())])
        (index-node-references-export-to-other-node-set!
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (index-node-references-import-in-this-node-set!
          define-node
          (sort-identifier-references
            (append 
              (index-node-references-import-in-this-node define-node)
              `(,reference))))
        (index-node-excluded-references-set! 
          signature-node
          (append 
            (index-node-excluded-references signature-node)
            `(,reference))))
      '())))
)
