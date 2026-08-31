(library (scheme-langserver analysis identifier rules s7 define*)
  (export define*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter variable 
(define (define*-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and signature-node
              (? (lambda (n) (list? (annotation-stripped (index-node-datum/annotations n)))))
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
            (define*-parameter-process index-node signature-node param-node document))
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
              (define*-parameter-process index-node signature-node (car rest) document)
              (loop (cdr rest))))))]
    [(:_ (? index-node-symbol? signature-node) . body)
      (let ([reference (make-identifier-reference 
              (index-node-expression signature-node)
              document 
              signature-node
              index-node
              '()
              'variable
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          (identifier-reference-index-node reference)
          (append 
            (index-node-references-export-to-other-node (identifier-reference-index-node reference))
            `(,reference)))
        (append-references-into-ordered-references-for document (index-node-parent index-node)  `(,reference)))]
    [else '()]))

(define (define*-parameter-process define*-node signature-node param-node document)
  (let* ([param-expression (annotation-stripped (index-node-datum/annotations param-node))]
      [identifier (cond
                    [(symbol? param-expression) param-expression]
                    [(pair? param-expression) (car param-expression)]
                    [else #f])])
    (if identifier
      (let ([reference (make-identifier-reference
                identifier
                document
                param-node
                define*-node
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
          define*-node
          (sort-identifier-references
            (append 
              (index-node-references-import-in-this-node define*-node)
              `(,reference))))
        (index-node-excluded-references-set! 
          signature-node
          (append 
            (index-node-excluded-references signature-node)
            `(,reference))))
      '())))
)
