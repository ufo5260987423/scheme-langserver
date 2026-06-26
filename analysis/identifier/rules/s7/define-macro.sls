(library (scheme-langserver analysis identifier rules s7 define-macro)
  (export define-macro-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure parameter
;https://www.zenlife.tk/scheme-hygiene-macro.md
(define (define-macro-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [(_ ((? symbol? identifier) . dummy0) dummy1 ... )
        (let* ([omg-index-node (cadr (index-node-children index-node))]
            [reference (make-identifier-reference 
                identifier 
                document 
                omg-index-node
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
          (let loop ([rest dummy0])
            (cond 
              [(pair? rest) 
                (let ([reference (make-identifier-reference 
                    (car rest)
                    document 
                    omg-index-node
                    index-node
                    '()
                    'parameter
                    '()
                    '())])
                  (index-node-references-export-to-other-node-set! 
                    (identifier-reference-index-node reference)
                    (append 
                      (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                      `(,reference)))
                  (append-references-into-ordered-references-for document index-node `(,reference)))
                (loop (cdr rest))]
              [(not (null? rest)) 
                (let ([reference (make-identifier-reference 
                    rest
                    document 
                    omg-index-node
                    index-node
                    '()
                    'parameter
                    '()
                    '())])
                  (index-node-references-export-to-other-node-set! 
                    (identifier-reference-index-node reference)
                    (append 
                      (index-node-references-export-to-other-node (identifier-reference-index-node reference))
                      `(,reference)))
                  (append-references-into-ordered-references-for document index-node `(,reference)))]
              [else '()])))]
      [else '()])))
)
