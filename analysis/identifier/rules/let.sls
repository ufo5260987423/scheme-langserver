(library (scheme-langserver analysis identifier rules let)
  (export 
    let-process
    let-parameter-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier util)

    (scheme-langserver analysis identifier rules define)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; procedure variable 
(define (let-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (? index-node-symbol? loop-identifier) (((? index-node-symbol? vars) . vals) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) vars))
      (index-node:regist-as-identifier-reference loop-identifier index-node loop-identifier (caddr (index-node-children (index-node-parent loop-identifier))) index-node document 'procedure)
      (map 
        (lambda (var) 
          (index-node:regist-as-identifier-reference var index-node var (index-node-parent var) index-node document 'variable)) 
        vars)
      (let ([exclude-set (apply append (map index-node-references-export-to-other-node vars))])
        (map (lambda (var) (index-node-excluded-references-set! var exclude-set)) vars))]
    [(:_ (((? index-node-symbol? vars) . vals) **1) . body)
      (check-duplicate-identifiers document (map (lambda (p) (cons (index-node-expression p) p)) vars))
      (map 
        (lambda (var) 
          (index-node:regist-as-identifier-reference var index-node var (index-node-parent var) index-node document 'variable)) 
        vars)
      (let ([exclude-set (apply append (map index-node-references-export-to-other-node vars))])
        (map (lambda (var) (index-node-excluded-references-set! var exclude-set)) vars))]
    [(:_ (? index-node-symbol? loop-identifier) . body)
      (index-node:regist-as-identifier-reference loop-identifier index-node loop-identifier #f index-node document 'procedure)]
    [else '()]))

(define (let-parameter-process initialization-index-node index-node let-node document type)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (if (not (symbol? expression))
      '()
      (let (
        [reference 
          (make-identifier-reference
            expression
            document
            index-node
            initialization-index-node
            '()
            type
            '()
            '())])
      (index-node-references-export-to-other-node-set! 
        index-node
        (append 
          (index-node-references-export-to-other-node index-node)
            `(,reference)))

      (append-references-into-ordered-references-for document let-node `(,reference))

      `(,reference)))))
)
