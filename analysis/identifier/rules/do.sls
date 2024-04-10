(library (scheme-langserver analysis identifier rules do)
  (export do-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util try)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; reference-identifier-type include 
; variable 
(define (do-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)])
    (try
      (match expression
        [(_ ((var init+update ... ) **1) fuzzy ... ) 
          (let* ([children (index-node-children index-node)]
            [var-index-node (cadr children)])
            (map (lambda (i) (private-process document i index-node var-index-node)) (index-node-children var-index-node)))]
        [else '()])
      (except c
        [else '()]))))

(define (private-process document target-index-node initialization-index-node exclude-index-node)
  (let* ([ann (index-node-datum/annotations target-index-node)]
      [expression (annotation-stripped ann)])
    (match expression
      [((? symbol? var) _ ...)
        (let ([reference 
            (make-identifier-reference
              var
              document
              target-index-node
              initialization-index-node
              '()
              'variable
              '()
              '())])
          (index-node-references-export-to-other-node-set! 
            target-index-node
            (append 
              (index-node-references-export-to-other-node target-index-node)
              `(,reference)))
          (index-node-references-import-in-this-node-set! 
            target-index-node
            (append 
              (index-node-references-import-in-this-node target-index-node)
              `(,reference)))
          (index-node-excluded-references-set! 
            exclude-index-node
            (append 
              (index-node-excluded-references exclude-index-node)
              `(,reference)))
          reference)]
      [else '()])))
)
