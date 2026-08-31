(library (scheme-langserver analysis identifier rules s7 lambda*)
  (export 
    lambda*-process
    parameter*-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; parameter 
(define (lambda*-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and params-node
              (? (lambda (n) (list? (annotation-stripped (index-node-datum/annotations n)))))
              (params **1))
        . body)
      (for-each
        (lambda (param)
          (parameter*-process index-node param index-node document))
        params)]
    [(:_ (? index-node-symbol? params-node) . body)
      (parameter*-process index-node params-node index-node document)]
    [(:_ (and params-node
              (? (lambda (n) (pair? (annotation-stripped (index-node-datum/annotations n)))))
              ((? index-node-symbol? first-param) . rest-params))
        . body)
      (parameter*-process index-node first-param index-node document)
      (let loop ([rest rest-params])
        (if (not (null? rest))
          (begin
            (parameter*-process index-node (car rest) index-node document)
            (loop (cdr rest)))))]
    [else '()]))

(define (parameter*-process initialization-index-node index-node lambda-node document)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [identifier (cond
                    [(symbol? expression) expression]
                    [(pair? expression) (car expression)]
                    [else #f])])
    (if identifier
      (let ([reference 
            (make-identifier-reference
              identifier
              document
              index-node
              initialization-index-node
              '()
              'parameter
              '()
              '())])
        (index-node-references-export-to-other-node-set! 
          index-node
          (append 
            (index-node-references-export-to-other-node index-node)
            `(,reference)))

        (index-node-references-import-in-this-node-set! 
          lambda-node
          (sort-identifier-references 
            (append 
              (index-node-references-import-in-this-node lambda-node)
              `(,reference))))

        (index-node-excluded-references-set! 
          (index-node-parent index-node)
          (append 
            (index-node-excluded-references index-node)
            `(,reference)))
        `(,reference))
      '())))
)
