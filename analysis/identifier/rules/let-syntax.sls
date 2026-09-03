(library (scheme-langserver analysis identifier rules let-syntax)
  (export 
    let-syntax-process
    let-syntax:attach-generator)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules define-syntax)

    (scheme-langserver virtual-file-system index-node))

; reference-identifier-type include 
; syntax-variable 
(define (let-syntax-process root-file-node root-library-node document index-node)
  (match-index-node index-node
    [(:_ (and bindings-list (((? index-node-symbol? identifier) . values) **1)) . body)
      (fold-left 
        (lambda (exclude-list identifier-index-node)
          (let* ([target-identifier-reference (let-parameter-process index-node identifier-index-node index-node document 'syntax-variable)]
              [extended-exclude-list (append exclude-list target-identifier-reference)])
            (if (not (null? target-identifier-reference))
              (index-node-excluded-references-set! bindings-list extended-exclude-list))
            extended-exclude-list))
        '()
        identifier)]
    [else '()]))

(define let-syntax:attach-generator syntax-binding:attach-generator)
) ; end library
