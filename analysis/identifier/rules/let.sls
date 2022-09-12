(library (scheme-langserver analysis identifier rules let)
  (export library-define-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (let-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('let ((identifier no-use ... ) **1 ) _ ... ) 
        (let loop ([kv-nodes (index-node-children (car (index-node-children index-node)))]
                   [current-kv-node (car kv-nodes)]
                   [current-key-identifier (annotation-stripped (index-node-datum/annotations current-key-node)) ])
        )
        (map 
          (lambda (kv-node)
            (let* ([key-node (car (index-node-children kv-node))]
                  [key-identifier (annotation-stripped (index-node-datum/annotations key-node))])
              (index-node-references-export-to-other-node-set! 
                key-node 
                (append (index-node-references-export-to-other-node key-node) `(,key-identifier)))))
          (index-node-children (car (index-node-children index-node)))
          )

        (index-node-references-import-in-this-node-set! 
          index-node 
          (append (index-node-references-import-in-this-node index-node) `(,key-identifier)))))
          ]
      [else '()])
    (map 
      (lambda (child-node) (library-define-process root-file-node document child-node))
        (index-node-children index-node))
    index-node))

)