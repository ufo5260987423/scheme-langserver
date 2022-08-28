(library (scheme-langserver analysis identifier reference)
  (export init-workspace)
  (import 
    (chezscheme) 
    (ufo-match)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable document)
    (immutable reference-index-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-references 
  (case-lambda
    [(index-file-node) 
      (init-references index-file-node 
        (document-index 
          (file-node-document index-file-node)))]
    [(index-file-node index-node) 
      (let* loop ([current-file-node index-file-node]
              [parent (file-node-parent current-file-node)])
        (if (null? parent)
          (init-references current-file-node index-file-node index)
          (loop parent (file-node-parent parent))))]
    [(file-system-node index-file-node index-node)
      (let loop ( [available-references-count (length (index-node-available-references index-node))])
        (index-node-available-references-extend index-node)
      )
    ]
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (index-available-references-extend index-node)
  (index-node-available-references-set! 
    index-node 
    (append '() (index-node-available-references index-node))))
)