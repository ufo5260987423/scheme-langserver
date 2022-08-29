(library (scheme-langserver analysis identifier reference)
  (export 
    find-available-references-for

    make-identifier-reference
    identifier-reference-document
    identifier-reference-index-node)
  (import 
    (rnrs)
    (scheme-langserver virtual-file-system index-node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type identifier-reference
  (fields
    (immutable document)
    (immutable index-node)))

(define (find-available-references-for current-index-node)
  (if (not (null? (index-node-parent current-index-node)))
    (append 
      (index-node-references-import-in-this-node current-index-node) 
      (find-available-references-for (index-node-parent current-index-node)))))
)