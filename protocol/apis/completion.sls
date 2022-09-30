(library (scheme-langserver protocol method)
  (export completion)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util natural-order-compare)
    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util io)

    (only (srfi :13 strings) string-prefix?))

;; Completion Request
(define (completion workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'text-document))]
      [position (alist->postion (assq-ref params 'position))]
      [file-node (walk-file (workspace-file-node workspace) (text-document-uri text-document))]
      [index-node (document-index-node (file-node-document file-node))]
      [target-index-node (pick-index-node-by index-node position)]
      [text (annotation-expression (index-node-datum/annotations target-index-node))]
      [candidate-references 
        (sort
          (lambda (a b) (natrual-order-compare (symbo->string (reference-identifier a)) (symbo->string (reference-identifier b))))
          (filter 
            (lambda (candidate-reference) (string-prefix? prefix (symbol->string (reference-identifier candidate-reference)))) 
            (find-available-references-for target-index-node)))])
    ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    (map reference->completion-item-alist candidate-references)))

(define (reference->completion-item-alist reference)
  (make-alist 'label (symbol->string (reference-identifier reference))))
)