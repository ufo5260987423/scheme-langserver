(library (scheme-langserver protocol apis completion)
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

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
(define (completion workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [bias (text+position->int text position)]
      [target-index-node (pick-index-node-from index-node-list bias)]
      [prefix 
        (if target-index-node 
          (if (null? (index-node-children target-index-node)) 
            (symbol->string (annotation-stripped (index-node-datum/annotations target-index-node))) 
            ""))])
    ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    (list->vector (map 
      identifier-reference->completion-item-alist 
      (sort
        (lambda (a b) (natural-order-compare (symbol->string (identifier-reference-identifier a)) (symbol->string (identifier-reference-identifier b))))
        (filter 
          (lambda (candidate-reference) (string-prefix? prefix (symbol->string (identifier-reference-identifier candidate-reference)))) 
          (find-available-references-for document target-index-node)))))))

(define (identifier-reference->completion-item-alist reference)
  (make-alist 'label (symbol->string (identifier-reference-identifier reference))))
)
