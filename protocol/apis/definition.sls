(library (scheme-langserver protocol apis definition)
  (export definition)
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
    (scheme-langserver virtual-file-system file-node))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
(define (definition workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [uri (text-document-uri text-document)]
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [index-node-list (document-index-node-list document)]
      [target-index-node (pick-index-node-from index-node-list (text+position->int (document-text document) position))]
      [prefix (if (null? (index-node-children target-index-node)) (annotation-expression (index-node-datum/annotations target-index-node)) )]
      [available-reference (find-available-references-for document target-index-node prefix)])
    (list->vector 
      (map identifier-reference->location->alist available-reference))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
(define (identifier-reference->location->alist reference)
  (location->alist
    (make-location 
      (document-uri (identifier-reference-document reference)) 
      (make-range 
        (index-node-start (identifier-reference-index-node reference))
        (index-node-end (identifier-reference-index-node reference))))))
)