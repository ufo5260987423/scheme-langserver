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
      [index-node (document-index-node document)]
      [target-index-node (pick-index-node-by index-node (text+position->int (document-text document) position))]
      [prefix (if (null? (index-node-children target-index-node)) (annotation-expression (index-node-datum/annotations target-index-node)) "")])
    (pretty-print prefix)
    (pretty-print "???")
    (list->vector (map 
      identifier-reference->location->alist 
      (filter 
        (lambda (candidate-reference) 
          (pretty-print (identifier-reference-identifier candidate-reference))
          (equal? prefix (identifier-reference-identifier candidate-reference))) 
        (find-available-references-for target-index-node))))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
(define (identifier-reference->location->alist reference)
  (pretty-print (identifier-reference-document reference))
  (location->alist
    (make-location 
      (document-uri (identifier-reference-document reference)) 
      (make-range 
        (source-object-bfp (annotation-source (identifier-reference-index-node reference))) 
        (source-object-efp (annotation-source (identifier-reference-index-node reference)))))))
)