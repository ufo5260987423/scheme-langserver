(library (scheme-langserver protocol apis definition)
  (export definition)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition
(define (definition workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [uri (text-document-uri text-document)]
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [fuzzy (refresh-workspace-for workspace file-node)]
      [index-node-list (document-index-node-list document)]
      [target-index-node (pick-index-node-from index-node-list (document+position->bias document line character))]
      [prefix 
        (if (null? target-index-node)
          '()
          (if (null? (index-node-children target-index-node)) 
            (annotation-stripped (index-node-datum/annotations target-index-node)) 
            '()))])
    (list->vector 
      (map identifier-reference->location->alist 
        (filter 
          (lambda (r)
            (not (null? (identifier-reference-document r))))
          (apply append 
            (map 
              root-ancestor
              (cond 
                [(null? target-index-node) '()]
                [(symbol? prefix) (find-available-references-for document target-index-node prefix)]
                [else (find-available-references-for document target-index-node)]))))))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
(define (identifier-reference->location->alist reference)
  (location->alist
    (make-location 
      (document-uri (identifier-reference-document reference))
      (make-range 
        (apply make-position (document+bias->position-list (identifier-reference-document reference) (index-node-start (identifier-reference-index-node reference))))
        (apply make-position (document+bias->position-list (identifier-reference-document reference) (index-node-end (identifier-reference-index-node reference))))))))
)