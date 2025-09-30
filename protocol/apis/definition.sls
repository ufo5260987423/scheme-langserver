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
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      ;why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (string-length (text-document-uri text-document)))) pre-file-node)]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [fuzzy (refresh-workspace-for workspace file-node)]
      [index-node-list (document-index-node-list document)]
      [target-index-node (pick-index-node-from index-node-list (document+position->bias document line character))]
      [import-file-node (if (null? target-index-node) '() (private:index-node-import-file-nodes target-index-node))]
      [prefix 
        (if (null? target-index-node)
          '()
          (if (null? (index-node-children target-index-node)) 
            (annotation-stripped (index-node-datum/annotations target-index-node)) 
            '()))])
    (list->vector 
      (cond 
        [(not (null? import-file-node))
          (map 
            (lambda (f)
              (location->alist
                (make-location
                  (document-uri (file-node-document f))
                  (make-range 
                    (apply make-position (document+bias->position-list (file-node-document f) 0))
                    (apply make-position (document+bias->position-list (file-node-document f) (- (string-length (document-text (file-node-document f))) 1)))))))
            import-file-node)]
        [(symbol? prefix) 
          (map identifier-reference->location->alist
            (filter 
              (lambda (r) (not (null? (identifier-reference-document r))))
              (apply append (map root-ancestor (find-available-references-for document target-index-node prefix)))))]
        [else '()]))))

(define (private:index-node-import-file-nodes index-node)
  (if (null? (index-node-import-file-nodes index-node))
    (if (null? (index-node-parent index-node))
      '()
      (private:index-node-import-file-nodes (index-node-parent index-node)))
    (index-node-import-file-nodes index-node)))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
(define (identifier-reference->location->alist reference)
  (location->alist
    (make-location 
      (document-uri (identifier-reference-document reference))
      (make-range 
        (apply make-position (document+bias->position-list (identifier-reference-document reference) (index-node-start (identifier-reference-index-node reference))))
        (apply make-position (document+bias->position-list (identifier-reference-document reference) (index-node-end (identifier-reference-index-node reference))))))))
)