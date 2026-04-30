(library (scheme-langserver protocol apis references)
  (export find-references)
  (import 
    (chezscheme) 

    (ufo-match)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util dedupe) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
(define (find-references workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [file-node (resolve-uri->file-node (workspace-file-node workspace) (text-document-uri text-document))])
    (if (null? file-node)
      '()
      (let* ([document (file-node-document file-node)]
      [fuzzy (refresh-workspace-for workspace file-node)]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [bias (document+position->bias document (position-line position) (position-character position))]
      [pre-target-index-node (pick-index-node-from index-node-list bias)]
      ;; Be tolerant when the client reports a position right *after* an identifier.
      ;; In that case `pick-index-node-from` often returns the parent list node.
      [target-index-node
        (if (null? pre-target-index-node)
          pre-target-index-node
          (if (null? (index-node-children pre-target-index-node))
            pre-target-index-node
            (if (> bias 0)
              (pick-index-node-from index-node-list (- bias 1))
              pre-target-index-node)))]
      [prefix
        (if (and
              (index-node? target-index-node)
              (null? (index-node-children target-index-node)))
          (let ([expr (annotation-stripped (index-node-datum/annotations target-index-node))])
            (if (symbol? expr) expr #f))
          #f)])
    (if (symbol? prefix)
      (let* ([available-references 
            (if (null? (index-node-references-export-to-other-node target-index-node))
              (find-available-references-for document target-index-node prefix)
              (index-node-references-export-to-other-node target-index-node))]
          [origin-documents (dedupe (map identifier-reference-document available-references))]
          [origin-uris (map document-uri origin-documents)]
          [origin-paths (map uri->path origin-uris)]
          [path-to-list 
            (apply append
              origin-paths
              (map 
                (lambda (path) (file-linkage-to (workspace-file-linkage workspace) path))
                origin-paths))]
          [root-file-node (workspace-file-node workspace)]
          [maybe-target-documents (map (lambda (local-path) (walk-file root-file-node local-path)) path-to-list)]
          [target-documents (map file-node-document maybe-target-documents)]
          [predicate? 
            (lambda (maybe-symbol)
              (if (symbol? maybe-symbol)
                (find 
                  (lambda (identifier) (equal? maybe-symbol identifier))
                  (map identifier-reference-identifier available-references))
                #f))])
        (list->vector 
          (map location->alist 
            (apply append 
              (map 
                (lambda (document) 
                  (apply append 
                    (map 
                      (lambda (index-node) (find-references-in document index-node available-references predicate?))
                      (document-index-node-list document))))
                target-documents)))))
          '#())))))
)
