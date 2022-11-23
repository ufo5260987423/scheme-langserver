(library (scheme-langserver protocol apis document-symbol)
  (export 
    document-symbol)
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

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentSymbol
(define (document-symbol workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [path (uri->path (text-document-uri text-document))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [index-node-list (document-index-node-list document)]
      [identifiers
        (filter 
          (lambda (identifier-reference)
            (equal? document (identifier-reference-document identifier-reference)))
          (apply append 
            (map 
              index-node-references-import-in-this-node
              index-node-list)))]
      [result-vector (list->vector (map identifier->document-symbol identifiers))])
    result-vector))

(define (identifier->document-symbol identifier)
  (let* ([document (identifier-reference-document identifier)]
      [text (document-text document)]
      [index-node (identifier-reference-index-node identifier)]
      [name (symbol->strng (identifier-reference-identifier identifier))]
      [start-position (text+position->int text (index-node-start index-node))]
      [end-position (text+position->int text (index-node-end index-node))]
      [range (make-range start-postion end-postion)]
      )
    (make-document-symbol 
      name
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind
; todo: type inference
      13
      range
      range)))
)
