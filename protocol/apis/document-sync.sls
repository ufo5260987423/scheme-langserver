(library (scheme-langserver protocol apis document-sync)
  (export 
    did-open
    did-close
    did-change)
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

    (only (srfi :13 strings) string-prefix? string-index))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization
(define (did-open workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [text (text-document-text text-document)]
      [mutex (workspace-mutex workspace)])
      (if (not (equal? text (document-text document)))
        (if mutex
          (with-mutex mutex 
            (refresh-workspace-for workspace-instance file-node text))
          (refresh-workspace-for workspace-instance file-node text)))))

(define (did-close workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [text (text-document-text text-document)])
      (refresh-workspace-for workspace-instance file-node text)))

(define (did-change workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [text (text-document-text text-document)])
      (refresh-workspace-for workspace-instance file-node text)))
)