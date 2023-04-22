(library (scheme-langserver protocol apis document-diagnostic)
  (export 
    diagnostic)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util natural-order-compare)
    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util try) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_pullDiagnostics
(define (diagnostic workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [path (uri->path (text-document-uri text-document))]
      [maybe (walk-file (workspace-file-node workspace) path)]
      [file-node 
        (if (null? maybe)
          (begin 
            (refresh-workspace workspace)
            (walk-file (workspace-file-node workspace) path))
          maybe)]
      [text (text-document-text text-document)])
    (try
      ;I'd only check leaf index-node
      (refresh-workspace-for workspace file-node text 'previous+single)
      (except e [else '()]))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(define (private-make-diagnostic range-start range-end severity message)
  (make-alist 'range (range->alist (make-range range-start range-end)) 'severity severity 'message message))
)