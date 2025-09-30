(library (scheme-langserver protocol apis document-diagnostic)
  (export 
    diagnostic)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (ufo-try) 
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_pullDiagnostics
(define (diagnostic workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      ; why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (string-length (text-document-uri text-document)))) pre-file-node)]
      [document (file-node-document file-node)]
      [diagnoses (document-diagnoses document)])
    ;I'd only check leaf index-node
    (vector-map 
      (lambda (diagnose)
        (let* ([index-node (car diagnose)]
            [s (index-node-start index-node)]
            [e (index-node-end index-node)]
            [severity (cadr diagnose)]
            [message (caddr diagnose)])
        (private-make-diagnostic document s e severity message)))
      (list->vector diagnoses))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(define (private-make-diagnostic document range-start range-end severity message)
  (make-alist 
    'range 
    (range->alist 
      (make-range 
        (apply make-position (document+bias->position-list document range-start))
        (apply make-position (document+bias->position-list document range-end)))) 
    'severity severity 
    'message message))
)