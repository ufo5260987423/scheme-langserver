(library (scheme-langserver protocol apis document-diagnostic)
  (export 
    diagnostic
    unpublish-diagnostics->list)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type substitutions util)
    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-replace))

(define (unpublish-diagnostics->list workspace)
  (let ([result 
        (map 
          (lambda (d)
            (make-alist
              'uri (document-uri d)
              'diagnostics (private:document->diagnostic-vec d)))
          (map file-node-document 
            (map 
              (lambda (s)
                (walk-file (workspace-file-node workspace) s))
              (workspace-undiagnosed-paths workspace))))])
    (if (null? (workspace-mutex workspace))
      (workspace-undiagnosed-paths-set! workspace '())
      (with-mutex (workspace-mutex workspace)
        (workspace-undiagnosed-paths-set! workspace '())))
    result))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_pullDiagnostics
(define (diagnostic workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      ; why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (string-length (text-document-uri text-document)))) pre-file-node)]
      [document (file-node-document file-node)]
      [diagnoses (document-diagnoses document)])
    (refresh-workspace-for workspace file-node)
    (private:document->diagnostic-vec document)))

(define (private:document->diagnostic-vec document)
  (vector-map 
    (lambda (diagnose)
      (let* ([s (car diagnose)]
          [e (cadr diagnose)]
          [severity (caddr diagnose)]
          [message (cadddr diagnose)])
      (private:make-diagnostic document s e severity message)))
    (list->vector (document-diagnoses document))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(define (private:make-diagnostic document range-start range-end severity message)
  (make-alist 
    'range 
    (range->alist 
      (make-range 
        (apply make-position (document+bias->position-list document range-start))
        (apply make-position (document+bias->position-list document range-end)))) 
    'severity severity 
    'message message))
)