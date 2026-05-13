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
  ; Snapshot and clear immediately so that even if the traversal
  ; raises an exception the paths are not re-processed on the next
  ; timer tick (Bug 3).
  (let ([paths (workspace-undiagnosed-paths workspace)])
    (workspace-undiagnosed-paths-set! workspace '())
    (fold-right
      (lambda (s acc)
        (let ([file-node (walk-file (workspace-file-node workspace) s)])
          (if (null? file-node)
            acc
            (let ([document (file-node-document file-node)])
              (cons
                (make-alist
                  'uri (document-uri document)
                  'diagnostics (private:document->diagnostic-vec document))
                acc)))))
      '()
      paths)))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_pullDiagnostics
(define (diagnostic workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [file-node (resolve-uri->file-node (workspace-file-node workspace) (text-document-uri text-document))])
    (if (null? file-node)
      '()
      (let* ([document (file-node-document file-node)]
        [diagnoses (document-diagnoses document)])
        (refresh-workspace-for workspace file-node)
        (private:document->diagnostic-vec document)))))

(define (private:document->diagnostic-vec document)
  (vector-map 
    (lambda (diagnose)
      (private:make-diagnostic document diagnose))
    (list->vector (document-diagnoses document))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
(define (private:make-diagnostic document diagnose)
  (let* ([s (car diagnose)]
      [e (cadr diagnose)]
      [severity (caddr diagnose)]
      [message (cadddr diagnose)]
      [source (if (>= (length diagnose) 5) (list-ref diagnose 4) "scheme-langserver")]
      [code (if (>= (length diagnose) 6) (list-ref diagnose 5) #f)])
    (if code
      (make-alist 
        'range 
        (range->alist 
          (make-range 
            (apply make-position (document+bias->position-list document s))
            (apply make-position (document+bias->position-list document e)))) 
        'severity severity 
        'message message
        'source source
        'code code)
      (make-alist 
        'range 
        (range->alist 
          (make-range 
            (apply make-position (document+bias->position-list document s))
            (apply make-position (document+bias->position-list document e)))) 
        'severity severity 
        'message message
        'source source))))
)