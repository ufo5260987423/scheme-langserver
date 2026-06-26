(library (scheme-langserver protocol apis document-diagnostic)
  (export 
    diagnostic
    unpublish-diagnostics->list)
  (import 
    (chezscheme) 

    (scheme-langserver analysis workspace)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

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
;
; NOTE: `document-diagnoses` stores diagnoses in the format
;   (start-bias end-bias severity message source code)
; where `start-bias` and `end-bias` are character offsets into the document.
; `document+bias->position-list` converts these offsets to (line character).
; Callers such as `append-new-diagnoses` must supply bias values, not raw
; line/character pairs.  For point diagnostics (e.g. syntax errors at an
; unknown exact position) both biases may be 0, which maps to line 0 char 0.
(define (private:make-diagnostic document diagnose)
  (let* ([s (car diagnose)]
      [e (cadr diagnose)]
      [severity (caddr diagnose)]
      [message (cadddr diagnose)]
      [source (if (>= (length diagnose) 5) (list-ref diagnose 4) "scheme-langserver")]
      [code (if (>= (length diagnose) 6) (list-ref diagnose 5) #f)]
      [tags (if (and code (or (string=? code "unused-import") (string=? code "unused-local-variable"))) (vector 1) #f)])
    ; LSP DiagnosticTag: 1 = Unnecessary, 2 = Deprecated.
    ; Unused imports and unused local variables are tagged as Unnecessary.
    (append 
      (make-alist 
        'range 
        (range->alist 
          (make-range 
            (apply make-position (document+bias->position-list document s))
            (apply make-position (document+bias->position-list document e)))) 
        'severity severity 
        'message message
        'source source)
      (if code (make-alist 'code code) '())
      (if tags (make-alist 'tags tags) '()))))
)