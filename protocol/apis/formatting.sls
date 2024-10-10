(library (scheme-langserver protocol apis formatting)
  (export formatting)
  (import 
    (chezscheme) 

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util cartesian-product)
    (scheme-langserver util path) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentRangeFormattingParams
(define (formatting workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      ;why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (length (text-document-uri text-document)))) '())]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [range 
        (make-range (make-position 0 0) 
          (apply make-position (document+bias->position-list document (string-length text))))]
      [target
        (call-with-string-output-port
          (lambda (output-port)
            (pretty-print (read (open-string-input-port text)) output-port)))]
      [text-edit (make-text-edit range target)])
    (vector (text-edit->alist-with-newText text-edit))))
)
