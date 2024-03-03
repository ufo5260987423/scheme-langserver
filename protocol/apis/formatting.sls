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
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [target
        (call-with-string-output-port
          (lambda (output-port)
            (pretty-print (read (open-string-input-port text)) output-port)))])
    (vector target)))
)
