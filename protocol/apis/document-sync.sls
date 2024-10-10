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

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util try) 
    (scheme-langserver util text) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization
(define (did-open workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [uri (text-document-uri text-document)]
      [path (uri->path uri)])
    (if 
      (and 
        (null? (walk-file (workspace-file-node workspace) path))
        ;for many LSP clients, they wrongly produce uri without processing escape character
        (null? (walk-file (workspace-file-node workspace) (substring uri 7 (string-length uri)))))
      ;TODO:well, can be optimized
      (refresh-workspace workspace))))

(define (did-close workspace params)
  '())

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
(define (did-change workspace params)
  (let* ([versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier)))]
      [document-text (document-text (file-node-document file-node))]
      [body (lambda() 
          (let loop ([content-changes (map alist->text-edit (vector->list (assq-ref params 'contentChanges)))]
              [text document-text])
            (if (null? content-changes)
              (update-file-node-with-tail workspace file-node text)
              (let* ([target (car content-changes)]
                  [range (text-edit-range target)]
                  [temp-text (text-edit-text target)]
                  [start (text+position->int text (position-line (range-start range)) (position-character (range-start range)))]
                  [end (text+position->int text (position-line (range-end range)) (position-character (range-end range)))])
                (loop 
                  (cdr content-changes) 
;;The actual content changes. The content changes describe single state
;;changes to the document. So if there are two content changes c1 (at
;;array index 0) and c2 (at array index 1) for a document in state S then
;;c1 moves the document from S to S' and c2 from S' to S''. So c1 is
;;computed on the state S and c2 is computed on the state S'.
;;To mirror the content of a document using change events use the following
;;approach:
;;- start with the same initial content
;;- apply the 'textDocument/didChange' notifications in the order you
;;  receive them.
;;- apply the `TextDocumentContentChangeEvent`s in a single notification
;;  in the order you receive them.
                  (string-replace text temp-text start end))))))])
    (try
      (body)
      (except e [else '()]))))
)