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

    (only (srfi :13 strings) string-replace))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization
(define (did-open workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [path (uri->path (text-document-uri text-document))]
      [maybe (walk-file (workspace-file-node workspace) path)]
      [file-node 
        (if (null? maybe)
          (begin 
            (refresh-workspace workspace)
            (walk-file (workspace-file-node workspace) path))
          maybe)]
      [text (text-document-text text-document)]
      [mutex (workspace-mutex workspace)])
    (if (null? mutex)
      (refresh-workspace-for workspace file-node text)
      (with-mutex mutex 
        (refresh-workspace-for workspace file-node text)))))

(define (did-close workspace params)
  (let* ([versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier)))]
      [text (document-text (file-node-document file-node))]
      [mutex (workspace-mutex workspace)])
    (if (null? mutex)
      (refresh-workspace-for workspace file-node text)
      (with-mutex mutex
        (refresh-workspace-for workspace file-node text)))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
(define (did-change workspace params)
  (let* ([versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier)))]
      [mutex (workspace-mutex workspace)]
      [body (lambda() 
          (let loop ([content-changes (map alist->text-edit (vector->list (assq-ref params 'contentChanges)))]
              [text (document-text (file-node-document file-node))])
            (if (null? content-changes)
              (refresh-workspace-for workspace file-node text)
              (let* ([target (car content-changes)]
                  [range (text-edit-range target)]
                  [temp-text (text-edit-text target)])
                (loop 
                  (cdr content-changes) 
                  (if range
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
                    (string-replace text temp-text
                      (text+position->int text (range-start range))
                      (text+position->int text (range-end range)))
                    temp-text))))))])
    (if (null? mutex)
      (body)
      (with-mutex mutex
        (body)))))

)