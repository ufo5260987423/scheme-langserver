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
    (scheme-langserver util try) 
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
      [text (text-document-text text-document)])
    (try
      (refresh-workspace-for workspace file-node text 'previous+single)
      (except e [else '()]))))

(define (did-close workspace params)
  (let* ([versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier)))]
      [text (document-text (file-node-document file-node))])
    (try
      (refresh-workspace-for workspace file-node text 'single+tail)
      (except e [else '()]))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange
(define (did-change workspace params)
  (let* ([versioned-text-document-identifier (alist->versioned-text-document-identifier (assq-ref params 'textDocument))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (versioned-text-document-identifier-uri versioned-text-document-identifier)))]
      [body (lambda() 
          (let loop ([content-changes (map alist->text-edit (vector->list (assq-ref params 'contentChanges)))]
              [text (document-text (file-node-document file-node))]
              [merged-range-list '()])
            (if (null? content-changes)
              (try
                (refresh-workspace-for workspace file-node text merged-range-list 'single+tail)
                (except e [else '()]))
              (let* ([target (car content-changes)]
                  [range (text-edit-range target)]
                  [temp-text (text-edit-text target)]
                  [start (text+position->int text (range-start range))]
                  [end (text+position->int text (range-end range))])
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
                    (string-replace text temp-text start end)
                    temp-text)
                  (private-merge merged-range-list start end (length temp-text)))))))])
    (try
      (body)
      (except e [else '()]))))

(define (private-merge merged-range-list old-start old-end change-length)
  (let* ([f (lambda (item)
        (let* ([item-old-start (car item)]
            [item-old-end (cadr item)]
            [item-new-end (caddr item)]
            [item-change-length (- item-new-end item-old-start)]
            [maybe-new-end (+ old-start change-length)])
          (or
            (and 
              (<= item-old-start old-start)
              (> item-new-end old-start))
            (and 
              (< item-old-start old-end)
              (>= item-new-end old-end))
            (and 
              (<= old-start item-old-start)
              (> old-end item-old-start))
            (and 
              (< old-start item-new-end)
              (>= old-end item-new-end)))))]
      [nf (lambda (item) (not (f item)))]
      [positive-list (filter f merged-range-list)]
      [negative-list (filter nf merged-range-list)]

      [positive-old-start (fold-left min old-start (map car positive-list))]
      ;apparently, all merged-change-ranges are distinct: 
      ;their old-ranges and new-ranges won't interleave with each others
      ;so actural-* must be in one item
      [actural-max-old-end (fold-left max 0 (map cadr positive-list))]
      [actural-max-new-end (fold-left max 0 (map caddr positive-list))]

      [positive-old-end (if (< actural-max-new-end old-end) (+ actural-max-old-end (- old-end actural-max-new-end)) actural-max-new-end)]
      [positive-new-end (+ positive-old-start change-length)])
    `(,@negative-list ,(list positive-old-start positive-old-end positive-new-end))))

)