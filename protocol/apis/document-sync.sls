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
      [document-text (document-text (file-node-document file-node))]
      [body (lambda() 
          (let loop ([content-changes (map alist->text-edit (vector->list (assq-ref params 'contentChanges)))]
              [text document-text]
              [align-list (list (private-generate-vector (string-length document-text)))])
            (if (null? content-changes)
              (try
                (refresh-workspace-for workspace file-node text (private-shrink-list->vector align-list) 'single+tail)
                (except e [else '()]))
              (let* ([target (car content-changes)]
                  [range (text-edit-range target)]
                  [temp-text (text-edit-text target)]
                  [start (text+position->int text (range-start range))]
                  [end (text+position->int text (range-end range))])
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
                  (string-replace text temp-text start end)
                  (private-align 
                    start 
                    end 
                    (+ start (string-length temp-text)) 
                    (append 
                      align-list 
                      (list (private-generate-vector (+ start (string-length temp-text) (- (string-length text) end)))))))))))])
    (try
      (body)
      (except e [else '()]))))

(define (private-shrink-list->vector origin-align-list)
  (if (null? origin-align-list)
    '#()
    (let ([tail-result (private-shrink-list->vector (cdr origin-align-list))]
        [head (car origin-align-list)])
      (if (null? (vector->list tail-result))
        head
        (vector-map
          (lambda (item) 
            (if (= -1 item)
              -1
              (vector-ref tail-result item)))
          head)))))

(define (private-align start origin-end target-end origin-align-list)
  (let ([origin-align-vector (cadr (reverse origin-align-list))])
    (let loop ([i start])
      (cond
        [(< i origin-end) 
          (vector-set! origin-align-vector i -1)
          (loop (+ 1 i))]
        [(< i (vector-length origin-align-vector))
          (vector-set! origin-align-vector i (+ target-end (- i origin-end)))
          (loop (+ 1 i))]
        [else origin-align-list]))))

(define (private-generate-vector length)
  (let ([result (make-vector length)])
    (let loop ([i 0])
      (if (< i length)
        (begin
          (vector-set! result i i)
          (loop (+ i 1)))
        result))))
)