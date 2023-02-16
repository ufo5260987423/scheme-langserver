(library (scheme-langserver protocol apis document-format)
  (export 
    document-format
    code-format)
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

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting
(define (document-format workspace params)
    (pretty-print 'format0)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [path (uri->path (text-document-uri text-document))]
      [maybe (walk-file (workspace-file-node workspace) path)]
      [file-node 
        (if (null? maybe)
          (begin 
            (refresh-workspace workspace)
            (walk-file (workspace-file-node workspace) path))
          maybe)]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [result (code-format (document-index-node-list document) text "")])
    (pretty-print 'format1)
    (try
      (vector 
        (text-edit->alist-with-newText 
          (make-text-edit 
            (make-range 
              (int+text->position 0 text)
              (int+text->position (string-length text) text))
            result)))
      (except e 
        [else 
          (vector 
            (text-edit->alist-with-newText 
              (make-text-edit 
                (make-range 
                  (int+text->position 0 text)
                  (int+text->position (string-length text) text))
                text)))]))))


; ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rangeFormatting
; (define (document-range-format workspace params)
;   (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
;       [range (alist->range (assq-ref params 'range))]
;       [path (uri->path (text-document-uri text-document))]
;       [maybe (walk-file (workspace-file-node workspace) path)]
;       [file-node 
;         (if (null? maybe)
;           (begin 
;             (refresh-workspace workspace)
;             (walk-file (workspace-file-node workspace) path))
;           maybe)]
;       [document (file-node-document file-node)]
;       [text (document-text document)]
;       [start (text+position->int text (range-start range))]
;       [end (text+position->int text (range-end range))]
;       )
;     (try
;       (vector (text-edit->alist-with-newText (make-text-edit range 
;       ;;;;;;;;;;;;;;;;;;;;;;;;;;
;       )))
;       (except e 
;         [else (vector (text-edit->alist-with-newText (make-text-edit range text)))]))))

;for comment, just get from origin text and pasted to new output
;depth->indent-space-number->parent count
(define (code-format list-of-index-nodes origin-text output-text)
  (let loop ([body list-of-index-nodes] 
      [result output-text])
    (pretty-print 'format3)
    (if (null? body)
      (let* ([line-start (private-line-start result)]
          [append-start (private-append-start result line-start)])
          (substring result 0 append-start))
      (loop (cdr body) (private-transform (car body) origin-text result)))))

(define (private-transform index-node origin-text output-text)
    (pretty-print 'format5)
  (let* ([children (index-node-children index-node)]
      [start (index-node-start index-node)]
      [end (- (index-node-end index-node) 1)]
      [line-start (private-line-start output-text)]
      [append-start (private-append-start output-text line-start)]
      [output-text (substring output-text 0 append-start)]
      [indent-spaces (private-ancestor->indent-space index-node)]
      [indent-space-count (string-length indent-spaces)]
      [bias (- indent-space-count (- append-start line-start))]
      [insert-spaces 
        (string-append 
          (if (> bias 0)
            (substring indent-spaces 0 bias) 
            "")
          (cond 
            [(= append-start line-start) ""]
            [(is-first-child? index-node) ""]
            [(null? (index-node-parent index-node)) ""]
            [else " "]))])
    (pretty-print 'format6)
    (if (null? children)
      (string-append output-text insert-spaces (substring origin-text start end))
      (let* ([first-child (car children)]
          [first-child-start (index-node-start first-child)]
          [result-with-head (string-append output-text insert-spaces (substring origin-text start first-child-start))]
          [result-with-middle (code-format children origin-text result-with-head)]

          [last-child (car (reverse children))]
          [last-child-end (index-node-end first-child)]
          [middle-line-start (private-line-start result-with-middle)]
          [middle-append-start (private-append-start result-with-middle middle-line-start)]
          [end-string (substring origin-text end (+ end 1))]
          [result-with-end (string-append (substring result-with-middle 0 middle-append-start) end-string)])
    (pretty-print 'format7)
        (string-append result-with-end end-string)))))

(define (private-ancestor->indent-space index-node)
  (let ([parent (index-node-parent index-node)])
    (if (null? parent)
      ""
      ;;default indent: 2 space
      (string-append "  " (private-ancestor->indent-space parent)))))

(define (private-line-start text)
  (let loop ([index (- (string-length text) 1)])
    (cond
      [(< index 0) 0]
      [(char=? #\newline (string-ref text index)) (+ 1 index)]
      [(char=? #\return (string-ref text index)) (+ 1 index)]
      [(zero? index) 0]
      [else (loop (- index 1))])))

(define (private-append-start text line-start)
  (let loop ([index (- (string-length text) 1)])
    (cond
      [(< index 0) 0]
      [(not (char-whitespace? (string-ref text index))) (+ 1 index)]
      [(= line-start index) line-start]
      [else (loop (- index 1))])))
)