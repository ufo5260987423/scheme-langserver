(library (scheme-langserver protocol apis hover)
  (export hover)
  (import 
    (chezscheme) 
    
    (only (srfi :13 strings) string-trim)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util io)
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
(define (hover workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      ;why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (string-length (text-document-uri text-document)))) pre-file-node)]
      [document (file-node-document file-node)])
    (refresh-workspace-for workspace file-node)
    (let* ([index-node-list (document-index-node-list document)]
        [target-index-node (pick-index-node-from index-node-list (document+position->bias document (position-line position) (position-character position)))]
        [import-file-node (if (null? target-index-node) '() (ancestor-recursion:index-node-import-file-nodes target-index-node))]
        [prefix 
          (if (index-node? target-index-node)  
            (if (and (null? (index-node-children target-index-node)) (null? import-file-node))
              (annotation-stripped (index-node-datum/annotations target-index-node)) 
              '())
            '())]
        [type-string 
          (if (and (workspace-type-inference? workspace) (null? import-file-node))
            (let* ([types (type:interpret-result-list target-index-node)])
              (fold-left 
                (lambda (prev current)
                  (string-append prev "\n\n" current))
                "## Type Inference"
                (dedupe (apply append (map type:interpret->strings types)))))
            "")])
      (if (null? import-file-node)
        (make-alist 
          'contents 
          (list->vector 
            (append 
              `(
                ,(if (index-node? target-index-node)
                  (string-append 
                    "# "
                    (call-with-string-output-port 
                      (lambda (p) (pretty-print prefix p))))
                  "")
                ,type-string)
              (if (symbol? prefix)
                (dedupe (map identifier-reference->hover (find-available-references-for document target-index-node prefix)))
                '()))))
        '()))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
(define (identifier-reference->hover reference) 
  (if (null? (identifier-reference-index-node reference))
    ""
    (let* (
      ; [not-target-index-node (identifier-reference-initialization-index-node reference)]
        [index-node (identifier-reference-initialization-index-node reference)]
        [document (identifier-reference-document reference)]
        [text (document-text document)]
        [start-pos (index-node-start index-node)]
        [end-pos (index-node-end index-node)]
        [siblings 
          (if (null? (index-node-parent index-node))
            (document-index-node-list document)
            (index-node-children (index-node-parent index-node)))]
        [parent-index-node-end-list 
          (filter 
            (lambda (end-pos) (< end-pos start-pos))
            (sort > (map index-node-end siblings)))]
        [document-index-node-end-list 
          (filter 
            (lambda (end-pos) (< end-pos start-pos)) 
            (sort > (map index-node-end (document-index-node-list document))))])
      (string-append 
        "## Definition\n```scheme\n"
        (string-trim
          (substring 
            text 
            (if (null? parent-index-node-end-list)
              (if (null? document-index-node-end-list) 0 (car document-index-node-end-list))
              (car parent-index-node-end-list)) 
            end-pos))
        "\n```"
            ))))
)