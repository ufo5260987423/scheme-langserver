(library (scheme-langserver protocol apis hover)
  (export hover)
  (import 
    (chezscheme) 
    
    (only (srfi :13 strings) string-trim)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

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
      [uri (text-document-uri text-document)]
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)])
    (refresh-workspace-for workspace file-node)
    (let* ([index-node-list (document-index-node-list document)]
        [target-index-node (pick-index-node-from index-node-list (text+position->int (document-text document) position))]
        [prefix (if (null? (index-node-children target-index-node)) (annotation-stripped (index-node-datum/annotations target-index-node)) '())])
      (if (null? prefix)
        '()
        (make-alist 
          'content 
          (list->vector (dedupe (map identifier-reference->hover (find-available-references-for document target-index-node prefix)))))))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
(define (identifier-reference->hover reference)
  (if (null? (identifier-reference-index-node reference))
    (symbol->string (identifier-reference-identifier reference))
    (let* ([not-target-index-node (identifier-reference-index-node reference)]
        [index-node (index-node-parent not-target-index-node)]
        [document (identifier-reference-document reference)]
        [text (document-text document)]
        [start-pos (index-node-start index-node)]
        [end-pos (index-node-end index-node)]
        [parent-index-node-end-list 
          (filter 
            (lambda (end-pos) (< end-pos start-pos))
            (sort > (map index-node-end (index-node-children (index-node-parent index-node)))))]
        [document-index-node-end-list 
          (filter 
            (lambda (end-pos) (< end-pos start-pos)) 
            (sort > (map index-node-end (document-index-node-list document))))])
      (string-trim
        (substring 
          text 
          (if (null? parent-index-node-end-list)
            (if (null? document-index-node-end-list) 0 (car document-index-node-end-list))
            (car parent-index-node-end-list)) 
          end-pos)))))
)