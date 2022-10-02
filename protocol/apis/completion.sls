(library (scheme-langserver protocol apis completion)
  (export completion)
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

    (only (srfi :13 strings) string-prefix? string-index))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
(define (completion workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [index-node (document-index-node document)]
      [target-index-node (pick-index-node-by index-node (text+position->int (document-text document) position))]
      [prefix (if (null? (index-node-children target-index-node)) (annotation-expression (index-node-datum/annotations target-index-node)) "")])
    ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    ; (pretty-print
    ;     (filter 
    ;       (lambda (candidate-reference) (string-prefix? prefix (symbol->string (identifier-reference-identifier candidate-reference)))) 
    ;       (find-available-references-for target-index-node)))
    (pretty-print (index-node-datum/annotations (index-node-parent target-index-node)))
    (pretty-print (index-node-references-import-in-this-node (index-node-parent target-index-node)))
    (pretty-print (index-node-references-export-to-other-node (index-node-parent target-index-node)))
    (pretty-print (find-available-references-for target-index-node))
    (map 
      identifier-reference->completion-item-alist 
      (sort
        (lambda (a b) (natural-order-compare (symbol->string (identifier-reference-identifier a)) (symbol->string (identifier-reference-identifier b))))
        (filter 
          (lambda (candidate-reference) (string-prefix? prefix (symbol->string (identifier-reference-identifier candidate-reference)))) 
          (find-available-references-for target-index-node)))
          )))

(define (identifier-reference->completion-item-alist reference)
  (make-alist 'label (symbol->string (identifier-reference-identifier reference))))

(define (text+position->int text position)
  (let loop ([current-line 0]
      [current-line-start-position 0])
    (let ([next-line-start-position 
          (if (string-index text #\newline (+ 1 current-line-start-position))
            (string-index text #\newline (+ 1 current-line-start-position))
            -1)]
          [maybe-result (+ current-line-start-position (position-character position))])
      (cond
        [(and (= current-line (position-line position)) (< maybe-result next-line-start-position)) 
          maybe-result]
        [(< current-line (position-line position)) 
          (loop (+ 1 current-line) next-line-start-position)]
        [else (raise 'position-out-of-range)]))))
)