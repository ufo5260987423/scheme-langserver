(library (scheme-langserver protocol apis references)
  (export 
    find-references)
  (import 
    (chezscheme) 

    (ufo-match)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util natural-order-compare)
    (scheme-langserver util association)
    (scheme-langserver util path) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_references
(define (find-references workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [path (uri->path (text-document-uri text-document))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [index-node-list (document-index-node-list document)]
      [text (document-text document)]
      [bias (text+position->int text position)]
      [target-index-node (pick-index-node-from index-node-list bias)]
      [prefix 
        (if target-index-node 
          (if (null? (index-node-children target-index-node)) 
            (symbol->string (annotation-expression (index-node-datum/annotations target-index-node)))
            ""))])
    (if (equal? "" prefix)
      '#()
      (let ([available-references (find-available-references-for document target-index-node prefix)]
          [path-to-list (get-reference-path-to (workspace-file-linkage workspace) path)]
          [root-file-node (workspace-file-node workspace)]
          [predicate? 
            (lambda (maybe-symbol)
              (if (symbol? maybe-symbol)
                (find 
                  (lambda (identifier) (equal? maybe-symbol identifier))
                  (map identifier-reference-identifier available-references))
                #f))])
        (list->vector (apply append (map 
          (lambda (document) 
            (apply append (map (lambda (index-node) 
              (find-references-in document index-node))
                (document-index-node-list document))))
          (map (lambda(local-path) (file-node-document (walk-file root-file-node local-path))) path-to-list))))))))

(define (find-references-in document index-node available-references predicate?)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-expression ann)]
      [children (index-node-children index-node)])
    (match expression
      [(? predicate? maybe-symbol) 
        (let ([result 
              (find 
                (lambda (candidate-reference) 
                  (if (find (lambda (cr) (equal? cr candidate-reference)) available-references)
                    #t
                    #f))
                (find-available-references-for document target-index-node maybe-symbol))])
          (if result
            `(,(make-location
              (document-uri document) 
              (make-range
                (int+text->position (index-node-start index-node) (document-text document))
                (int+text->position (index-node-end index-node) (document-text document)))))
            '()))]
      [else 
        (if (null? children)
          '()
          (apply append
            (map (lambda (child-index-node) (find-references-in document child-index-node available-references)) children)))])))
)
