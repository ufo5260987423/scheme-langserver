(library (scheme-langserver protocol apis completion)
  (export completion)
  (import 
    (chezscheme) 

    (scheme-langserver analysis type domain-specific-language interpreter)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util natural-order-compare)
    (scheme-langserver util association)
    (scheme-langserver util cartesian-product)
    (scheme-langserver util path) 
    (scheme-langserver util io)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
(define (completion workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [bias (text+position->int text position)]
      [fuzzy (refresh-workspace-for workspace file-node)]
      [index-node-list (document-index-node-list document)]
      [target-index-node (pick-index-node-from index-node-list bias)]
      [prefix 
        (if target-index-node 
          (if (null? (index-node-children target-index-node)) 
            (if (null? (annotation-stripped (index-node-datum/annotations target-index-node)))
              ""
              (symbol->string (annotation-stripped (index-node-datum/annotations target-index-node))))
            ""))]
      [whole-list
        (filter 
          (lambda (candidate-reference) (string-prefix? prefix (symbol->string (identifier-reference-identifier candidate-reference)))) 
          (find-available-references-for document target-index-node))]
      [type-inference? (workspace-type-inference? workspace)])
    ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    (list->vector (map 
      identifier-reference->completion-item-alist 
      (if type-inference?
        (sort-with-type-inferences (document-substitution-list document) target-index-node whole-list)
        (sort-identifier-references whole-list))))))

(define (sort-with-type-inferences substitutions position-index-node target-identifier-reference-list)
  (let* ([position-variable (index-node-variable position-index-node)]
      [env (make-type:environment substitutions)]
      [position-types (type:interpret-result-list position-variable env)]
      [target-identifiers-with-types 
        (map 
          (lambda (identifier-reference)
            `(,identifier-reference . 
                ,(cond 
                  [(not (null? (identifier-reference-type-expressions identifier-reference))) 
                    (find 
                      (lambda (current-pair)
                        (type:->? (car current-pair) (cdr current-pair) env))
                      (cartesian-product (identifier-reference-type-expressions identifier-reference) position-types))]
                  [(null? (identifier-reference-index-node identifier-reference)) #f]
                  [else 
                    (let* ([current-index-node (identifier-reference-index-node identifier-reference)]
                        [current-variable (index-node-variable current-index-node)]
                        [current-document (identifier-reference-document identifier-reference)]
                        [current-substitutions (document-substitution-list current-document)]
                        [current-env (make-type:environment current-substitutions)]
                        [current-types (type:interpret-result-list current-variable current-env)])
                      (if (null? (identifier-reference-type-expressions identifier-reference))
                        (identifier-reference-type-expressions-set! identifier-reference current-types))
                      (find 
                        (lambda (current-pair)
                          (type:->? (car current-pair) (cdr current-pair) env))
                        (cartesian-product current-types position-types)))])))
          target-identifier-reference-list)]
      [true-list (map car (filter (lambda (current-pair) (cdr current-pair)) target-identifiers-with-types))]
      [false-list (map car (filter (lambda (current-pair) (not (cdr current-pair))) target-identifiers-with-types))])
    (append 
      (sort-identifier-references true-list)
      (sort-identifier-references false-list))))

(define (identifier-reference->completion-item-alist reference)
  (make-alist 'label (symbol->string (identifier-reference-identifier reference))))
)
