(library (scheme-langserver protocol apis completion)
  (export completion)
  (import 
    (chezscheme) 

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type substitutions rules trivial)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver protocol alist-access-object)

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
      ;why pre-file-node? because many LSP clients, they wrongly produce uri without processing escape character, and here I refer
      ;https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri
      [pre-file-node (walk-file (workspace-file-node workspace) (uri->path (text-document-uri text-document)))]
      [file-node (if (null? pre-file-node) (walk-file (workspace-file-node workspace) (substring (text-document-uri text-document) 7 (string-length (text-document-uri text-document)))) pre-file-node)]
      [document (file-node-document file-node)]
      [text (document-text document)]
      [bias (document+position->bias document (position-line position) (position-character position))]
      [fuzzy (refresh-workspace-for workspace file-node)]
      [index-node-list (document-index-node-list document)]
      [pre-target-index-node (pick-index-node-from index-node-list bias)]
      [target-index-node 
        (if (null? pre-target-index-node)
          pre-target-index-node
          (if (null? (index-node-children pre-target-index-node))
            pre-target-index-node
            (pick-index-node-from index-node-list (- bias 1))))]
      [prefix 
        (if (null? target-index-node)
          ""
          (if (and 
              (null? (index-node-children target-index-node)) 
              (symbol? (annotation-stripped (index-node-datum/annotations target-index-node))))
            (symbol->string (annotation-stripped (index-node-datum/annotations target-index-node)))
            ""))]
      [whole-list
        (if (equal? "" prefix)
          (find-available-references-for document target-index-node)
          (filter 
            (lambda (candidate-reference) 
              (string-prefix? prefix (symbol->string (identifier-reference-identifier candidate-reference))))
            (find-available-references-for document target-index-node)))]
      ; [type-inference? (workspace-type-inference? workspace)]
      [type-inference? #f])
      ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    (list->vector 
      (map 
        (lambda (identifier)
          (identifier-reference->completion-item-alist identifier prefix))
        (if type-inference?
          (sort-with-type-inferences document target-index-node whole-list)
          (sort-identifier-references whole-list))))))

(define (private-generate-position-expression index-node)
  (if (and (not (null? (index-node-parent index-node))) (is-first-child? index-node))
    (let* ([ancestor (index-node-parent index-node)]
        [children (index-node-children ancestor)]
        [rests (cdr children)]
        [rest-variables (map index-node-variable rests)])
      `(,(index-node-variable ancestor) <- (inner:list? ,@rest-variables)))
    (let* ([ancestor (index-node-parent index-node)]
        [children (index-node-children ancestor)]
        [head (car children)]
        [head-variable (index-node-variable head)]
        [rests (cdr children)]
        [rest-variables (map index-node-variable rests)]
        [index (index-of (list->vector rests) index-node)]
        [symbols (generate-symbols-with "d" (length rest-variables))])
      (if (= index (length rests))
        '()
        `((with ((a b c)) 
          ((with ((x ,@symbols))
            ,(vector-ref (list->vector symbols) index))
            c)) 
          ,head-variable)))))

(define (sort-with-type-inferences target-document position-index-node target-identifier-reference-list)
  (print-graph #t)
  (let* ([substitutions (document-substitution-list target-document)]
      [position-expression (private-generate-position-expression position-index-node)]
      [env (make-type:environment substitutions)]
      [position-types (type:interpret-result-list position-expression env)]
      [target-identifiers-with-types 
        (map 
          (lambda (identifier-reference)
            `(,identifier-reference . 
                ,(cond 
                  [(not (null? (identifier-reference-type-expressions identifier-reference))) 
                    (find 
                      (lambda (current-pair)
                        (type:->? (car current-pair) (cadr current-pair) env))
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
                          (type:->? (car current-pair) (cadr current-pair) env))
                        (cartesian-product current-types position-types)))])))
          target-identifier-reference-list)]
      [true-list (map car (filter (lambda (current-pair) (cdr current-pair)) target-identifiers-with-types))]
      [false-list (map car (filter (lambda (current-pair) (not (cdr current-pair))) target-identifiers-with-types))])
    (append 
      (sort-identifier-references true-list)
      (sort-identifier-references false-list))))

(define (identifier-reference->completion-item-alist reference prefix)
  (let* ([s (symbol->string (identifier-reference-identifier reference))]
      [l (string-length prefix)])
    (make-alist 
      'label s
      'insertText (substring s (- l 1) (string-length s))
      )))
)
