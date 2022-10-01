(library (scheme-langserver protocol method)
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

    (only (srfi :13 strings) string-prefix?))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionParams
(define (completion workspace params)
;;todo:get mutex
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->postion (assq-ref params 'position))]
      [file-node (walk-file (workspace-file-node workspace) (text-document-uri text-document))]
      [document (file-node-document file-node)]
      [index-node (document-index-node document)]
      [target-index-node (pick-index-node-by index-node (text+postion->int (document-text document) postion))]
      [text (annotation-expression (index-node-datum/annotations target-index-node))]
      [candidate-references 
        (sort
          (lambda (a b) (natrual-order-compare (symbo->string (reference-identifier a)) (symbo->string (reference-identifier b))))
          (filter 
            (lambda (candidate-reference) (string-prefix? prefix (symbol->string (reference-identifier candidate-reference)))) 
            (find-available-references-for target-index-node)))])
    ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionList
    (map reference->completion-item-alist candidate-references)))

(define (reference->completion-item-alist reference)
  (make-alist 'label (symbol->string (reference-identifier reference))))

(define (text+postion->int text postion)
  (let loop ([current-line 0]
      [current-line-start-postion 0])
    (let ([next-line-start-postion 
          (if (string-index text #\newline (+ 1 current-line-start-postion))
            (string-index text #\newline (+ 1 current-line-start-postion))
            -1)]
          [maybe-result (+ current-postion (postion-character postion))])
      (cond
        [(and (= current-line (postion-line postion)) (< maybe-result next-line-start-postion)) 
          maybe-result]
        [(< current-line (postion-line postion)) 
          (loop (+ 1 current-line) next-line-start-postion)]
        [else (raise 'postion-out-of-range)]))))
)