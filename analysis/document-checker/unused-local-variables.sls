(library (scheme-langserver analysis document-checker unused-local-variables)
  (export 
    check-unused-local-variables)
  (import 
    (chezscheme)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused local variable detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collect symbols that are explicitly exported from the document's
; top-level (library ...) / (define-library ...) form.  Local bindings
; whose identifiers appear here must not be reported as unused.
(define (private:collect-exported-identifiers document)
  (let ([exported (make-eq-hashtable)])
    (for-each
      (lambda (top-node)
        (let ([expr (annotation-stripped (index-node-datum/annotations top-node))])
          (when (and (pair? expr) (or (eq? 'library (car expr)) (eq? 'define-library (car expr))))
            (for-each
              (lambda (body-node)
                (let ([body-expr (annotation-stripped (index-node-datum/annotations body-node))])
                  (when (and (pair? body-expr) (eq? 'export (car body-expr)))
                    (for-each
                      (lambda (export-item-node)
                        (private:collect-export-item-identifiers exported export-item-node))
                      (cdr (index-node-children body-node))))))
              (cddr (index-node-children top-node))))))
      (document-index-node-list document))
    exported))

(define (private:collect-export-item-identifiers exported export-item-node)
  (let ([export-expr (annotation-stripped (index-node-datum/annotations export-item-node))])
    (cond
      [(symbol? export-expr)
        (eq-hashtable-set! exported export-expr #t)]
      [(and (pair? export-expr) (eq? 'rename (car export-expr)))
        (for-each
          (lambda (pair-node)
            (let ([pair-expr (annotation-stripped (index-node-datum/annotations pair-node))])
              (when (and (pair? pair-expr) (symbol? (car pair-expr)))
                (eq-hashtable-set! exported (car pair-expr) #t))))
          (cdr (index-node-children export-item-node)))]
      [else (void)])))

(define (private:collect-local-binding-references document)
  ; Local bindings may live either in document-ordered-reference-list (e.g.
  ; with-syntax syntax-parameters) or in index-node-references-export-to-other-node
  ; of the identifier leaf node (e.g. define/lambda/let).  Collect from both
  ; places and dedupe by the binding's index-node to avoid duplicate diagnostics.
  (let ([result '()] [seen (make-eq-hashtable)])
    (define (add! ref)
      (let ([index-node (identifier-reference-index-node ref)])
        (when (and (eq? (identifier-reference-document ref) document)
                (null? (identifier-reference-library-identifier ref))
                (index-node? index-node)
                (not (eq-hashtable-contains? seen index-node)))
          (eq-hashtable-set! seen index-node #t)
          (set! result (cons ref result)))))
    (for-each add! (document-ordered-reference-list document))
    (let walk ([node (document-index-node-list document)])
      (cond
        [(null? node) (void)]
        [(pair? node)
          (walk (car node))
          (walk (cdr node))]
        [(index-node? node)
          (for-each add! (index-node-references-import-in-this-node node))
          (for-each add! (index-node-references-export-to-other-node node))
          (for-each add! (index-node-excluded-references node))
          (walk (index-node-children node))]
        [else (void)]))
    result))

; Only parameters (lambda/case-lambda/define parameter-list formals) are
; reported.  Top-level define names and let-bound variables are intentionally
; skipped to avoid forward-reference and import-rename false positives.
(define (private:underscore-prefixed? id)
  (let ([s (symbol->string id)])
    (and (> (string-length s) 0) (char=? (string-ref s 0) #\_))))

; Parameters whose name starts with "_" are conventionally ignored:
; the author explicitly marks them as intentionally unused (e.g. callbacks
; required by a fixed-arity protocol, rest-arg sinks like do-nothing).
(define (check-unused-local-variables document)
  (let* ([exported-ht (private:collect-exported-identifiers document)]
      [seen (make-eq-hashtable)])
    (for-each
      (lambda (ref)
        (when (and (not (eq-hashtable-contains? seen ref))
                (eq? (identifier-reference-type ref) 'parameter)
                (zero? (identifier-reference-usage-count ref)))
          (let ([id (identifier-reference-identifier ref)])
            (when (and (symbol? id)
                    (not (eq-hashtable-contains? exported-ht id))
                    (not (private:underscore-prefixed? id)))
              (eq-hashtable-set! seen ref #t)
              (let ([index-node (identifier-reference-index-node ref)])
                (when (index-node? index-node)
                  (append-new-diagnoses document
                    `(,(index-node-start index-node) ,(index-node-end index-node) 2
                      ,(string-append "Unused local variable: " (symbol->string id))
                      "identifier" "unused-local-variable"))))))))
      (private:collect-local-binding-references document))))
)
