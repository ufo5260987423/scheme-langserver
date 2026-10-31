(library (scheme-langserver protocol apis hover)
  (export hover)
  (import
    (chezscheme)

    (only (srfi :13 strings) string-trim string-join)

    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type semantic-subtyping semtype)

    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association)
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

; ------------------------------------------------------------------
; occurrence-typing narrowing display (route-4 demo, type-inference-
; precedents.md §2.5): when the hovered symbol sits in a branch of
; (if (pred? x) then else) and x IS the hovered symbol, the route-4
; engine narrows the symbol's candidate types with sem:restrict (then)
; or sem:remove (else) and the delta is shown under "## Narrowed".
; Heuristic on purpose: same-symbol matching and the builtin predicate
; table below; anything undetermined stays silent.
; ------------------------------------------------------------------
(define PRIVATE-NARROW-PREDICATES
  '((number? . number?) (integer? . integer?) (rational? . rational?)
    (real? . real?) (complex? . complex?) (fixnum? . fixnum?)
    (bignum? . bignum?) (flonum? . flonum?) (string? . string?)
    (char? . char?) (boolean? . boolean?) (symbol? . symbol?)))

(define (private:within? target ancestor)
  (let loop ([node target])
    (cond
      [(eq? node ancestor) #t]
      [(not (index-node? node)) #f]
      [else (loop (index-node-parent node))])))

(define (private:occurrence-narrowing document target-index-node)
  (let ([target-expr (annotation-stripped (index-node-datum/annotations target-index-node))])
    (and (symbol? target-expr)
      (let loop ([node (index-node-parent target-index-node)])
        (and (index-node? node)
          (let ([expr (annotation-stripped (index-node-datum/annotations node))])
            (if (and (list? expr) (eq? (car expr) 'if) (pair? (cdr expr)))
              (let* ([children (index-node-children node)]
                  [then-node (and (> (length children) 2) (list-ref children 2))]
                  [else-node (and (> (length children) 3) (list-ref children 3))]
                  [branch
                    (cond
                      [(and then-node (private:within? target-index-node then-node)) 'then]
                      [(and else-node (private:within? target-index-node else-node)) 'else]
                      [else #f])])
                (if branch
                  (private:narrow-from-test document target-index-node (cadr children) target-expr branch)
                  (loop (index-node-parent node))))
              (loop (index-node-parent node)))))))))

;narrows each convertible candidate, drops the emptied ones, and
;returns (original . narrowed) as sem:type unions -- #f when no candidate
;converts. The change check is at the SET level: an already-precise
;candidate surviving unchanged is still part of a narrowed set.
(define (private:narrow-set candidates tau branch)
  (let loop ([cs candidates] [orig (sem:empty)] [narrowed (sem:empty)] [any #f])
    (if (null? cs)
      (and any (cons orig narrowed))
      (let ([s (private:narrow-convert (car cs))])
        (if s
          (let ([r ((if (eq? branch 'then) sem:restrict sem:remove) s tau)])
            (loop (cdr cs)
              (sem:union orig s)
              (if (sem:is-empty? r) narrowed (sem:union narrowed r))
              #t))
          (loop (cdr cs) orig narrowed any))))))

;candidates are sometimes index-nodes (uninterpreted bindings); unfold one
;interpretation level and union whatever converts.
(define (private:narrow-convert candidate)
  (or (sem:convert candidate)
    (and (index-node? candidate)
      (let* ([inner (type:interpret-result-list candidate)]
          [convs (filter (lambda (x) x) (map sem:convert inner))])
        (and (not (null? convs))
          (fold-left sem:union (sem:empty) convs))))))



(define (private:narrow-arg target-index-node arg-node target-sym pred branch test-expr)
  (and (eq? (annotation-stripped (index-node-datum/annotations arg-node)) target-sym)
    (let* ([tau (sem:convert (cdr pred))]
        [result
          (private:narrow-set
            (type:interpret-result-list target-index-node)
            tau
            branch)])
      (and result
        (not (sem:equivalent? (car result) (cdr result)))
        (not (sem:is-empty? (cdr result)))
        (string-append
          "\n\n## Narrowed ("
          (if (eq? branch 'then) "then" "else")
          "-branch of (" (symbol->string (car test-expr)) " ...))\n"
          (string-join
            (map symbol->string (reverse (sem:base-symbols (cdr result))))
            " | "))))))

(define (private:narrow-from-test document target-index-node test-node target-sym branch)
  (let ([test-expr (annotation-stripped (index-node-datum/annotations test-node))])
    (and (list? test-expr)
      (pair? (cdr test-expr))
      (let ([pred (assq (car test-expr) PRIVATE-NARROW-PREDICATES)]
          [test-children (index-node-children test-node)])
        (and pred
          (> (length test-children) 1)
          (private:narrow-arg
            target-index-node
            (list-ref test-children 1)
            target-sym
            pred
            branch
            test-expr))))))

; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
(define (hover workspace params)
  (let* ([text-document (alist->text-document (assq-ref params 'textDocument))]
      [position (alist->position (assq-ref params 'position))]
      [line (position-line position)]
      [character (position-character position)]
      [file-node (resolve-uri->file-node (workspace-file-node workspace) (text-document-uri text-document))])
    (if (null? file-node)
      'null
      (let* ([document (file-node-document file-node)])
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
                    ,(string-append type-string
                        (or (private:occurrence-narrowing document target-index-node) "")))
                  (if (symbol? prefix)
                    (dedupe (map identifier-reference->hover (find-available-references-for document target-index-node prefix)))
                    '()))))
            '()))))))

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
