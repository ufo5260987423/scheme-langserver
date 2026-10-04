(library (scheme-langserver analysis identifier self-defined-rules router)
  (export route&add
    hand-written-rule-covers?
    lazy-expand-enclosing-call)
  (import 
    (chezscheme)
    (scheme-langserver util path)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier expanders expansion-wrap)
    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
    (scheme-langserver analysis identifier self-defined-rules ufo-match match)
    (scheme-langserver analysis identifier self-defined-rules ufo-try try)
    
    (scheme-langserver analysis identifier self-defined-rules goldfish define-case-class)
    (scheme-langserver analysis identifier self-defined-rules goldfish let1)
    (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda))

(define (route&add 
  rules target-identifier
  file-linkage identifier-list current-document expanded+callee-list memory
  add-rule-procedure step)
  (let* ([top (root-ancestor target-identifier)]
        [expressions (map identifier-reference-identifier top)]
        [library-identifiers (map identifier-reference-library-identifier top)])
    (cond 
      [(and (equal? library-identifiers '((srfi :23 error tricks))) (equal? expressions '(SRFI-23-error->R6RS)))
        (add-rule-procedure rules `((,do-nothing . ,begin-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((srfi private include))) (equal? expressions '(include/resolve)))
        (let ([target-lambda 
            (lambda (root-file-node root-library-node document index-node)
              (include-resolve-process root-file-node root-library-node document index-node 
                (lambda (current-document) 
                  (file-linkage-set! file-linkage (uri->path (document-uri document)) (uri->path (document-uri current-document)))
                  (step root-file-node root-library-node file-linkage current-document expanded+callee-list memory))))])
          (add-rule-procedure rules `((,target-lambda) . ,target-identifier)))]
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
        (add-rule-procedure rules `((,try-process) . ,target-identifier))]
      ;Eager auto-expansion for the match macro family entry point (Q2):
      ;expanding match one layer at analysis time establishes clause
      ;bindings via the generated let structure; deeper cascade members
      ;(match-next/match-one/...) intentionally have no eager rule and
      ;fall back to query-time lazy expansion. The expander is looked up
      ;through the ancestor chain because imported references do not copy
      ;the syntax-expander field. Falls back to the hand-written rule when
      ;no expander is available (e.g. stale cache state).
      ;Eager auto-expansion for the whole ufo-match macro family (Q2):
      ;expanding match fully at analysis time establishes clause bindings
      ;through the cascade (match -> match-next -> match-one -> ...), the
      ;coverage the hand-written match-process used to declare directly.
      ;The expander is looked up through the ancestor chain because
      ;imported references do not copy the syntax-expander field. Falls
      ;back to the hand-written rule for match when no expander is
      ;available (e.g. stale cache state); the recursion guard (memory
      ;cap) bounds pathological cascades.
      [(and (equal? library-identifiers '((ufo-match)))
        (find (lambda (e) (equal? expressions (list e)))
          '(match match-next match-one match-check-ellipsis match-two match-check-identifier)))
        (let ([expander
            (or (identifier-reference-syntax-expander target-identifier)
              (guard (e [else #f])
                (identifier-reference-syntax-expander
                  (find identifier-reference-syntax-expander (root-ancestor target-identifier)))))])
          (if expander
            (add-rule-procedure rules
              `((,(expansion-generator->rule expander step file-linkage expanded+callee-list memory target-identifier)) .
                ,target-identifier))
            (if (equal? expressions '(match))
              (add-rule-procedure rules `((,match-process) . ,target-identifier))
              rules)))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(let1)))
        (add-rule-procedure rules `((,let1-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii oop))) (equal? expressions '(define-case-class)))
        (add-rule-procedure rules `((,define-case-class-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(typed-lambda)))
        (add-rule-procedure rules `((,typed-lambda-process) . ,target-identifier))]

      ; [(identifier-reference-syntax-expander target-identifier)
      ;   (add-rule-procedure rules 
      ;     `((,(expansion-generator->rule (identifier-reference-syntax-expander target-identifier) step file-linkage expanded+callee-list memory target-identifier)) . 
      ;       ,target-identifier))]
      [else rules])))

;the set of (library-identifiers, expressions) pairs that route&add covers
;with hand-written rules. Keep in sync with the cond arms above; the lazy
;expansion path uses this to avoid double-handling macros that already have
;precise hand-written semantics.
(define (hand-written-rule-covers? target-identifier)
  (let* ([top (root-ancestor target-identifier)]
      [expressions (map identifier-reference-identifier top)]
      [library-identifiers (map identifier-reference-library-identifier top)])
    (or
      (and (equal? library-identifiers '((srfi :23 error tricks))) (equal? expressions '(SRFI-23-error->R6RS)))
      (and (equal? library-identifiers '((srfi private include))) (equal? expressions '(include/resolve)))
      (and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
      ;match is handled by eager auto-expansion (see route&add above), not
      ;by a hand-written rule -- keep this predicate in sync with route&add
      (and (equal? library-identifiers '((liii base))) (equal? expressions '(let1)))
      (and (equal? library-identifiers '((liii oop))) (equal? expressions '(define-case-class)))
      (and (equal? library-identifiers '((liii base))) (equal? expressions '(typed-lambda))))))

;Query-time lazy expansion (Q2): walk ancestors of index-node and expand the
;first one that is a macro call carrying a syntax-expander without a
;hand-written rule. Returns #t iff an expansion actually ran. Callers should
;re-query references after a #t result. Re-expansion is possible on repeated
;calls; downstream dedupe (shallow-copy's per-identifier export check and
;merge-ordered-lists) keeps duplication bounded.
(define (lazy-expand-enclosing-call root-file-node root-library-node file-linkage document index-node step)
  (let loop ([node index-node])
    (cond
      [(or (not node) (null? node)) #f]
      [else
        (let* ([expr (guard (e [else '()])
                  (annotation-stripped (index-node-datum/annotations node)))]
            [head (if (pair? expr) (car expr) #f)])
          (if (symbol? head)
            (let ([expander-reference
                (find
                  (lambda (r)
                    (and (or (identifier-reference-syntax-expander r)
                          (find identifier-reference-syntax-expander (root-ancestor r)))
                      (not (hand-written-rule-covers? r))))
                  (guard (e [else '()])
                    (find-available-references-for document node head)))])
              (if expander-reference
                (let ([expander
                    (or (identifier-reference-syntax-expander expander-reference)
                      (guard (e [else #f])
                        (identifier-reference-syntax-expander
                          (find identifier-reference-syntax-expander (root-ancestor expander-reference)))))])
                  ((expansion-generator->rule expander
                    step file-linkage '() '() expander-reference)
                    root-file-node root-library-node document node)
                  #t)
                (loop (index-node-parent node))))
            (loop (index-node-parent node))))]))))
