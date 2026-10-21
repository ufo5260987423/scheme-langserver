(library (scheme-langserver analysis identifier self-defined-rules router)
  (export route&add)
  (import
    (chezscheme)

    (scheme-langserver util path)

    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier ir-auto-route)
    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
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
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(let1)))
        (add-rule-procedure rules `((,let1-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii oop))) (equal? expressions '(define-case-class)))
        (add-rule-procedure rules `((,define-case-class-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(typed-lambda)))
        (add-rule-procedure rules `((,typed-lambda-process) . ,target-identifier))]

      ;Eager auto-expansion for the ufo-match macro family -- now IR-ONLY
      ;(route 3, ir-auto-route; route 2's generator pipeline was retired
      ;2026-10-21 per doc/design/route2-retirement-proposal.md). The family
      ;gate stays: it selects which identifiers get the IR rule. The IR path
      ;needs NO langserver-side expander -- Chez's own expander does the work,
      ;resolving match against the env bootstrapped by ir-auto-route (host
      ;import fast path, else workspace-library load). On ANY failure the call
      ;site silently gets no expansion-derived references: see
      ;ir-auto-route:stats / REPLAY_TRACE. SLSCHEME_IR_ROUTE=0 now means "no
      ;auto-resolution for the family" (the old route-2 opt-out is gone with
      ;the pipeline).
      [(and (equal? library-identifiers '((ufo-match)))
          (find (lambda (e) (equal? expressions (list e)))
            '(match match-next match-one match-check-ellipsis match-two match-check-identifier)))
        (add-rule-procedure rules `((,(ir-auto-route:try-expand)) . ,target-identifier))]
      [else rules])))
)
