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
    (scheme-langserver analysis identifier expander ir-auto-route)
    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
    (scheme-langserver analysis identifier self-defined-rules ufo-try try)

    (scheme-langserver analysis identifier self-defined-rules goldfish define-case-class)
    (scheme-langserver analysis identifier self-defined-rules goldfish let1)
    (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda))

(define (route&add
  rules target-identifier
  file-linkage
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
                  (step root-file-node root-library-node file-linkage current-document))))])
          (add-rule-procedure rules `((,target-lambda) . ,target-identifier)))]
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
        (add-rule-procedure rules `((,try-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(let1)))
        (add-rule-procedure rules `((,let1-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii oop))) (equal? expressions '(define-case-class)))
        (add-rule-procedure rules `((,define-case-class-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(typed-lambda)))
        (add-rule-procedure rules `((,typed-lambda-process) . ,target-identifier))]

      ;Generic arm -- ANY define-syntax-headed call site whose head resolved
      ;to a library (root-ancestor carries a non-empty library-identifier),
      ;not just the ufo-match family / match-index-node: the IR path is
      ;macro-agnostic (head symbol + library come from the call site itself,
      ;the env is bootstrapped from the workspace's own copy of THAT
      ;library), so the two retired family arms (2026-10-29) were the last
      ;family-specific holdouts. OPT-IN via ir-auto-route:enabled?
      ;(default off; run.ss -a enable). Hand rules above keep priority (the
      ;include arm has file-linkage side effects, ufo-try/goldfish carry
      ;semantics beyond reference synthesis). Unresolvable heads (empty
      ;library-identifier, e.g. script-level or let-syntax-bound macros)
      ;fall through silently, as before.
      [(and (find (lambda (t) (eq? 'syntax-variable t))
              (map identifier-reference-type top))
         (find (lambda (lib) (not (null? lib))) library-identifiers))
        (add-rule-procedure rules `((,(ir-auto-route:try-expand)) . ,target-identifier))]
      [else rules])))
)
