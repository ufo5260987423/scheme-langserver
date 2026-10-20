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
    (scheme-langserver analysis identifier expanders expansion-wrap)
    (scheme-langserver analysis identifier ir-auto-route)
    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
    ;ufo-match/match (match-process) import removed 2026-10-05: the hand-written fallback is disabled, nothing references it here
    (scheme-langserver analysis identifier self-defined-rules ufo-try try)

    (scheme-langserver analysis identifier self-defined-rules goldfish define-case-class)
    (scheme-langserver analysis identifier self-defined-rules goldfish let1)
    (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda))

(define (private:find-expander identifier)
  ;expander of the reference itself, else of any ancestor, with a cycle-safe breadth-first walk -- root-ancestor cannot be reused here
  ;because parent chains may be cyclic and its naive recursion diverges
  (or (identifier-reference-syntax-expander identifier)
    (let loop ([rest (identifier-reference-parents identifier)] [seen (make-eq-hashtable)])
      (cond
        [(null? rest) #f]
        [(hashtable-ref seen (car rest) #f)
          (loop (cdr rest) seen)]
        [else
          (hashtable-set! seen (car rest) #t)
          (or (identifier-reference-syntax-expander (car rest))
            (loop
              (append (identifier-reference-parents (car rest)) (cdr rest))
              seen))]))))

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

      ;Eager auto-expansion for the ufo-match macro family (Q2): expanding match fully at analysis time establishes clause bindings through the
      ;cascade (match -> match-next -> match-one -> ...), the coverage the hand-written match-process used to declare directly. Measured at
      ;+231 ms init for this repo's ~57 call sites. The generic fallback below stays OFF: unfiltered eager expansion of every .akku/lib macro
      ;made init exceed 5 minutes (measured 2026-10-04). The expander is taken from the reference itself first; imported references do not
      ;copy the syntax-expander field, hence the ancestor-chain fallback (taking the expander OF the found reference, not the reference).
      [(and (equal? library-identifiers '((ufo-match)))
        (find (lambda (e) (equal? expressions (list e)))
          '(match match-next match-one match-check-ellipsis match-two match-check-identifier)))
        (let ([expander (private:find-expander target-identifier)])
          (if expander
            (add-rule-procedure rules
              ;Route-3 controlled parallel phase (env-gated, silent fallback to
              ;the route-2 rule; see ir-auto-route and doc/analysis/chez-sc-expand-
              ;records-switch.md §9). With the gate off the wrapper is a pass-through.
              `((,(ir-auto-route:wrap-rule
                  (expansion-generator->rule expander step file-linkage expanded+callee-list memory target-identifier))) .
                ,target-identifier))
            ;Hand-written match-process fallback DISABLED 2026-10-05: a full-repo probe shows the auto path (expander present)
            ;covers 100% of cross-file match-family call sites; the no-expander remainder resolves to quoted data or in-library
            ;self-calls that never reach this arm. Removing the fallback aligns match with the other five family macros (no
            ;hand-written rule either). Original line kept commented: (if (equal? expressions '(match))
            ;(add-rule-procedure rules `((,match-process) . ,target-identifier)) rules)
            rules))]
      [else rules])))
)
