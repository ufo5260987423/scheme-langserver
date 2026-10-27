(library (scheme-langserver analysis type substitutions self-defined-rules router)
  (export route&add)
  (import 
    (rnrs)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis type substitutions self-defined-rules ufo-try try))

(define (route&add rules target-identifier root-library-node macro-infer add-rule-procedure)
  (let* ([top (root-ancestor target-identifier)]
        [expressions (map identifier-reference-identifier top)]
        [library-identifiers (map identifier-reference-library-identifier top)])
    (cond
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
        (add-rule-procedure rules `((,try-process) . ,target-identifier))]

      ;;GENERIC MACRO ARM (mimics the identifier side's automatic macro
      ;;resolution): any define-syntax-headed call site is EXPANDED by
      ;;Chez, the type generator runs over the reconstructed expansion,
      ;;and the inferred substitutions map back to the call site by
      ;;source position -- macro-agnostic, no hand-written pattern
      ;;semantics. try above keeps its hand rule (a control-flow form).
      [(find (lambda (t) (eq? 'syntax-variable t))
              (map identifier-reference-type top))
        (display "[type-router] generic arm: ")
        (display expressions)
        (newline)
        (add-rule-procedure
          rules
          `((,(lambda (document index-node)
                (macro-infer root-library-node document index-node)))
            . ,target-identifier))]

      [else rules])))
)