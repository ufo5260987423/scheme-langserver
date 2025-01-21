(library (scheme-langserver analysis type substitutions self-defined-rules router)
  (export route&add)
  (import 
    (rnrs)
    (scheme-langserver util path)
    (scheme-langserver util contain)

    (scheme-langserver analysis util)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis type substitutions rules application)

    (scheme-langserver analysis type substitutions self-defined-rules ufo-try try))

(define (route&add rules target-identifier add-rule-procedure)
  (let* ([top (root-ancestor target-identifier)]
        [expressions (map identifier-reference-identifier top)]
        [library-identifiers (map identifier-reference-library-identifier top)])
    (cond 
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
        (add-rule-procedure rules `((,try-process) . ,target-identifier))]
      ; [(and (equal? library-identifiers '((ufo-match))) (equal? expressions '(match)))
      ;   (add-rule-procedure rules `((,case-process) . ,target-identifier))]
      [else rules])))
)