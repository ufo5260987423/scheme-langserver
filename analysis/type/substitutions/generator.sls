(library (scheme-langserver analysis type substitutions generator)
  (export 
    construct-substitutions-for
    type-inference-advanced?)
  (import 
    (chezscheme)
    (ufo-try)
    (ufo-match-steer)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    
    (scheme-langserver analysis type substitutions rules if)
    (scheme-langserver analysis type substitutions rules cond)
    (scheme-langserver analysis type substitutions rules do)
    (scheme-langserver analysis type substitutions rules let)
    (scheme-langserver analysis type substitutions rules let*)
    (scheme-langserver analysis type substitutions rules lambda)
    (scheme-langserver analysis type substitutions rules case-lambda)
    (scheme-langserver analysis type substitutions rules record)
    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type substitutions rules define)
    (scheme-langserver analysis type substitutions rules application)
    (scheme-langserver analysis type substitutions rules begin)
    (scheme-langserver analysis type substitutions rules unless)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver analysis type substitutions fixpoint)
    (scheme-langserver analysis type substitutions rest-elements)
    (scheme-langserver analysis type substitutions macro-expansion)
    (scheme-langserver analysis type substitutions self-defined-rules router))

;;OPT-IN gate for the expensive type-inference passes (P1 recursive-return
;;fixpoint + P3 dotted-rest element inference), measured 2026-11-05 on this
;;repo: with the gate off, full init with type inference drops from ~32 s
;;to ~12 s, and the pathological single interpretation in srfi-115's
;;regexp-impl.scm (~22 s, uninterruptible by the passes' between-calls
;;budgets) never runs. Default off; set BEFORE the thread pool spawns
;;(init-server's 11-argument form, run.ss -t advance) so forked workers
;;inherit it; shorter init-server arities and tests set the parameter
;;directly. The workspace-cache manifest keys on it.
(define type-inference-advanced? (make-parameter #f))

(define construct-substitutions-for
  (case-lambda
    [(document)
      (construct-substitutions-for document #f)]
    [(document root-library-node)
      ;;root-library-node reaches the macro type-inference arm (see
      ;;establish-available-rules-from): it locates the workspace's own
      ;;copies of macro libraries for the expansion env bootstrap.
      ;;Drop any fixpoint groups noted by a previous analysis of this
      ;;document first: a refresh re-runs the walk and the accumulator
      ;;table must not accumulate duplicates.
      (fixpoint-clear-document! document)
      (for-each
        (lambda (index-node)
          (step document index-node root-library-node
            (lambda (infer-root-library-node infer-document infer-node)
              (type:expand-and-infer construct-substitutions-for
                infer-root-library-node infer-document infer-node))))
        (document-index-node-list document))
      ;;P1: recursive-return fixpoint pass (guarded internally; a failure
      ;;degrades to the pre-fixpoint status quo, never breaks the caller)
      ;;and P3: dotted-rest element-type inference (same discipline:
      ;;guarded, budgeted, status-quo on failure). Both sit behind the
      ;;type-inference-advanced? gate: off by default, -t advance opts in.
      (when (type-inference-advanced?)
        (fixpoint-substitutions-for! document)
        (rest-elements-substitutions-for! document))]))

(define step
  (case-lambda
    [(current-document current-index-node root-library-node macro-infer)
      (match-index-node current-index-node
        [(and (? index-node-quote? :_) (head . :_))
          (extend-index-node-substitution-list current-index-node head)
          (trivial-process current-document head)]
        ;#'(1 2 3) is a syntax not a list
        [(? index-node-syntax? :_) '()]
        [(and (? index-node-quasiquote? :_) (head . rest))
          (let ([available-identifiers (find-available-references-for current-document current-index-node)])
            (extend-index-node-substitution-list current-index-node head)
            (for-each
              (lambda (current)
                (step current-document current available-identifiers 'quasiquoted root-library-node macro-infer))
              (cons head rest)))]
        [(and (? index-node-quasisyntax? :_) (head . rest))
          (let ([available-identifiers (find-available-references-for current-document current-index-node)])
            (extend-index-node-substitution-list current-index-node head)
            (for-each
              (lambda (current)
                (step current-document current available-identifiers 'quasisyntax root-library-node macro-infer))
              (cons head rest)))]

        [(head . rest)
          (let* ([head-expression (index-node-expression head)]
              [target-rules
                (cond 
                  [(symbol? head-expression)
                    (establish-available-rules-from 
                      (find-available-references-for current-document current-index-node head-expression)
                      root-library-node macro-infer)]
                  [else '()])])
            (if (symbol? head-expression)
              (try 
                (for-each
                  (lambda (current) ((car (cdr current)) current-document current-index-node))
                  target-rules)
                (except c
                  [(condition? c)
                    (append-new-diagnoses current-document
                      `(,(index-node-start current-index-node) ,(index-node-end current-index-node) 2
                        ,(string-append "Type rule warning: " (condition-message c))
                        "type" "type-rule-warning"))
                    '()]
                  [else (raise c)]))
              (application-process current-document current-index-node))
            ;;per-child isolation: a condition in ONE child must not
            ;;starve its later siblings of analysis (measured: the macro-
            ;;expansion virtual tree has nodes whose trivial processing
            ;;raises, and the whole subtree after them silently went
            ;;untyped -- one diagnosis, no types)
            (for-each
              (lambda (child-index-node)
                (try
                  (step current-document child-index-node root-library-node macro-infer)
                  (except c
                    [(condition? c)
                      (append-new-diagnoses current-document
                        `(,(index-node-start child-index-node) ,(index-node-end child-index-node) 2
                          ,(string-append "Type inference warning: " (condition-message c))
                          "type" "type-inference-warning"))
                      '()]
                    [else (raise c)])))
              (cons head rest))
            ;;the cddr half of a rule chain runs AFTER the children walk --
            ;;the same two-phase idiom as the identifier side's
            ;;(do-nothing . begin-process). Today only the fixpoint
            ;;collection hooks use it (possible-fix-point-process:define /
            ;;:named-let); singleton chains have a null cddr and no-op.
            (try
              (for-each
                (lambda (target-rule)
                  (let ([after (cddr target-rule)])
                    (when (procedure? after)
                      (after current-document current-index-node))))
                target-rules)
              (except c
                [(condition? c)
                  (append-new-diagnoses current-document
                    `(,(index-node-start current-index-node) ,(index-node-end current-index-node) 2
                      ,(string-append "Type inference warning: " (condition-message c))
                      "type" "type-inference-warning"))
                  '()]
                [else (raise c)])))]
        [else (trivial-process current-document current-index-node)])]
      [(current-document current-index-node available-identifiers quasi-quoted-syntaxed root-library-node macro-infer)
        (match-index-node current-index-node
          [(and (? (lambda (_) (eq? quasi-quoted-syntaxed 'quasiquoted)) :_)
             (or (? index-node-unquote? :_) (? index-node-unquote-splicing? :_)))
            (for-each
              (lambda (current) (step current-document current root-library-node macro-infer))
              (index-node-children current-index-node))]
          [(and (? (lambda (_) (eq? quasi-quoted-syntaxed 'quasisyntaxed)) :_)
             (or (? index-node-unsyntax? :_) (? index-node-unsyntax-splicing? :_)))
            (for-each
              (lambda (current) (step current-document current root-library-node macro-infer))
              (index-node-children current-index-node))]
          [(head . rest)
            (for-each
              (lambda (current) (step current-document current available-identifiers quasi-quoted-syntaxed root-library-node macro-infer))
              (cons head rest))]
          [else '()])]))

(define (private-rule-compare? item0 item1)
  (apply identifier-compare? (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-rule-compare? origin 
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (establish-available-rules-from identifier-list root-library-node macro-infer)
  (fold-left 
    (lambda (rules identifier)
      (let* ([top (root-ancestor identifier)]
          [r (map identifier-reference-identifier top)]
          [i (identifier-reference-identifier identifier)]
          [is (map identifier-reference-library-identifier top)]
          [type (map identifier-reference-type top)])
        (if (or (equal? '(procedure) type) (equal? '(variable) type) (equal? '(getter) type) (equal? '(setter) type) (equal? '(predicator) type) (equal? '(constructor) type)) 
          (private-add-rule rules `((,application-process) . ,identifier))
          (if (find meta? top)
            (cond 
              [(equal? r '(define)) (private-add-rule rules `((,define-process . ,possible-fix-point-process:define) . ,identifier))]
              [(equal? r '(define-record-type)) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
              [(equal? r '(define-top-level-value)) (private-add-rule rules `((,define-process) . ,identifier))]
              [(equal? r '(do)) (private-add-rule rules `((,do-process) . ,identifier))]

              [(equal? r '(if)) (private-add-rule rules `((,if-process) . ,identifier))]
              [(equal? r '(cond)) (private-add-rule rules `((,cond-process) . ,identifier))]
              [(equal? r '(unless)) (private-add-rule rules `((,unless-process) . ,identifier))]

              [(equal? r '(case-lambda)) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
              [(equal? r '(lambda)) (private-add-rule rules `((,lambda-process) . ,identifier))]

              [(equal? r '(let)) (private-add-rule rules `((,let-process . ,possible-fix-point-process:named-let) . ,identifier))]
              [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
              [(equal? r '(letrec)) (private-add-rule rules `((,let*-process) . ,identifier))]
              [(equal? r '(letrec*)) (private-add-rule rules `((,let*-process) . ,identifier))]

              [(equal? r '(begin)) (private-add-rule rules `((,begin-process) . ,identifier))]

              [else (private-add-rule rules `((,do-nothing) . ,identifier))])
            (route&add rules identifier root-library-node macro-infer private-add-rule)))))
    '()
    (filter 
      (lambda (identifier) 
        (not 
          (or 
            (eq? 'parameter (identifier-reference-type identifier))
            (eq? 'syntax-parameter (identifier-reference-type identifier))
            ;speculative references (IR-synthesized macro-derived bindings,
            ;e.g. match pattern variables) never denote procedures or
            ;special forms -- in head position they sit inside macro
            ;patterns, which must not be typed as applications
            (identifier-reference-speculative? identifier))))
      identifier-list)))
)
