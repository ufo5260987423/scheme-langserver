(library (scheme-langserver analysis abstract-interpreter)
  (export step)
  (import
    (chezscheme)

    (ufo-try)
    (ufo-match-steer)

    (scheme-langserver analysis util)


    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier rules define-syntax)
    (scheme-langserver analysis identifier rules define-record-type)
    (scheme-langserver analysis identifier rules define-top-level-syntax)

    (scheme-langserver analysis identifier rules do)

    (scheme-langserver analysis identifier rules lambda)
    (scheme-langserver analysis identifier rules case-lambda)

    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules let*)
    (scheme-langserver analysis identifier rules let-syntax)
    (scheme-langserver analysis identifier rules letrec)
    (scheme-langserver analysis identifier rules letrec*)
    (scheme-langserver analysis identifier rules let-values)
    (scheme-langserver analysis identifier rules let*-values)
    (scheme-langserver analysis identifier rules letrec-syntax)

    (scheme-langserver analysis identifier rules load)

    (scheme-langserver analysis identifier rules fluid-let)
    (scheme-langserver analysis identifier rules fluid-let-syntax)

    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier rules library-export)
    (scheme-langserver analysis identifier rules library-import)

    (scheme-langserver analysis identifier rules syntax-case)
    (scheme-langserver analysis identifier rules syntax-rules)
    (scheme-langserver analysis identifier rules with-syntax)
    (scheme-langserver analysis identifier rules identifier-syntax)

    (scheme-langserver analysis identifier rules r7rs define)
    (scheme-langserver analysis identifier rules r7rs define-library-import)

    (scheme-langserver analysis identifier rules s7 define-macro)
    (scheme-langserver analysis identifier rules s7 define*)
    (scheme-langserver analysis identifier rules s7 lambda*)

    (scheme-langserver analysis identifier self-defined-rules router)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

;TODO: in case of self-defined macro's partially evaluation leading endless recursions, add a recursion avoid mechanism.
(define step
  (case-lambda
    [(root-file-node root-library-node file-linkage current-document)
      (for-each
        (lambda (current-index-node)
          (step root-file-node root-library-node file-linkage current-document current-index-node))
        (document-index-node-list current-document))
      (document-ordered-reference-list current-document)]
    [(root-file-node root-library-node file-linkage current-document current-index-node)
      (match-index-node current-index-node
        [('quote . :_)
          (index-node-excluded-references-set! current-index-node
            (find-available-references-for current-document current-index-node))]
        [('quasiquote . :_)
          (let ([available-identifiers (find-available-references-for current-document current-index-node)])
            (index-node-excluded-references-set! current-index-node available-identifiers)
            (for-each
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i available-identifiers 'quasiquoted))
              (index-node-children current-index-node)))]
        [('syntax . :_)
          (index-node-excluded-references-set! current-index-node
            (filter
              (lambda (i) (not (eq? (identifier-reference-type i) 'syntax-parameter)))
              (find-available-references-for current-document current-index-node)))]
        [('quasisyntax . :_)
          (let ([available-identifiers (find-available-references-for current-document current-index-node)])
            (index-node-excluded-references-set! current-index-node
              (filter
                (lambda (i) (not (eq? (identifier-reference-type i) 'syntax-parameter)))
                available-identifiers))
            (for-each
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i available-identifiers 'quasisyntaxed))
              (index-node-children current-index-node)))]
        [(children **1)
          (let* ([head (car children)]
              [head-expression (index-node-expression head)]
              [target-rules
                (if (index-node-symbol? head)
                  (establish-available-rules-from
                    file-linkage
                    (find-available-references-for current-document current-index-node head-expression)
                    current-document)
                  '())])
            (try
              (for-each (lambda (f) ((cadr f) root-file-node root-library-node current-document current-index-node)) target-rules)
              (except c
                [else
                  (append-new-diagnoses current-document `(,(index-node-start current-index-node) ,(index-node-end current-index-node) 2 "Scheme-langserver Warning: fail to catch identifiers" "identifier" "identifier-resolution-failure"))]))
            (for-each
              (lambda (child-index-node)
                (step root-file-node root-library-node file-linkage current-document child-index-node))
              children)
            (try
              (for-each
                (lambda (f)
                  (if (not (null? (cddr f)))
                    ((cddr f) root-file-node root-library-node current-document current-index-node)))
                target-rules)
              (except c
                [else
                  (append-new-diagnoses current-document `(,(index-node-start current-index-node) ,(index-node-end current-index-node) 2 "Scheme-langserver Warning: fail to catch identifiers" "identifier" "identifier-resolution-failure"))])))]
        [(? index-node-symbol?)
          (let ([refs (find-available-references-for current-document current-index-node
                        (annotation-stripped (index-node-datum/annotations current-index-node)))])
            (for-each
              (lambda (ref)
                (identifier-reference-usage-count-set! ref (+ 1 (identifier-reference-usage-count ref))))
              refs)
            refs)]
        [else '()])]
      [(root-file-node root-library-node file-linkage current-document current-index-node available-identifiers quasi-quoted-syntaxed)
        (if (case quasi-quoted-syntaxed
            ['quasiquoted  (or (unquote? current-index-node) (unquote-splicing? current-index-node))]
            ['quasisyntaxed (or (unsyntax? current-index-node) (unsyntax-splicing? current-index-node))])
          (begin
            ;;(debug:print-expression current-index-node) (pretty-print (null? (index-node-children current-index-node)))
            ;;(debug:print-expression (index-node-parent current-index-node))
            (index-node-references-import-in-this-node-set! current-index-node (sort-identifier-references available-identifiers))
            (map
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i))
              (index-node-children current-index-node)))
          (map
            (lambda (i)
              (step root-file-node root-library-node file-linkage current-document i available-identifiers quasi-quoted-syntaxed))
            (index-node-children current-index-node)))]))

(define (private-rule-compare? item0 item1)
  (apply identifier-compare? (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-rule-compare? origin
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (establish-available-rules-from file-linkage identifier-list current-document)
  (fold-left
    (lambda (rules identifier)
      (let* ([top (root-ancestor identifier)]
          [r (map identifier-reference-identifier top)])
        (if (find meta? top)
          (cond
            [(and (equal? r '(define)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,define-process) . ,identifier))]
            [(and (equal? r '(define)) (or (private:top-env=? 'r7rs top) (private:top-env=? 's7 top)))
              (private-add-rule rules `((,define-r7rs-process) . ,identifier))]
            [(equal? r '(define-syntax)) (private-add-rule rules `((,define-syntax-process) . ,identifier))]
            [(equal? r '(define-record-type)) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
            [(equal? r '(do)) (private-add-rule rules `((,do-process) . ,identifier))]
            [(equal? r '(case-lambda)) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
            [(equal? r '(lambda)) (private-add-rule rules `((,lambda-process) . ,identifier))]

            [(equal? r '(set!)) (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]
            [(and (equal? r '(set-top-level-value!)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]
            [(and (equal? r '(define-top-level-value)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]

            [(and (equal? r '(set-top-level-syntax!)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,define-top-level-syntax-process) . ,identifier))]
            [(and (equal? r '(define-top-level-syntax)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,define-top-level-syntax-process) . ,identifier))]

            [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
            [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
            [(equal? r '(let-values)) (private-add-rule rules `((,let-values-process) . ,identifier))]
            [(equal? r '(let*-values)) (private-add-rule rules `((,let*-values-process) . ,identifier))]
            [(equal? r '(let-syntax)) (private-add-rule rules `((,let-syntax-process) . ,identifier))]
            [(equal? r '(letrec)) (private-add-rule rules `((,letrec-process) . ,identifier))]
            [(equal? r '(letrec*)) (private-add-rule rules `((,letrec*-process) . ,identifier))]
            [(equal? r '(letrec-syntax)) (private-add-rule rules `((,letrec-syntax-process) . ,identifier))]
            [(and (equal? r '(fluid-let)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,fluid-let-process) . ,identifier))]
            [(and (equal? r '(fluid-let-syntax)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,fluid-let-syntax-process) . ,identifier))]

            [(and (equal? r '(syntax-case)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,syntax-case-process) . ,identifier))]
            [(equal? r '(syntax-rules)) (private-add-rule rules `((,syntax-rules-process) . ,identifier))]
            [(and (equal? r '(identifier-syntax)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,identifier-syntax-process) . ,identifier))]
            [(and (equal? r '(with-syntax)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,with-syntax-process) . ,identifier))]

            [(and (equal? r '(library)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,library-import-process . ,export-process) . ,identifier))]
            [(and (equal? r '(invoke-library)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,invoke-library-process) . ,identifier))]
            [(and (equal? r '(import)) (private:top-env=? 'r6rs top))
              (private-add-rule rules `((,import-process) . ,identifier))]
            [(and (equal? r '(import)) (or (private:top-env=? 'r7rs top) (private:top-env=? 's7 top)))
              (private-add-rule rules `((,r7-import-process) . ,identifier))]

            [(or (equal? r '(load)) (equal? r '(load-program)) (equal? r '(load-library)))
              (private-add-rule rules `((,load-process) . ,identifier))]

            [(equal? r '(begin)) (private-add-rule rules `((,do-nothing . ,begin-process) . ,identifier))]

            [(and (equal? r '(define-library)) (or (private:top-env=? 'r7rs top) (private:top-env=? 's7 top)))
              (private-add-rule rules `((,library-import-process-r7rs . ,export-process-r7rs) . ,identifier))]

            [(and (equal? r '(define*)) (private:top-env=? 's7 top))
              (private-add-rule rules `((,define*-process) . ,identifier))]

            [(and (equal? r '(define-macro)) (private:top-env=? 's7 top))
              (private-add-rule rules `((,define-macro-process) . ,identifier))]

            [(and (equal? r '(lambda*)) (private:top-env=? 's7 top))
              (private-add-rule rules `((,lambda*-process) . ,identifier))]

            [else rules])
          (route&add
            rules identifier
            file-linkage identifier-list current-document
            private-add-rule step))))
    '()
    (filter
      (lambda (identifier)
        (not
          (or
            ; (equal? 'syntax-variable (identifier-reference-type identifier))
            (eq? 'parameter (identifier-reference-type identifier))
            (eq? 'syntax-parameter (identifier-reference-type identifier))
            (eq? 'procedure (identifier-reference-type identifier))
            (eq? 'variable (identifier-reference-type identifier)))))
      identifier-list)))

(define (private:top-env=? standard top)
  (find (lambda (item) (eq? standard (identifier-reference-top-environment item))) top))
)