(library (scheme-langserver analysis abstract-interpreter)
  (export step)
  (import 
    (chezscheme) 

    (ufo-try)
    (scheme-langserver util path)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)
    (scheme-langserver util binary-search)

    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis identifier primitive-variable)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier rules define-syntax)
    (scheme-langserver analysis identifier rules define-record-type)
    (scheme-langserver analysis identifier rules define-top-level-value)
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
    (scheme-langserver analysis identifier rules load-library)
    (scheme-langserver analysis identifier rules load-program)

    (scheme-langserver analysis identifier rules fluid-let)
    (scheme-langserver analysis identifier rules fluid-let-syntax)

    (scheme-langserver analysis identifier rules body)

    (scheme-langserver analysis identifier rules library-export)
    (scheme-langserver analysis identifier rules library-import)

    (scheme-langserver analysis identifier rules syntax-case)
    (scheme-langserver analysis identifier rules syntax-rules)
    (scheme-langserver analysis identifier rules self-defined-syntax)
    (scheme-langserver analysis identifier rules with-syntax)
    (scheme-langserver analysis identifier rules identifier-syntax)

    (scheme-langserver analysis identifier rules r7rs define-r7rs)

    (scheme-langserver analysis identifier self-defined-rules router)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

;TODO: in case of self-defined macro's partially evaluation leading endless recursions, add a recursion avoid mechanism. 
(define step 
  (case-lambda 
    [(root-file-node root-library-node file-linkage current-document)
      (step root-file-node root-library-node file-linkage current-document '())]
    [(root-file-node root-library-node file-linkage current-document expanded+callee-list)
      (fold-left
        (lambda (l current-index-node)
          (step root-file-node root-library-node file-linkage current-document current-index-node '() '()))
        '() 
        (document-index-node-list current-document))
      (document-ordered-reference-list current-document)]
    [(root-file-node root-library-node file-linkage current-document expanded+callee-list memory)
      (fold-left
        (lambda (l current-index-node)
          (step root-file-node root-library-node file-linkage current-document current-index-node '() memory))
        '() 
        (document-index-node-list current-document))
      (document-ordered-reference-list current-document)]
    [(root-file-node root-library-node file-linkage current-document current-index-node expanded+callee-list memory)
      (cond 
        [(quote? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node (private:find-available-references-for expanded+callee-list current-document current-index-node))]
        [(quasiquote? current-index-node current-document) 
          (let ([available-identifiers (private:find-available-references-for expanded+callee-list current-document current-index-node)])
            (index-node-excluded-references-set! current-index-node available-identifiers)
            (map 
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i available-identifiers 'quasiquoted expanded+callee-list memory))
              (index-node-children current-index-node)))]
        [(syntax? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node 
            (filter (lambda (i) (not (equal? (identifier-reference-type i) 'syntax-parameter))) (private:find-available-references-for expanded+callee-list current-document current-index-node)))]
        [(quasisyntax? current-index-node current-document)
          (let ([available-identifiers (private:find-available-references-for expanded+callee-list current-document current-index-node)])
            (index-node-excluded-references-set! current-index-node (filter (lambda (i) (not (equal? (identifier-reference-type i) 'syntax-parameter))) available-identifiers))
            (map 
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i available-identifiers 'quasisyntaxed expanded+callee-list memory))
              (index-node-children current-index-node)))]
        [(not (null? (index-node-children current-index-node))) 
          (let* ([children (index-node-children current-index-node)]
              [head (car children)]
              [head-expression (annotation-stripped (index-node-datum/annotations head))]
              [target-rules
                (cond 
                  [(symbol? head-expression)
                    (establish-available-rules-from 
                      file-linkage
                      (private:find-available-references-for expanded+callee-list current-document current-index-node head-expression)
                      current-document
                      expanded+callee-list 
                      `(,memory (,(annotation-stripped (index-node-datum/annotations current-index-node)))))]
                  [else '()])])
            (map (lambda (f) ((car (cdr f)) root-file-node root-library-node current-document current-index-node)) target-rules)
            (fold-left
              (lambda (l child-index-node)
                (step root-file-node root-library-node file-linkage current-document child-index-node expanded+callee-list memory))
              '()
              children)
            (map 
              (lambda (f) 
                (if (not (null? (cdr (cdr f))))
                  ((cdr (cdr f)) root-file-node root-library-node current-document current-index-node))) 
              target-rules))]
        [else '()])]
      [(root-file-node root-library-node file-linkage current-document current-index-node available-identifiers quasi-quoted-syntaxed expanded+callee-list memory)
        (if (case quasi-quoted-syntaxed
            ['quasiquoted  (or (unquote? current-index-node current-document) (unquote-splicing? current-index-node current-document))]
            ['quasisyntaxed (or (unsyntax? current-index-node current-document) (unsyntax-splicing? current-index-node current-document))])
          (begin
            ;; (debug:print-expression current-index-node)
            ;; (pretty-print (null? (index-node-children current-index-node)))
            ;; (debug:print-expression (index-node-parent current-index-node))
            (index-node-references-import-in-this-node-set! current-index-node (sort-identifier-references available-identifiers))
            (map 
              (lambda (i)
                (step root-file-node root-library-node file-linkage current-document i expanded+callee-list memory))
              (index-node-children current-index-node)))
          (map
            (lambda (i)
              (step root-file-node root-library-node file-linkage current-document i available-identifiers quasi-quoted-syntaxed expanded+callee-list memory))
            (index-node-children current-index-node)))]))

(define (private-rule-compare? item0 item1)
  (apply identifier-compare? (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-rule-compare? origin 
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (establish-available-rules-from file-linkage identifier-list current-document expanded+callee-list memory)
  (fold-left 
    (lambda (rules identifier)
      (let* ([top (root-ancestor identifier)]
          [r (map identifier-reference-identifier top)]
          [i (identifier-reference-identifier identifier)]
          [is (map identifier-reference-library-identifier top)])
        (if (find meta-library? is)
          (cond 
            [(and (equal? r '(define)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,define-process) . ,identifier))]
            [(and (equal? r '(define)) (find-top-env? 'r7rs top))
              (private-add-rule rules `((,define-r7rs-process) . ,identifier))]
            [(equal? r '(define-syntax)) (private-add-rule rules `((,define-syntax-process) . ,identifier))]
            [(equal? r '(define-record-type)) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
            [(equal? r '(do)) (private-add-rule rules `((,do-process) . ,identifier))]
            [(equal? r '(case-lambda)) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
            [(equal? r '(lambda)) (private-add-rule rules `((,lambda-process) . ,identifier))]

            [(equal? r '(set!)) (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]
            [(and (equal? r '(set-top-level-value!)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]
            [(and (equal? r '(define-top-level-value)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,define-top-level-value-process) . ,identifier))]

            [(and (equal? r '(set-top-level-syntax!)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,define-top-level-syntax-process) . ,identifier))]
            [(and (equal? r '(define-top-level-syntax)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,define-top-level-syntax-process) . ,identifier))]

            [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
            [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
            [(equal? r '(let-values)) (private-add-rule rules `((,let-values-process) . ,identifier))]
            [(equal? r '(let*-values)) (private-add-rule rules `((,let*-values-process) . ,identifier))]
            [(equal? r '(let-syntax)) (private-add-rule rules `((,let-syntax-process) . ,identifier))]
            [(equal? r '(letrec)) (private-add-rule rules `((,letrec-process) . ,identifier))]
            [(equal? r '(letrec*)) (private-add-rule rules `((,letrec*-process) . ,identifier))]
            [(equal? r '(letrec-syntax)) (private-add-rule rules `((,letrec-syntax-process) . ,identifier))]
            [(and (equal? r '(fluid-let)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,fluid-let-process) . ,identifier))]
            [(and (equal? r '(fluid-let-syntax)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,fluid-let-syntax-process) . ,identifier))]

            [(and (equal? r '(syntax-case)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,syntax-case-process) . ,identifier))]
            [(equal? r '(syntax-rules)) (private-add-rule rules `((,syntax-rules-process) . ,identifier))]
            [(and (equal? r '(identifier-syntax)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,identifier-syntax-process) . ,identifier))]
            [(and (equal? r '(with-syntax)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,with-syntax-process) . ,identifier))]

            [(and (equal? r '(library)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,library-import-process . ,export-process) . ,identifier))]
            [(and (equal? r '(invoke-library)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,invoke-library-process) . ,identifier))]
            [(equal? r '(import)) 
              (let ([special 
                    (lambda (root-file-node root-library-node document index-node)
                      (if (null? (index-node-parent index-node))
                        (import-process root-file-node root-library-node document index-node)
                        (let* ([t (car (index-node-children (index-node-parent index-node)))]
                            [t-e (annotation-stripped (index-node-datum/annotations t))])
                          (cond 
                            [(not (symbol? t-e))
                              (import-process root-file-node root-library-node document index-node)]
                            [(and (null? (index-node-parent t)) (equal? t-e 'library)) 
                              (do-nothing root-file-node root-library-node document index-node)]
                            [(null? (index-node-parent t)) 
                              (import-process root-file-node root-library-node document index-node)]
                            [(find 
                              (lambda (ss)
                                (and (equal? (identifier-reference-identifier ss) 'library)
                                  (contain? '((chezscheme) (rnrs) (rnrs (6)) (scheme) (rnrs base)) (identifier-reference-library-identifier ss)))) 
                              (apply append (map root-ancestor (find-available-references-for current-document (index-node-parent t) t-e))))
                              (do-nothing root-file-node root-library-node document index-node)]
                            [else (import-process root-file-node root-library-node document index-node)]))))])
                (private-add-rule rules `((,special) . ,identifier)))]

            [(equal? r '(load)) (private-add-rule rules `((,load-process) . ,identifier))]
            [(and (equal? r '(load-program)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,load-program-process) . ,identifier))]
            [(and (equal? r '(load-library)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,load-library-process) . ,identifier))]

            [(and (equal? r '(body)) (find-top-env? 'r6rs top))
              (private-add-rule rules `((,do-nothing . ,body-process) . ,identifier))]

            [else rules])
          (route&add 
            rules identifier 
            file-linkage identifier-list current-document expanded+callee-list memory
            private-add-rule step))))
    '()
    (filter 
      (lambda (identifier) 
        (not 
          (or 
            (equal? 'parameter (identifier-reference-type identifier))
            (equal? 'syntax-parameter (identifier-reference-type identifier))
            (equal? 'procedure (identifier-reference-type identifier))
            (equal? 'variable (identifier-reference-type identifier)))))
      identifier-list)))

(define private:find-available-references-for 
  (case-lambda 
    [(expanded+callee-list current-document current-index-node)
      (let ([result (assoc current-index-node expanded+callee-list)])
        (if result 
          (private:find-available-references-for expanded+callee-list current-document (cdr result))
          (find-available-references-for current-document current-index-node)))]
    [(expanded+callee-list current-document current-index-node expression)
      (let ([result (assoc current-index-node expanded+callee-list)])
        (if result 
          (private:find-available-references-for expanded+callee-list current-document (cdr result) expression)
          (find-available-references-for current-document current-index-node expression)))]))

(define (find-top-env? standard top)
  (find (lambda (top-environment) (equal? standard top-environment))
    (map identifier-reference-top-environment top)))

)