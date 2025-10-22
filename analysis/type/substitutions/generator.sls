(library (scheme-langserver analysis type substitutions generator)
  (export 
    construct-substitutions-for)
  (import 
    (chezscheme)
    (ufo-try)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type substitutions rules if)
    (scheme-langserver analysis type substitutions rules cond)
    (scheme-langserver analysis type substitutions rules case)
    (scheme-langserver analysis type substitutions rules do)
    (scheme-langserver analysis type substitutions rules let)
    (scheme-langserver analysis type substitutions rules let*)
    (scheme-langserver analysis type substitutions rules letrec)
    (scheme-langserver analysis type substitutions rules lambda)
    (scheme-langserver analysis type substitutions rules case-lambda)
    (scheme-langserver analysis type substitutions rules record)
    (scheme-langserver analysis type substitutions rules trivial)
    (scheme-langserver analysis type substitutions rules define)
    (scheme-langserver analysis type substitutions rules application)
    (scheme-langserver analysis type substitutions rules begin)
    (scheme-langserver analysis type substitutions util)

    (scheme-langserver analysis type substitutions self-defined-rules router))

(define (construct-substitutions-for document)
  (map
    (lambda (index-node) (step document index-node '()))
    (document-index-node-list document)))

; document index-node allow-unquote? quoted?
(define step 
  (case-lambda 
    [(current-document current-index-node expanded+callee-list)
      (cond 
        [(null? (index-node-children current-index-node))
          (trivial-process current-document current-index-node)]

        [(quote? current-index-node current-document) 
          (extend-index-node-substitution-list 
            current-index-node 
            (car (index-node-children current-index-node)))
          (trivial-process current-document (car (index-node-children current-index-node)))]
        ;#'(1 2 3) is a syntax not a list
        [(syntax? current-index-node current-document) '()]
        [(quasiquote? current-index-node current-document) 
          (extend-index-node-substitution-list 
            current-index-node 
            (car (index-node-children current-index-node)))
          (let ([available-identifiers (private:find-available-references-for expanded+callee-list current-document current-index-node)])
            (map
              (lambda (current)
                (step current-document current available-identifiers 'quasiquoted expanded+callee-list))
              (index-node-children current-index-node)))]
        [(quasisyntax? current-index-node current-document)
          (extend-index-node-substitution-list 
            current-index-node 
            (car (index-node-children current-index-node)))
          (let ([available-identifiers (private:find-available-references-for expanded+callee-list current-document current-index-node)])
            (map
              (lambda (current)
                (step current-document current available-identifiers 'quasisyntax expanded+callee-list))
              (index-node-children current-index-node)))]

        [(not (null? (index-node-children current-index-node))) 
          (let* ([children (index-node-children current-index-node)]
              [head (car children)]
              [head-expression (annotation-stripped (index-node-datum/annotations head))]
              [target-rules
                (cond 
                  [(symbol? head-expression)
                    (establish-available-rules-from 
                      (private:find-available-references-for expanded+callee-list current-document current-index-node head-expression)
                      current-document
                      expanded+callee-list)]
                  [else '()])])
            (if (symbol? head-expression)
              (try 
                (map
                  (lambda (current) ((car (cdr current)) current-document current-index-node))
                  target-rules)
                (except c
                  [else '()]))
              ;this must be grounded, generally you shouldn't test this.
              (application-process current-document current-index-node))
            (try 
              (map
                (lambda (child-index-node) (step current-document child-index-node expanded+callee-list))
                children)
              (except c 
                [else '()])))])]
      [(current-document current-index-node available-identifiers quasi-quoted-syntaxed expanded+callee-list)
        (if (case quasi-quoted-syntaxed
            ['quasiquoted  (or (unquote? current-index-node current-document) (unquote-splicing? current-index-node current-document))]
            ['quasisyntaxed (or (unsyntax? current-index-node current-document) (unsyntax-splicing? current-index-node current-document))]
            [else #f])
          (map
            (lambda (current) (step current-document current expanded+callee-list))
            (index-node-children current-index-node))
          (map
            (lambda (current) (step current-document current available-identifiers quasi-quoted-syntaxed expanded+callee-list))
            (index-node-children current-index-node)))]))

(define (private-rule-compare? item0 item1)
  (apply identifier-compare? (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-rule-compare? origin 
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (establish-available-rules-from identifier-list current-document expanded+callee-list)
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
              [(equal? r '(define)) (private-add-rule rules `((,define-process) . ,identifier))]
              [(equal? r '(define-record-type)) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
              [(equal? r '(define-top-level-value)) (private-add-rule rules `((,define-process) . ,identifier))]
              [(equal? r '(do)) (private-add-rule rules `((,do-process) . ,identifier))]

              [(equal? r '(if)) (private-add-rule rules `((,if-process) . ,identifier))]
              [(equal? r '(cond)) (private-add-rule rules `((,cond-process) . ,identifier))]
              [(equal? r '(unless)) (private-add-rule rules `((,begin-process) . ,identifier))]

              [(equal? r '(case-lambda)) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
              [(equal? r '(lambda)) (private-add-rule rules `((,lambda-process) . ,identifier))]

              [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
              [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
              [(equal? r '(letrec)) (private-add-rule rules `((,letrec-process) . ,identifier))]
              [(equal? r '(letrec*)) (private-add-rule rules `((,let*-process) . ,identifier))]

              [(equal? r '(begin)) (private-add-rule rules `((,begin-process) . ,identifier))]

              [else (private-add-rule rules `((,do-nothing) . ,identifier))])
            (route&add rules identifier private-add-rule)))))
    '()
    (filter 
      (lambda (identifier) 
        (not 
          (or 
            (equal? 'parameter (identifier-reference-type identifier))
            (equal? 'syntax-parameter (identifier-reference-type identifier)))))
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
)