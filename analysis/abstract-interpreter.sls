(library (scheme-langserver analysis abstract-interpreter)
  (export step)
  (import 
    (chezscheme) 

    (scheme-langserver util contain)
    (scheme-langserver util dedupe)
    (scheme-langserver util binary-search)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis identifier rules define)
    (scheme-langserver analysis identifier rules define-syntax)
    (scheme-langserver analysis identifier rules define-record-type)

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
    (scheme-langserver analysis identifier rules with-syntax)
    (scheme-langserver analysis identifier rules identifier-syntax)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

(define step 
  (case-lambda 
    [(root-file-node root-library-node current-document)
      (fold-left
        (lambda (l current-index-node)
          (step root-file-node root-library-node current-document current-index-node))
        '() 
        (document-index-node-list current-document))
      (document-reference-list current-document)]
    [(root-file-node root-library-node current-document current-index-node)
      (cond 
        [(quote? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node (find-available-references-for current-document current-index-node))]
        [(syntax? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node (find-available-references-for current-document current-index-node))]
        [(quasiquote? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node (find-available-references-for current-document current-index-node))
          (map 
            (lambda (i)
              (step root-file-node root-library-node current-document i (index-node-excluded-references current-index-node)))
            (index-node-children current-index-node))]
        ; [(quaisisyntax? current-index-node current-document)]
        [(not (null? (index-node-children current-index-node))) 
          (let* ([children (index-node-children current-index-node)]
              [head (car children)]
              [head-expression (annotation-stripped (index-node-datum/annotations head))])
            (if (symbol? head-expression)
              (let* ([current-available-identifiers (find-available-references-for current-document current-index-node head-expression)]
                  [target-rules (establish-available-rules-from current-available-identifiers)])
                (map (lambda (f) ((car (cdr f)) root-file-node root-library-node current-document current-index-node)) target-rules)
                (fold-left
                  (lambda (l child-index-node)
                    (step root-file-node root-library-node current-document child-index-node))
                  '()
                  children)
                (map (lambda (f) 
                  (if (not (null? (cdr (cdr f))))
                    ((cdr (cdr f)) root-file-node root-library-node current-document current-index-node))) target-rules))))]
        [else '()])]
      [(root-file-node root-library-node current-document current-index-node available-identifiers)
        (if (or 
            (unquote? current-index-node current-document)
            (unsyntax? current-index-node current-document)
            (unquote-splicing? current-index-node current-document)
            (unsyntax-splicing? current-index-node current-document))
          (begin
            (index-node-references-import-in-this-node-set! current-index-node available-identifiers)
            (map 
              (lambda (i)
                (step root-file-node root-library-node current-document i))
              (index-node-children current-index-node)))
          (map
            (lambda (i)
              (step root-file-node root-library-node current-document i available-identifiers))
            (index-node-children current-index-node)))]))

(define (private-rule-compare? item0 item1)
  (apply identifier-compare? (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-rule-compare? origin 
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (private-remove-rule origin target-identifier-reference)
  (filter 
    (lambda (rule)
      (not (equal? (car origin) target-identifier-reference)))
    origin))

(define (establish-available-rules-from identifier-list)
  (fold-left 
    (lambda (rules identifier)
      (let* ([top (root-ancestor identifier)]
          [r (map identifier-reference-identifier top)]
          [i (identifier-reference-identifier identifier)]
          [is (map identifier-reference-library-identifier top)])
        (if (or 
            (equal? is '((chezscheme)))
            (equal? is '((rnrs)))
            (equal? is '((rnrs (6))))
            (equal? is '((rnrs base)))
            (equal? is '((scheme))))
          (cond 
            [(equal? r '(define)) (private-add-rule rules `((,define-process) . ,identifier))]
            [(equal? r '(define-syntax)) (private-add-rule rules `((,define-syntax-process) . ,identifier))]
            [(equal? r '(define-record-type)) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
            [(equal? r '(do)) (private-add-rule rules `((,do-process) . ,identifier))]
            [(equal? r '(case-lambda) ) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
            [(equal? r '(lambda)) (private-add-rule rules `((,lambda-process) . ,identifier))]

            [(equal? r '(let)) (private-add-rule rules `((,let-process) . ,identifier))]
            [(equal? r '(let*)) (private-add-rule rules `((,let*-process) . ,identifier))]
            [(equal? r '(let-values)) (private-add-rule rules `((,let-values-process) . ,identifier))]
            [(equal? r '(let*-values)) (private-add-rule rules `((,let*-values-process) . ,identifier))]
            [(equal? r '(let-syntax)) (private-add-rule rules `((,let-syntax-process) . ,identifier))]
            [(equal? r '(letrec)) (private-add-rule rules `((,letrec-process) . ,identifier))]
            [(equal? r '(letrec*)) (private-add-rule rules `((,letrec*-process) . ,identifier))]
            [(equal? r '(letrec-syntax)) (private-add-rule rules `((,letrec-syntax-process) . ,identifier))]
            [(equal? r '(fluid-let)) (private-add-rule rules `((,fluid-let-process) . ,identifier))]
            [(equal? r '(fluid-let-syntax)) (private-add-rule rules `((,fluid-let-syntax-process) . ,identifier))]

            [(equal? r '(syntax-case)) (private-add-rule rules `((,syntax-case-process) . ,identifier))]
            [(equal? r '(syntax-rules)) (private-add-rule rules `((,syntax-rules-process) . ,identifier))]
            [(equal? r '(identifier-syntax)) (private-add-rule rules `((,identifier-syntax-process) . ,identifier))]
            [(equal? r '(with-syntax)) (private-add-rule rules `((,with-syntax-process) . ,identifier))]

            [(equal? r '(library)) (private-add-rule rules `((,library-import-process . ,export-process) . ,identifier))]
            [(equal? r '(import)) (private-add-rule rules `((,import-process) . ,identifier))]

            [(equal? r '(load)) (private-add-rule rules `((,load-process) . ,identifier))]
            [(equal? r '(load-program)) (private-add-rule rules `((,load-program-process) . ,identifier))]
            [(equal? r '(load-library)) (private-add-rule rules `((,load-library-process) . ,identifier))]

            [(equal? r '(body)) (private-add-rule rules `((,do-nothing ,body-process) . ,identifier))]

            [else rules])
          ;todo: generate rule from syntax-variable
          rules
        )))
    '()
    (filter 
      (lambda (identifier) 
        (not 
          (or 
            (equal? 'parameter (identifier-reference-type identifier))
            (equal? 'syntax-parameter (identifier-reference-type identifier))
            (equal? 'procedure (identifier-reference-type identifier)))))
      identifier-list)))

(define initial-available-rules 
  (let ([l '(define define-syntax define-record-type do 
      case-lambda lambda 
      let let* let-values let*-values let-syntax letrec letrec* letrec-syntax fluid-let fluid-let* fluid-let-syntax
      syntax-case syntax-rules identifier-syntax with-syntax 
      library load load-program load-library import)])
    (establish-available-rules-from
      (filter 
        (lambda (i)
          (contain? l (identifier-reference-identifier i)))
        (find-meta '(chezscheme))))))
)