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
          (step root-file-node root-library-node current-document current-index-node 
            (update-available-rules initial-available-rules (document-reference-list current-document)) 
            (document-reference-list current-document)))
        '() 
        (document-index-node-list current-document))
      (document-reference-list current-document)]
    [(root-file-node root-library-node current-document current-index-node available-rules available-identifiers)
      (cond 
        [(quote? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node available-identifiers)]
        [(syntax? current-index-node current-document) (index-node-excluded-references-set! current-index-node available-identifiers)]
        [(quasiquote? current-index-node current-document) 
          (index-node-excluded-references-set! current-index-node available-identifiers)
          (map 
            (lambda (i)
              (step root-file-node root-library-node current-document i available-rules available-identifiers #t))
            (index-node-children current-index-node))]
        ; [(quaisisyntax? current-index-node current-document)]
        [(not (null? (index-node-children current-index-node))) 
          (let* ([children (index-node-children current-index-node)]
              [head (car children)]
              [head-expression (annotation-stripped (index-node-datum/annotations head))]
              [pre-current-available-identifiers
                (if (null? (index-node-parent current-index-node))
                  available-identifiers
                  (private-extend available-identifiers (index-node-parent current-index-node)))]
              [current-available-identifiers (private-extend pre-current-available-identifiers current-index-node)]
              [current-available-rules (update-available-rules available-rules current-available-identifiers)]
              [target-rules 
                (filter 
                  (lambda (rule) (equal? head-expression (identifier-reference-identifier (car rule)))) 
                  current-available-rules)])
            (map (lambda (f) ((car (cdr f)) root-file-node root-library-node current-document current-index-node)) target-rules)
            (fold-left
              (lambda (l child-index-node)
                (step root-file-node root-library-node current-document child-index-node current-available-rules current-available-identifiers))
              '()
              children)
            (map (lambda (f) 
              (if (not (null? (cdr (cdr f))))
                ((cdr (cdr f)) root-file-node root-library-node current-document current-index-node))) target-rules))]
        [else '()])]
      [(root-file-node root-library-node current-document current-index-node available-rules available-identifiers quoted?)
        (if (or 
            (unquote? current-index-node current-document)
            (unsyntax? current-index-node current-document)
            (unquote-splicing? current-index-node current-document)
            (unsyntax-splicing? current-index-node current-document))
          (begin
            (index-node-references-import-in-this-node-set! current-index-node available-identifiers)
            (map 
              (lambda (i)
                (step root-file-node root-library-node current-document i available-rules available-identifiers))
              (index-node-children current-index-node)))
          (map 
            (lambda (i)
              (step root-file-node root-library-node current-document i available-rules available-identifiers #t))
            (index-node-children current-index-node)))]))

(define (private-extend origin index-node)
  (let* ([import (index-node-references-import-in-this-node index-node)]
      [import-identifiers (map identifier-reference-identifier import)]
      [exclude (index-node-excluded-references index-node)])
    (if (and (null? import) (null? exclude))
      origin
      (filter 
        (lambda (i)
          (not (contain? exclude i)))
        (append 
          (filter 
            (lambda (i) (not (contain? import-identifiers (identifier-reference-identifier i))))
            origin)
          import)))))
 
(define (private-compare item0 item1)
  (sort-identifier-references (map car (list item0 item1))))

(define (private-add-rule origin target-rule)
  (merge private-compare origin 
    `((,(cdr target-rule) . ,(car target-rule)))))

(define (private-remove-rule origin target-identifier-reference)
  (filter 
    (lambda (rule)
      (not (equal? (car origin) target-identifier-reference)))
    origin))

(define (update-available-rules origin identifier-list)
  (let* ([filtered-origin-rules (filter (lambda (r) (contain? identifier-list (car r))) origin)]
      [filtered-origin-identifiers (map car filtered-origin-rules)]
      [filtered-new (filter (lambda (i) (not (contain? filtered-origin-identifiers i))) identifier-list)])
    (merge private-compare filtered-origin-rules (establish-available-rules-from filtered-new))))

(define (establish-available-rules-from identifier-list)
  (fold-left 
    (lambda (rules identifier)
      (let* ([top (root-ancestor identifier)]
          [r (map identifier-reference-identifier top)]
          [i (identifier-reference-identifier identifier)]
          [is (map identifier-reference-library-identifier top)])
        (if (or 
            (contain? is '(chezscheme))
            (contain? is '(rnrs))
            (contain? is '(rnrs (6)))
            (contain? is '(rnrs base))
            (contain? is '(scheme)))
          (cond 
            [(contain? r 'define) (private-add-rule rules `((,define-process) . ,identifier))]
            [(contain? r 'define-syntax) (private-add-rule rules `((,define-syntax-process) . ,identifier))]
            [(contain? r 'define-record-type) (private-add-rule rules `((,define-record-type-process) . ,identifier))]
            [(contain? r 'do) (private-add-rule rules `((,do-process) . ,identifier))]
            [(contain? r 'case-lambda) (private-add-rule rules `((,case-lambda-process) . ,identifier))]
            [(contain? r 'lambda) (private-add-rule rules `((,lambda-process) . ,identifier))]

            [(contain? r 'let) (private-add-rule rules `((,let-process) . ,identifier))]
            [(contain? r 'let*) (private-add-rule rules `((,let*-process) . ,identifier))]
            [(contain? r 'let-values) (private-add-rule rules `((,let-values-process) . ,identifier))]
            [(contain? r 'let*-values) (private-add-rule rules `((,let*-values-process) . ,identifier))]
            [(contain? r 'let-syntax) (private-add-rule rules `((,let-syntax-process) . ,identifier))]
            [(contain? r 'letrec) (private-add-rule rules `((,letrec-process) . ,identifier))]
            [(contain? r 'letrec*) (private-add-rule rules `((,letrec*-process) . ,identifier))]
            [(contain? r 'letrec-syntax) (private-add-rule rules `((,letrec-syntax-process) . ,identifier))]
            [(contain? r 'fluid-let) (private-add-rule rules `((,fluid-let-process) . ,identifier))]
            [(contain? r 'fluid-let-syntax) (private-add-rule rules `((,fluid-let-syntax-process) . ,identifier))]

            [(contain? r 'syntax-case) (private-add-rule rules `((,syntax-case-process) . ,identifier))]
            [(contain? r 'syntax-rules) (private-add-rule rules `((,syntax-rules-process) . ,identifier))]
            [(contain? r 'identifier-syntax) (private-add-rule rules `((,identifier-syntax-process) . ,identifier))]
            [(contain? r 'with-syntax) (private-add-rule rules `((,with-syntax-process) . ,identifier))]

            [(contain? r 'library) (private-add-rule rules `((,library-import-process . ,export-process) . ,identifier))]
            [(contain? r 'import) (private-add-rule rules `((,import-process) . ,identifier))]

            [(contain? r 'load) (private-add-rule rules `((,load-process) . ,identifier))]
            [(contain? r 'load-program) (private-add-rule rules `((,load-program-process) . ,identifier))]
            [(contain? r 'load-library) (private-add-rule rules `((,load-library-process) . ,identifier))]

            [(contain? r 'body) (private-add-rule rules `((,do-nothing ,body-process) . ,identifier))]

            [else rules])
          ;todo: generate rule from syntax-variable
          rules
        )))
    '()
    identifier-list))

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