(library (scheme-langserver analysis identifier self-defined-rules router)
  (export route&add)
  (import 
    (rnrs)
    (scheme-langserver util path)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules begin)

    (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
    (scheme-langserver analysis identifier self-defined-rules ufo-match match)
    (scheme-langserver analysis identifier self-defined-rules ufo-try try)
    
    (scheme-langserver analysis identifier self-defined-rules goldfish define-case-class)
    (scheme-langserver analysis identifier self-defined-rules goldfish let1)
    (scheme-langserver analysis identifier self-defined-rules goldfish typed-lambda))

(define (route&add 
  rules target-identifier
  file-linkage identifier-list current-document expanded+callee-list memory
  add-rule-procedure step)
  (let* ([top (root-ancestor target-identifier)]
        [expressions (map identifier-reference-identifier top)]
        [library-identifiers (map identifier-reference-library-identifier top)]
        [possible-new-memory `(,@(reverse (cdr (reverse memory))) (,(car (reverse memory)) . ,identifier-list))])
    (cond 
      [(and (equal? library-identifiers '((srfi :23 error tricks))) (equal? expressions '(SRFI-23-error->R6RS)))
        (add-rule-procedure rules `((,do-nothing . ,begin-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((srfi private include))) (equal? expressions '(include/resolve)))
        (let ([target-lambda 
            (lambda (root-file-node root-library-node document index-node)
              (include-resolve-process root-file-node root-library-node document index-node 
                (lambda (current-document) 
                  (file-linkage-set! file-linkage (uri->path (document-uri document)) (uri->path (document-uri current-document)))
                  (step root-file-node root-library-node file-linkage current-document expanded+callee-list (reverse (cdr (reverse memory)))))))])
          (add-rule-procedure rules `((,target-lambda) . ,target-identifier)))]
      [(and (equal? library-identifiers '((ufo-try))) (equal? expressions '(try)))
        (add-rule-procedure rules `((,try-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((ufo-match))) (equal? expressions '(match)))
        (add-rule-procedure rules `((,match-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(let1)))
        (add-rule-procedure rules `((,let1-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii oop))) (equal? expressions '(define-case-class)))
        (add-rule-procedure rules `((,define-case-class-process) . ,target-identifier))]
      [(and (equal? library-identifiers '((liii base))) (equal? expressions '(typed-lambda)))
        (add-rule-procedure rules `((,typed-lambda-process) . ,target-identifier))]
      [(and (contain? (map identifier-reference-type top) 'syntax-variable) (not (contain? memory (car (reverse possible-new-memory))))) 
        ; (fold-left add-rule-procedure rules
        ;   (map 
        ;     (lambda (t)
        ;       `((,(lambda (root-file-node root-library-node document index-node)
        ;           (self-defined-syntax-process t index-node document expanded+callee-list 
        ;             (lambda (specific-document generated-index-node new-expanded+callee-list)
        ;               (step root-file-node root-library-node file-linkage specific-document generated-index-node new-expanded+callee-list 
        ;               ;看起来在处理identifier-list的时候，因为一开始没加,导致了一些问题。可能出在source->annotaiton的过程中，也可能出在step过程中
        ;                 ; `(,@(reverse (cdr (reverse memory))) (,(car (reverse memory)) . ,identifier-list))
        ;                 possible-new-memory))))) 
        ;         . ,t))
        ;     top))
        ;not now to delete
        rules
        ]
      [else rules])))
)