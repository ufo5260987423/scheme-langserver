(library (scheme-langserver analysis type argument-checker)
  (export argument-checker-attach)
  (import 
    (chezscheme)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver util contain)
    (scheme-langserver util dedupe)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver analysis type meta-type)
    (scheme-langserver analysis type util))

;;using dynamic programming
(define argument-checker-attach 
  (case-lambda 
    [(argument-index-nodes document root-identifiers) 
      (map 
        (lambda (root-identifier) 
          (map 
            (lambda (single-expression)
              (if (lambda? single-expression)
                (argument-checker-attach 
                  argument-index-nodes 
                  document 
                  (cadr single-expression) 
                  root-identifier)))
            (identifier-reference-type-expressions root-identifier)))
        root-identifiers)]
    [(argument-index-nodes document parameter-rules root-identifier)
      (argument-checker-attach 
        argument-index-nodes 
        document 
        parameter-rules 
        (identifier-reference-index-node root-identifier) 
        (identifier-reference-document root-identifier))]
    [(argument-index-nodes document parameter-rules reference-index-node reference-document)
      (cond
        [(and (null? argument-index-nodes) (null? parameter-rules)) #t]
        [(or (not (null? argument-index-nodes)) (not (null? parameter-rules))) #f]
        [else
          (let* ([rule-segments (private-segment parameter-rules)]
              [current-segment (car rule-segments)]
              [current-index-node (car argument-index-nodes)]
              [current-rule (car current-segment)]
              [type-expression (private-type-expression-of current-segment)])
            (cond
              [(private-is-... current-segment)
                (if (check-argument-satisfy>= type-expression (index-node-actural-have-type current-index-node))
                  (cond
                    [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) reference-index-node reference-document)
                      (index-node-should-have-type-set!  current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules reference-index-node reference-document)
                      (index-node-should-have-type-set! current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [else #f])
                  (cond
                    [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) reference-index-node reference-document) #t]
                    [else #f]))]
              [(private-is-**1 current-segment)
                (if (check-argument-satisfy>= type-expression (index-node-actural-have-type current-index-node))
                  (cond
                    [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) reference-index-node reference-document)
                      (index-node-should-have-type-set! current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules reference-index-node reference-document)
                      (index-node-should-have-type-set! current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [else #f])
                  #f)]
              [else 
                (if (check-argument-satisfy>= type-expression (index-node-actural-have-type current-index-node))
                  (cond
                    [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) reference-index-node reference-document)
                      (index-node-should-have-type-set! current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules reference-index-node reference-document)
                      (index-node-should-have-type-set! current-index-node (dedupe (append (index-node-should-have-type current-index-node) `(,current-rule))))
                      #t]
                    [else #f])
                  #f)]))])]))

(define (check-argument-satisfy>= should-have-type actural-have-type)
  (let* ([intersection (type-satisfy>=intersection should-have-type actural-have-type)])
    (if (null? actural-have-type)
      #t
      (not (null? intersection)))))

(define (private-is-... segment) (equal? '... (car (reverse segment))))

(define (private-is-**1 segment) (equal? '**1 (car (reverse segment))))

(define (private-type-expression-of segment) (car segment))

(define (private-segment rule-list)
  (let loop ([loop-body rule-list] [result '()])
    (if (null? loop-body)
      (cond
        [(equal? (car loop-body) '...) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (contain? (car (reverse result)) '...)
                  (contain? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,@(car (reverse result)) ...))))))]
        [(equal? (car loop-body) '**1) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (contain? (car (reverse result)) '...)
                  (contain? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,@(car (reverse result)) **1))))))]
        [else (loop (cdr loop-body) (append result `(,(car loop-body))))]))))
)