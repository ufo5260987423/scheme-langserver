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
(define (argument-checker-attach argument-index-nodes document parameter-rules root-identifiers)
  (cond
    [(and (null? argument-index-nodes) (null? parameter-rules)) #t]
    [(or (not (null? argument-index-nodes)) (not (null? parameter-rules))) #f]
    [else
      (let* ([rule-segments (private-segment parameter-rules)]
          [current-segment (car rule-segment)]
          [current-index-node (car argument-index-node)]
          [type-expression (private-type-expression-of segment)])
        (cond
          [(private-is-... current-segment)
            (if (apply or 
                (map
                  (lambda (root-identifier)
                    (check-argument-satisfy>= 
                      type-exression current-index-node document 
                      (index-node-actrua1l-have-type current-index-node) (identifier-reference-index-node root-identifier) (identifier-reference-document root-identifier)))
                  root-identifiers))
              (cond
                [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) root-identifiers)
                  (index-node-should-have-type-set!  current-index-node current-rule)
                  #t]
                [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules root-identifiers)
                  (index-node-should-have-type-set! current-index-node type-expression)
                  #t]
                [else #f])
              (cond
                [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) root-identifiers) #t]
                [else #f]))]
          [(private-is-**1 current-segment)
            (if (apply or 
                (map
                  (lambda (root-identifier)
                    (check-argument-satisfy>= 
                      type-exression current-index-node document 
                      (index-node-actrua1l-have-type current-index-node) (identifier-reference-index-node root-identifier) (identifier-reference-document root-identifier)))
                  root-identifiers))
              (cond
                [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) root-identifiers)
                  (index-node-should-have-type-set!  current-index-node current-rule)
                  #t]
                [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules root-identifiers)
                  (index-node-should-have-type-set! current-index-node type-expression)
                  #t]
                [else #f])
              #f)]
          [else 
            (if (apply or 
                (map
                  (lambda (root-identifier)
                    (check-argument-satisfy>= 
                      type-exression current-index-node document 
                      (index-node-actrua1l-have-type current-index-node) (identifier-reference-index-node root-identifier) (identifier-reference-document root-identifier)))
                  root-identifiers))
              (cond
                [(argument-checker-attach (cdr argument-index-nodes) document (cddr parameter-rules) root-identifiers)
                  (index-node-should-have-type-set!  current-index-node current-rule)
                  #t]
                [(argument-checker-attach (cdr argument-index-nodes) document parameter-rules root-identifiers)
                  (index-node-should-have-type-set! current-index-node type-expression)
                  #t]
                [else #f])
              #f)]))]))

(define (check-argument-satisfy>= index-node document parameter-rule root-identifiers)
  (let* ([should-have-type parameter-rule]
      [actrual-have-type (index-node-actrual-have-type index-node)]
      [intersection 
        (type-satisfy>=intersection 
          should-have-type (identifier-reference-index-node root-identifier) (identifier-reference-document root-identifier) 
          actrual-have-type index-node document)])
    (if (null? actrual-have-type)
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
                  (equal? (car (car (reverse result)) '...))
                  (equal? (car (car (reverse result)) '**1)))
                (raise "wrong rule")
                (loop (cdr loop-body) (append (reverse (cdr (reverse result)) `(,(car (reverse result)) ...)))))))]
        [(equal? (car loop-body) '**1) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (equal? (car (car (reverse result)) '...))
                  (equal? (car (car (reverse result)) '**1)))
                (raise "wrong rule")
                (loop (cdr loop-body) (append (reverse (cdr (reverse result)) `(,(car (reverse result)) **1)))))))]
        [else (loop (cdr loop-body) (append result `(,(car loop-body))))]))))
)