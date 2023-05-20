(library (scheme-langserver analysis type util)
  (export 
    lambda?
    lambda-templates->new-substitution-list
    has-intersection?
    type->string)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type walk-engine)
    (scheme-langserver analysis type variable))

(define (lambda? body)
  (if (list? body)
    (match body
      [(head '<- tail) #t]
      [else #f])
    #f))

(define (type->string type)
  (cond
    [(list? type) 
      (string-append 
        "(" 
        (fold-left 
          (lambda (remain current) 
            (if (equal? "" remain) current (string-append remain " " current))) 
            "" 
          (map type->string type))
        ")")]
    [(vector? type) 
      (string-append 
        "#(" 
        (fold-left 
          (lambda (remain current) 
            (if (equal? "" remain) current (string-append remain " " current))) 
            "" 
          (map type->string (vector->list type)))
        ")")]
    [(symbol? type) (symbol->string type)]
    [(identifier-reference? type) (type->string (identifier-reference-identifier type))]))

(define (has-intersection? type0 type1)
  (if (private-type-equal? type0 type1)
    #t
    (let ([segments0 (private-segment type0)]
        [segments1 (private-segment type1)])
      (or 
        (and (null? segments0) (null? segments1))
        (not (null? (private-lambda-templates->new-substitution-list (map (lambda (segment) `(,segment = ,(make-variable))) segments1) segments0 segments1)))
        (not (null? (private-lambda-templates->new-substitution-list (map (lambda (segment) `(,segment = ,(make-variable))) segments0) segments1 segments0)))))))

(define (private-type-equal? type0 type1)
  (cond 
    [(or (equal? type0 'something) (equal? type1 'something)) #t]
    [(equal? type0 type1) #t]
    [(and (identifier-reference? type0) (identifier-reference? type1)) 
      (or (is-ancestor-of? type0 type1) (is-ancestor-of? type1 type0))]
    [(and (list? type0) (list? type1))
      (let loop ([body0 type0] [body1 type1])
        (cond 
          [(and (null? body0) (null? body1)) #t]
          [(and (not (null? body0)) (not (null? body1)))
            (let ([head0 (car body0)]
                [head1 (car body1)])
              (private-type-equal? type0 type1))]
          [else #f]))]
    [(and (vector? type0) (vector? type1))
      (let loop ([body0 (vector->list type0)] [body1 (vector->list type1)])
        (cond 
          [(and (null? body0) (null? body1)) #t]
          [(and (not (null? body0)) (not (null? body1)))
            (let ([head0 (car body0)]
                [head1 (car body1)])
              (private-type-equal? type0 type1))]
          [else #f]))]
    [else #f]))

(define (lambda-templates->new-substitution-list substitutions lambda-templates return-variable-list ready-list) 
  (if (null? ready-list)
    substitutions
    (fold-left
      (lambda (substitutions-tmp lambda-template)
        (let ([tmp 
              (private-lambda-templates->new-substitution-list 
                substitutions-tmp
                (private-segment (caddr lambda-template))
                ready-list)])
          (if (null? tmp)
            substitutions-tmp
            (fold-left 
              add-to-substitutions
              tmp 
              (map 
                (lambda (return-variable)
                  `(,return-variable = ,(car lambda-template)))
                return-variable-list)))))
      substitutions
      lambda-templates)))

;return '() substitutions or extended-substitutions
;and '() represent an error
(define private-lambda-templates->new-substitution-list
  (case-lambda 
    ;process null values
    ;if current-segment in other clause is available, they won't jump in this clause
    [(substitutions rest-segments ready-list)
      (cond 
        [(and (null? rest-segments) (null? ready-list)) substitutions]

        ;rest-segments is null and ready-list is not null
        [(null? rest-segments) '()]

        ;rest-segments is not null and ready-list is null
        ;and both of them are not null
        [else 
          (private-lambda-templates->new-substitution-list 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            ready-list
            #f)])]
    ;jump from above clause
    ;suppose current-segment is not null
    ;suppose ready-list is not null
    ;rest-segments may be null
    ;this clause won't extend substitutions
    ;this clause won't check whether rest-segments is null
    [(substitutions current-segment rest-segments ready-list last?)
      (cond
        [(and (private-is-... current-segment) (null? ready-list))
          ;skip current-segment is available
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            ready-list)]
        [(private-is-... current-segment)
          (let ([tmp
                (private-lambda-templates->new-substitution-list 
                  substitutions
                  current-segment
                  rest-segments
                  (car ready-list)
                  (cdr ready-list)
                  last?)])
            (if (null? tmp)
              ;skip current-segment is available
              (private-lambda-templates->new-substitution-list 
                substitutions
                rest-segments
                ready-list)
              tmp))]

        [(and (private-is-**1 current-segment) (null? ready-list) last?)
          ;skip current-segment is available
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            ready-list)]
        [(and (private-is-**1 current-segment) (null? ready-list) (not last?)) '()]
        [(and (private-is-**1 current-segment) last?) 
          (let ([tmp
                (private-lambda-templates->new-substitution-list 
                  substitutions
                  current-segment
                  rest-segments
                  (car ready-list)
                  (cdr ready-list)
                  last?)])
            (if (null? tmp)
              ;skip current-segment is available
              (private-lambda-templates->new-substitution-list 
                substitutions
                rest-segments
                ready-list)
              tmp))]
        [(and (private-is-**1 current-segment) (not last?)) 
          (private-lambda-templates->new-substitution-list 
            substitutions
            current-segment
            rest-segments
            (car ready-list)
            (cdr ready-list)
            last?)]

        [last? 
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            ready-list)]
        [(null? ready-list) '()]
        [else 
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car ready-list) 
            (cdr ready-list) 
            last?)])]
    ;do actural substitution extending
    ;current-segment is not null
    ;current is not null
    ;rest-segments may be null
    ;ready-list may be null
    ;jump from above last clause
    ;jump into above two clauses
    [(substitutions current-segment rest-segments current ready-list last?)
      (let* ([type (private-type-of current-segment)]
          ;avoid unsuitable implicit-conversion
          ;only identifier-calling in initializations would cause implicit conversion
          [variable (if (index-node? current) (index-node-variable current) current)]
          [variable-list-pre
            (if (index-node? current)
              (if (find 
                  (lambda (initialization) 
                    (is-ancestor? initialization current)) 
                  (map identifier-reference-initialization-index-node (variable-identifier-references variable)))
                (reify substitutions variable)
                '())
              (reify substitutions variable))]
          [variable-list 
            (filter 
              (lambda (item) (not (equal? type item))) 
              ;this will import parameters' variable
              (filter variable? variable-list-pre))]
          [new-substitutions (map (lambda (single-variable) `(,single-variable = ,type)) variable-list)]
          ;default extension
          [extended-substitutions (fold-left add-to-substitutions substitutions new-substitutions)]
          [result
            (private-lambda-templates->new-substitution-list 
              extended-substitutions
              rest-segments
              ready-list)])
        ;well, null means failure
        (if (null? result)
          ;attach current-segment to next item 
          (cond
            ;mas, não há item disponível
            [(null? ready-list) '()]
            [(private-is-... current-segment)
              (private-lambda-templates->new-substitution-list 
                extended-substitutions
                current-segment 
                rest-segments 
                ready-list
                #t)]
            [(private-is-**1 current-segment)
              (private-lambda-templates->new-substitution-list 
                extended-substitutions
                current-segment 
                rest-segments 
                ready-list
                #t)]
            [else '()])
          result))]))

(define (private-is-... segment) 
  (if (list? segment)
    (equal? '... (car (reverse segment))))
    #f)

(define (private-is-**1 segment) 
  (if (list? segment)
    (equal? '**1 (car (reverse segment))))
    #f)

(define (private-type-of segment) 
  (if (list? segment)
    (car segment)
    segment))

(define (private-segment rule-list)
  (let loop ([loop-body rule-list] [result '()])
    (if (null? loop-body)
      result
      (cond
        [(equal? (car loop-body) '...) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (equal? (car (reverse result)) '...)
                  (equal? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,(list (car (reverse result)) '...)))))))]
        [(equal? (car loop-body) '**1) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (equal? (car (reverse result)) '...)
                  (equal? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,(list (car (reverse result)) '**1)))))))]
        [else (loop (cdr loop-body) (append result `(,(car loop-body))))]))))
)