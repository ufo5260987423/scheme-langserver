(library (scheme-langserver analysis type util)
  (export 
    lambda?
    lambda-templates->new-substitution-list
    pure-identifier-reference?
    pure-variable?)
  (import 
    (chezscheme)
    (ufo-match)

    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type walk-engine)
    (scheme-langserver analysis type variable))

(define (lambda? body)
  (if (list? body)
    (match body
      [(head '<- tail) #t]
      [else #f])
    #f))

(define (pure-variable? body)
  (if (list? body)
    (fold-left 
      (lambda (flag item)
        (and flag (pure-variable? item)))
      #t
      body)
    (variable? body)))

(define (pure-identifier-reference? body)
  (if (list? body)
    (fold-left 
      (lambda (flag item)
        (and flag (pure-identifier-reference? item)))
      #t
      body)
    (identifier-reference? body)))

(define (lambda-templates->new-substitution-list substitutions lambda-templates return-variable-list ready-list) 
  (if (null? ready-list)
    substitutions
    (fold-left
      (lambda (substitutions-tmp rule-segments)
        (let ([tmp 
              (private-lambda-templates->new-substitution-list 
                substitutions-tmp
                ;lambda-template->parameter-templates
                (caddr rule-segments)
                ready-list)])
          (if (null? tmp)
            substitutions
            (append
              tmp 
              (map 
                (lambda (return-variable)
                  `(,return-variable = ,(car rule-segments)))
                return-variable-list))
            )))
      substitutions
      (map private-segment lambda-templates))))

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
          [variable-list 
              (filter 
                (lambda (item) 
                  (not (equal? type item))) 
                ;this will import parameters' variable
                (filter variable? (reify substitutions current)))]
          [new-substitutions (map (lambda (single-variable) `(,single-variable = ,type)) variable-list)]
          ;default extension
          [extended-substitutions (dedupe (append substitutions new-substitutions))]
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