(library (scheme-langserver analysis type util)
  (export 
    lambda?
    lambda-templates->new-substitution-list
    pure-identifier-reference?
    pure-variable?)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type walk-engine)
    (scheme-langserver analysis type variable))

(define (lambda? body)
  (if (list? body)
    (= 2 (length body))
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

(define (lambda-templates->new-substitution-list substitutions lambda-templates return-variable-list rest-index-node-list) 
  (if (null? rest-index-node-list)
    substitutions
    (fold-left
      (lambda (substitutions-tmp rule-segments)
        (let ([tmp 
              (private-lambda-templates->new-substitution-list 
                substitutions-tmp
                ;lambda-template->parameter-templates
                (cadr rule-segments)
                rest-index-node-list)])
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
    [(substitutions rest-segments rest-index-nodes)
      (cond 
        [(and (null? rest-segments) (null? rest-index-nodes)) substitutions]

        ;rest-segments is null and rest-index-nodes is not null
        [(null? rest-segments) '()]

        ;rest-segments is not null and rest-index-nodes is null
        ;and both of them are not null
        [else 
          (private-lambda-templates->new-substitution-list 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            rest-index-nodes
            #f)])]
    ;jump from above clause
    ;suppose current-segment is not null
    ;suppose rest-index-nodes is not null
    ;rest-segments may be null
    ;this clause won't extend substitutions
    ;this clause won't check whether rest-segments is null
    [(substitutions current-segment rest-segments rest-index-nodes last?)
      (cond
        [(and (private-is-... current-segment) (null? rest-index-nodes))
          ;skip current-segment is available
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            rest-index-nodes)]
        [(private-is-... current-segment)
          (let ([tmp
                (private-lambda-templates->new-substitution-list 
                  substitutions
                  current-segment
                  rest-segments
                  (car rest-index-nodes)
                  (cdr rest-index-nodes)
                  last?)])
            (if (null? tmp)
              ;skip current-segment is available
              (private-lambda-templates->new-substitution-list 
                substitutions
                rest-segments
                rest-index-nodes)
              tmp))]

        [(and (private-is-**1 current-segment) (null? rest-index-nodes) last?)
          ;skip current-segment is available
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            rest-index-nodes)]
        [(and (private-is-**1 current-segment) (null? rest-index-nodes) (not last?)) '()]
        [(and (private-is-**1 current-segment) last?) 
          (let ([tmp
                (private-lambda-templates->new-substitution-list 
                  substitutions
                  current-segment
                  rest-segments
                  (car rest-index-nodes)
                  (cdr rest-index-nodes)
                  last?)])
            (if (null? tmp)
              ;skip current-segment is available
              (private-lambda-templates->new-substitution-list 
                substitutions
                rest-segments
                rest-index-nodes)
              tmp))]
        [(and (private-is-**1 current-segment) (not last?)) 
          (private-lambda-templates->new-substitution-list 
            substitutions
            current-segment
            rest-segments
            (car rest-index-nodes)
            (cdr rest-index-nodes)
            last?)]

        [last? 
          (private-lambda-templates->new-substitution-list 
            substitutions
            rest-segments
            rest-index-nodes)]
        [(null? rest-index-nodes) '()]
        [else 
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes) 
            last?)])]
    ;do actural substitution extending
    ;current-segment is not null
    ;current-index-node is not null
    ;rest-segments may be null
    ;rest-index-nodes may be null
    ;jump from above last clause
    ;jump into above two clauses
    [(substitutions current-segment rest-segments current-index-node rest-index-nodes last?)
      (let* ([type (private-type-of current-segment)]
          [variable-list 
              (filter 
                (lambda (item) 
                  (not (equal? type item))) 
                ;this will import parameters' variable
                (filter variable? (reify substitutions current-index-node)))]
          [new-substitutions (map (lambda (single-variable) `(,single-variable = ,type)) variable-list)]
          ;default extension
          [extended-substitutions (dedupe (append substitutions new-substitutions))]
          [result
            (private-lambda-templates->new-substitution-list 
              extended-substitutions
              rest-segments
              rest-index-nodes)])
        ;well, null means failure
        (if (null? result)
          ;attach current-segment to next index-node
          (cond
            ;mas, não há index-node disponível
            [(null? rest-index-nodes) '()]
            [(private-is-... current-segment)
              (private-lambda-templates->new-substitution-list 
                extended-substitutions
                current-segment 
                rest-segments 
                rest-index-nodes
                #t)]
            [(private-is-**1 current-segment)
              (private-lambda-templates->new-substitution-list 
                extended-substitutions
                current-segment 
                rest-segments 
                rest-index-nodes
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