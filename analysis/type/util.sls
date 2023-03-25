(library (scheme-langserver analysis type util)
  (export 
    lambda?
    lambda-templates->new-substitution-list)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type walk-engine))

(define (lambda? body)
  (if (list? body)
    (= 2 (length body))
    #f))

(define (lambda-templates->new-substitution-list substitutions lambda-templates rest-index-node-list) 
  (if (null? rest-index-node-list)
    substitutions
    (fold-left
      (lambda (substitutions-tmp rule-segments)
        (private-lambda-templates->new-substitution-list 
          substitutions-tmp
          rule-segments
          rest-index-node-list))
      substitutions
      (map private-segment lambda-templates))))

(define private-lambda-templates->new-substitution-list
  (case-lambda 
    ;only for initialization
    ;porque se-calher normalmente há um current-segment por initialized procedure
    [(substitutions rest-segments rest-index-nodes)
      (cond
        [(and (null? rest-segments) (null? rest-index-nodes)) substitutions]
        [(null? rest-segments) '()]
        [(null? rest-index-nodes) '()]
        [else 
          (private-lambda-templates->new-substitution-list 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            rest-index-nodes
            #f)])]
    ;process null value 
    ;jump from other two clause
    ;directly reach termination at 1 2 3 4 7 9 11
    [(substitutions current-segment rest-segments rest-index-nodes last?)
  (pretty-print 'aaa)
      (cond
        ;1
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-... current-segment)) substitutions]
        ;2
        ;**1 has been consumed
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-**1 current-segment) last?) substitutions]
        ;3
        ;**1 haven't been consumed
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-**1 current-segment) (not last?)) '()]
        ;4
        [(and (null? rest-segments) (null? rest-index-nodes)) substitutions]

        ;5
        ;rest-index-node haven't been ran out
        [(and (null? rest-segments) (private-is-... current-segment)) 
  (pretty-print 'aaa0)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            last?)]
        ;6
        ;rest-index-node haven't been ran out
        [(and (null? rest-segments) (private-is-**1 current-segment)) 
  (pretty-print 'aaa1)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            last?)]
        ;7
        ;rest-index-node haven't been ran out
        [(null? rest-segments) '()]

        ;8
        [(and (null? rest-index-nodes) (private-is-... current-segment)) 
  (pretty-print 'aaa2)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            rest-index-nodes
            #f)]
        ;9
        [(and (null? rest-index-nodes) (private-is-**1 current-segment) (not last?)) '()]
        ;10
        [(and (null? rest-index-nodes) (private-is-**1 current-segment) last?) 
  (pretty-print 'aaa3)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            rest-index-nodes
            #f)]
        ;11
        [(and (null? rest-index-nodes)) '()]

        ;12
        [(and (private-is-**1 current-segment) (not last?)) 
  (pretty-print 'aaa4)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            #f)]
        ;13
        [(and (private-is-**1 current-segment) last?) 
  (pretty-print 'aaa5)
          (let ([tmp-result 
                ;be greedy
                (private-lambda-templates->new-substitution-list 
                  substitutions 
                  current-segment 
                  rest-segments 
                  (car rest-index-nodes) 
                  (cdr rest-index-nodes)
                  #t)])
            (if (null? tmp-result)
              (private-lambda-templates->new-substitution-list 
                substitutions 
                (car rest-segments) 
                (cdr rest-segments)
                (car rest-index-nodes) 
                (cdr rest-index-nodes)
                #f)
              tmp-result))]
        ;14
        [(and (private-is-... current-segment)) 
  (pretty-print 'aaa6)
          (let ([tmp-result 
                ;be greedy
                (private-lambda-templates->new-substitution-list 
                  substitutions 
                  current-segment 
                  rest-segments 
                  (car rest-index-nodes) 
                  (cdr rest-index-nodes)
                  #t)])
            (if (null? tmp-result)
              (private-lambda-templates->new-substitution-list 
                substitutions 
                (car rest-segments) 
                (cdr rest-segments)
                (car rest-index-nodes) 
                (cdr rest-index-nodes)
                #f)
              tmp-result))]
        ;15
        [else 
  (pretty-print 'aaa7)
          (private-lambda-templates->new-substitution-list 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes) 
            #f)])]
    ;add to substitutions
    [(substitutions current-segment rest-segments current-index-node rest-index-nodes last?)
  (pretty-print 'ccc)
      (let* ([type (private-type-of current-segment)]
          [variable-list (walk:index-node->single-variable-list substitutions current-index-node)]
          [new-substitutions (map (lambda (single-variable) `(,current-index-node : ,single-variable)) variable-list)]
          [extended-substitutions `(,@substitutions ,@new-substitutions)])
  (pretty-print 'ccc0)
        (cond
          ;jump in from: 5 14
          ;may jump in from: 13 14
          [(private-is-... current-segment)
  (pretty-print 'ccc1)
            (private-lambda-templates->new-substitution-list 
              extended-substitutions
              current-segment 
              rest-segments 
              (cdr rest-index-nodes)
              #t)]
          ;jump in from: 6 12 13
          ;may jump in from: 13 14
          [(private-is-**1 current-segment)
  (pretty-print 'ccc2)
            (let ([tmp-result0 
                  (private-lambda-templates->new-substitution-list 
                    extended-substitutions
                    current-segment 
                    rest-segments 
                    (cdr rest-index-nodes)
                    #t)])
              (if (and last? (null? tmp-result0))
                (private-lambda-templates->new-substitution-list 
                  substitutions
                  (car rest-segments)
                  (cdr rest-segments)
                  (cdr rest-index-nodes)
                  #f)
                '()))]
          ;jump in from: 15
          ;may jump in from: 13 14
          [else 
  (pretty-print 'ccc3)
            (private-lambda-templates->new-substitution-list 
              extended-substitutions 
              current-segment 
              rest-segments 
              rest-index-nodes
              #f)]))]))

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