(library (scheme-langserver analysis type util)
  (export 
    lambda?
    construct-lambda
    construct-type-expression-with-meta
    collect-reference-should-have-type)
  (import 
    (chezscheme)
    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type walk-engine))

(define (lambda? body)
  (= 2 (length body)))

(define (construct-type-expression-with-meta meta-identifier)
  (if (list? meta-identifier)
    (map construct-type-expression-with-meta meta-identifier)
    (let* ([target-meta (find-meta '(rnrs))]
        [target-identifier (find (lambda(x) (equal? (identifier-reference-identifier x) meta-identifier)) target-meta)])
      (if target-identifier target-identifier meta-identifier))))

(define add-applyable-lambda-types-to-substitutions
  (case-lambda
    [(substitutions application-index-node) 
      (let* ([children (index-node-children application-index-node)]
        [head-index-node (car children)]
        [rest-children (cdr children)]
        [head-variable-list (walk:index-node->single-variable-list head-index-node)])
        (fold-left
          (lambda (substitutions-tmp head-variable)
            (add-applyable-lambda-types-to-substitutions 
              substitutions-tmp
              head-variable
              rest-children))
          substitutions
          head-variable-list))]
    [(substitutions head-variable rest-index-node-list) 
      (if (null? rest-index-node-list)
        substitutions
        (fold-left
          (lambda (substitutions-tmp rule-segments)
            (private-add-applyable-lambda-types-to-substitutions 
              substitutions-tmp
              rule-segments
              rest-index-node-list))
          substitutions
          (map 
            (lambda (lambda-types) (private-segment (cadr lambda-types))) 
            (filter lambda? (walk substitutions head-variable)))))]))

(define private-add-applyable-lambda-types-to-substitutions
  (case-lambda 
    ;only for initialization
    ;porque se-calher normalmente há um current-segment por initialized procedure
    [(substitutions rest-segments rest-index-nodes)
      (cond
        [(and (null? rest-segments) (null? rest-index-nodes)) substitutions]
        [(null? rest-segments) '()]
        [(null? rest-index-nodes) '()]
        [else 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            rest-index-nodes
            #f)])]
    ;process null value 
    ;jump from other two clause
    ;directly reach termination at 1 2 3 4 7 9 11
    [(substitutions current-segment rest-segments rest-index-nodes last?)
      (cond
        ;1
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-... current-segment)) substitutions]
        ;2
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-**1 current-segment) last?) substitutions]
        ;3
        [(and (null? rest-segments) (null? rest-index-nodes) (private-is-**1 current-segment) (not last?)) '()]
        ;4
        [(and (null? rest-segments) (null? rest-index-nodes)) '()]

        ;5
        [(and (null? rest-segments) (private-is-... current-segment)) 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            last?)]
        ;6
        [(and (null? rest-segments) (private-is-**1 current-segment)) 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            last?)]
        ;7
        [(and (null? rest-segments)) '()]

        ;8
        [(and (null? rest-index-nodes) (private-is-... current-segment)) 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            current-index-node
            rest-index-nodes
            #f)]
        ;9
        [(and (null? rest-index-nodes) (private-is-**1 current-segment) (not last?)) '()]
        ;10
        [(and (null? rest-index-nodes) (private-is-**1 current-segment) last?) 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            (car rest-segments) 
            (cdr rest-segments) 
            current-index-node
            rest-index-nodes
            #f)]
        ;11
        [(and (null? rest-index-nodes)) '()]

        ;12
        [(and (private-is-**1 current-segment) (not last?)) 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes)
            #f)]
        ;13
        [(and (private-is-**1 current-segment) last?) 
          (let ([tmp-result 
                (private-add-applyable-lambda-types-to-substitutions 
                  substitutions 
                  (car rest-segments) 
                  (cdr rest-segments)
                  (car rest-index-nodes) 
                  (cdr rest-index-nodes)
                  #f)])
            (if (null? tmp-result)
              (private-add-applyable-lambda-types-to-substitutions 
                substitutions 
                current-segment 
                rest-segments 
                (car rest-index-nodes) 
                (cdr rest-index-nodes)
                #f)
              tmp-result))]
        ;14
        [(and (private-is-... current-segment)) 
          (let ([tmp-result 
                (private-add-applyable-lambda-types-to-substitutions 
                  substitutions 
                  (car rest-segments) 
                  (cdr rest-segments)
                  (car rest-index-nodes) 
                  (cdr rest-index-nodes)
                  #f)])
            (if (null? tmp-result)
              (private-add-applyable-lambda-types-to-substitutions 
                substitutions 
                current-segment 
                rest-segments 
                (car rest-index-nodes) 
                (cdr rest-index-nodes)
                #f)
              tmp-result))]
        ;15
        [else 
          (private-add-applyable-lambda-types-to-substitutions 
            substitutions 
            current-segment 
            rest-segments 
            (car rest-index-nodes) 
            (cdr rest-index-nodes) 
            #f)])]
    ;add to substitutions
    [(substitutions current-segment rest-segments current-index-node rest-index-nodes last?)
      (let* ([type (private-type-of current-segment)]
          [variable-list (walk:index-node->single-variable-list substitutions current-index-node)]
          [new-substitutions (map (lambda (single-variable) `(,current-index-node : ,single-variable)) variable-list)]
          [extended-substitutions `(,@substitutions ,@new-substitutions)])
        (cond
          ;jump in from: 5 14
          ;may jump in from: 8 10 13 14
          [(private-is-... current-segment)
            (private-add-applyable-lambda-types-to-substitutions 
              extended-substitutions
              current-segment 
              rest-segments 
              (cdr rest-index-nodes)
              #t)]
          ;jump in from: 6 12 13
          ;may jump in from: 8 10 13 14
          [(private-is-**1 current-segment)
            (let ([tmp-result0 
                  (private-add-applyable-lambda-types-to-substitutions 
                    extended-substitutions
                    current-segment 
                    rest-segments 
                    (cdr rest-index-nodes)
                    #t)])
              (if (and last? (null? tmp-result0))
                (private-add-applyable-lambda-types-to-substitutions 
                  substitutions
                  (car rest-segments)
                  (cdr rest-segments)
                  (cdr rest-index-nodes)
                  #f)
                '()))]
          ;jump in from: 15
          ;may jump in from: 8 10 13 14
          [else 
            (add-applyable-lambda-types-to-substitutions 
              extended-substitutions 
              current-segment 
              rest-rule-segments 
              #f)]))]))

(define (private-is-... segment) (equal? '... (car (reverse segment))))

(define (private-is-**1 segment) (equal? '**1 (car (reverse segment))))

(define (private-type-of segment) (car segment))

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