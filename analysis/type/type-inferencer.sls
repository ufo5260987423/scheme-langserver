(library (scheme-langserver analysis type type-inferencer)
  (export 
    type-inference-for
    construct-substitution-list-for)
  (import 
    (chezscheme)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)
    
    (scheme-langserver analysis type rules if)
    (scheme-langserver analysis type rules let)
    (scheme-langserver analysis type rules lambda)
    (scheme-langserver analysis type rules trivial)
    (scheme-langserver analysis type rules define)
    (scheme-langserver analysis type rules application)
    ; (scheme-langserver analysis type rules define)

    (scheme-langserver analysis type util)
    (scheme-langserver analysis type walk-engine))

; (define (find-type-conflicts index-node document)
;   (let ([types (filter is-pure-identifier-reference-misture? (type-inference-for index-node document))]
;       )
;     (if (< (length types) 2)

;     )
;   )
; ))

; (define (private-check-identifier-reference item)
;   (cond
;     [(list? item) (apply and (map private-filter item))]
;     [(vector? item) (apply and (map private-filter (vector->list item)))]
;     [else (identifier-reference? item)]))

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

;; We regard the indexes and references as a graph of existed variable and values. 
;; try to get result type by substitution
(define (type-inference-for index-node document)
  (dedupe (reify (document-substitution-list document) index-node)))

(define (construct-substitution-list-for document)
  (document-substitution-list-set! 
    document 
    (fold-left 
      private-add-implicit-convertions
      (apply 
        append
        (map 
          (lambda (index-node) (private-construct-substitution-list document index-node '()))
          (document-index-node-list document)))
      (document-index-node-list document))))

;gradule typing: unsafe convertion
(define (private-add-implicit-convertions substitutions index-node)
  (let ([children (index-node-children index-node)])
    (if (null? children)
      substitutions
      (let* ([base (fold-left private-add-implicit-convertions substitutions children)]
          [head-index-node (car children)]
          [rest-index-nodes (cdr children)]
          [reified-head-result (reify base head-index-node)]
          [filtered-lambdas (dedupe (filter lambda? reified-head-result))]
          [return-variable-list (walk:index-node->single-variable-list substitutions index-node)])
        (lambda-templates->new-substitution-list base filtered-lambdas return-variable-list rest-index-nodes)))))

(define (private-construct-substitution-list document index-node base-substitution-list)
  (let* ([children (index-node-children index-node)]
      [children-substitution-list
        (apply 
          append 
          (map 
            (lambda (child) 
              (private-construct-substitution-list document child base-substitution-list))
            children))])
    (fold-left
      (lambda (current-substitutions proc)
        ; (pretty-print proc)
        (filter
          (lambda (a) (not (null? a)))
          (proc document index-node current-substitutions)))
      children-substitution-list
      ;all these processor except trivial-process must add its result to current index-node
      ;such as for (app param ...), app's result type could be (return-type (param-type ...))
      ;must extend substitution with (current-index-node-variable = return-type)
      (list 
        ;this should be the first
        trivial-process

        ;their position and order, I don't care.
        define-process
        let-process
        if-process
        lambda-process

        ;this should be the last
        application-process
      ))))
)