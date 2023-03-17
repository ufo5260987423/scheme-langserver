(library (scheme-langserver analysis type walk-engine)
  (export 
    walk
    walk:index-node->single-variable-list
    walk:supper-type-list
    reify
    add-to-substitutions
    remove-from-substitutions

    construct-substitutions-between-index-nodes
    construct-parameter-variable-products-with
    construct-lambdas-with)
  (import 
    (chezscheme)

    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type variable)

    (ufo-match))

(define reify
  (case-lambda 
    ;suppose target is atom
    ;in substitutions we have following forms
    ; [((? variable? head) '= tail) tail] tail is supposed as list of variables or misture of variables and identifier-references
    ; [((? index-node? head) ': (? variable? tail)) tail]
    ; [((? variable? head) ': (? identifier-reference? tail)) tail]
    ; this following two are for type rules
    ; [((? identifier-reference? head) '< (? identifier-reference? tail)) tail]
    ; [((? identifier-reference? head) '> (? identifier-reference? tail)) tail]
    ; [((? identifier-reference? head) '< 'something?) tail]
    [(substitutions target) (reify substitutions target '())]
    [(substitutions target paths)
      ; (pretty-print 'test1)
      ; (pretty-print (index-node? target))
      ; (pretty-print (variable? target))
      ; (pretty-print 'test2)
      ; (pretty-print paths)
      (let* ([initial `(,target)]
          [dryed (private-dry target)]
          [ready-for-recursive-result
            (fold-left 
              (lambda (result current)
                (if (contain? paths current)
                  result 
                  (append 
                    result
                    (apply 
                      append
                      (map 
                        (lambda (single-substitution) 
                          (map 
                            (lambda (single-target)
                              ; (pretty-print 'test3)
                              ; (pretty-print single-substitution)
                              (private-substitute single-target single-substitution))
                            result))
                        (walk substitutions current)))
                        )
                        ))
              initial
              dryed)])
        ; (pretty-print 'test4)
        ; (pretty-print ready-for-recursive-result)
        ; (pretty-print 'test5)
        ; (pretty-print (append paths dryed))
        (if (equal? ready-for-recursive-result initial)
          initial
          (apply append (map (lambda (item) (reify substitutions item (append paths dryed))) ready-for-recursive-result))))
          ]))

(define (private-substitute origin single-substitution)
  (cond 
    ;correspond to walk-left
    [(equal? origin (car single-substitution)) 
      (match single-substitution
        [((? variable? head) '= tail) tail]
        [((? index-node? head) ': (? variable? tail)) tail]
        [((? variable? head) ': tail) tail]
        [else origin])]
    [(list? origin) (map (lambda (item) (private-substitute item single-substitution)) origin)]
    [else origin]))

(define (private-dry target)
  (cond 
    [(list? target) (apply append (map private-dry target))]
    [(index-node? target) `(,target)]
    [(identifier-reference? target) `(,target)]
    [(variable? target) `(,target)]
    [(equal? target 'something?) '(something?)]
    [else `(,target)]))

(define (construct-substitutions-between-index-nodes substitutions left-index-node right-index-node symbol)
  (apply append 
    (map 
      (lambda (left-variable)
        (map 
          (lambda (right-variable)
            (list left-variable symbol right-variable))
          (walk substitutions right-index-node)))
      (walk substitutions left-index-node))))

(define (construct-parameter-variable-products-with substitutions parameter-index-nodes)
  (let ([variables-list
        (map 
          (lambda (index-node)
            (walk substitutions index-node))
          parameter-index-nodes)]
        [flat-pair 
        (lambda (pair) 
          (if (pair? pair)
            (apply append (map flat-pair pair))
            pair))])
    (apply cartesian-product variables-list)))

(define (construct-lambdas-with return-variables parameter-variable-products)
  (cartesian-product return-variables parameter-variable-products))

(define (walk:index-node->single-variable-list substitutions index-node)
  (apply append (map 
    (lambda (substitution)
      (match substitution
        [((? index-node? head) ': (? variable? tail)) 
          (if (equal? index-node head)
            `(,tail)
            '())]
        [else '()]))
    substitutions)))

(define (walk:supper-type-list substitutions identifier-reference)
  (apply append (map 
    (lambda (substitution)
      (match substitution
        [((? identifier-reference? head) '< (? identifier-reference? tail)) 
          (if (equal? identifier-reference head)
            (dedupe `(,@(walk:supper-type-list substitutions tail) ,tail))
            '())]
        [((? identifier-reference? head) '> (? identifier-reference? tail)) 
          (if (equal? identifier-reference tail)
            (dedupe `(,@(walk:supper-type-list substitutions head) ,head))
            '())]
        [else '()]))
    substitutions)))

(define (walk substitutions target)
  ; (dedupe `(,@(private-walk-left substitutions target) ,@(private-walk-right substitutions target)))
  (private-walk-left substitutions target))

(define (private-walk-left substitutions left)
  (filter (lambda (substitution) (equal? (car substitution) left)) substitutions))

(define (private-walk-right substitutions right)
  (filter (lambda (substitution) (equal? (caddr substitution) right)) substitutions))

(define add-to-substitutions 
  (case-lambda 
    [(target) (list target)]
    [(substitutions target)
      (if (contain? substitutions target)
        substitutions
        (if (null? target)
          substitutions
          `(,@substitutions ,target)))]))

(define (remove-from-substitutions substitutions target)
  (if (list? target)
    (fold-left 
      (lambda (substitutions-tmp item) 
        (remove-from-substitutions substitutions-tmp item)) 
      substitutions
      target)
    (filter
      (lambda (substitution) (not (contain? (private-dry substitution) target)))
      substitutions)))
)