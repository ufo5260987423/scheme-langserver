(library (scheme-langserver analysis type walk-engine)
  (export 
    walk
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
    ; [((? variable? head) ': (? identifier-reference? tail)) tail]
    ; this following two are for type rules
    [(substitutions target-variable) (reify substitutions target-variable '())]
    [(substitutions target-variable paths)
      ; (pretty-print 'test1)
      ; (pretty-print (variable? target))
      ; (pretty-print 'test2)
      ; (pretty-print paths)
      (let* ([initial `(,target-variable)]
          [dryed (private-dry target-variable)]
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
                        (walk substitutions current))))))
              initial
              dryed)])
        ; (pretty-print 'test4)
        ; (pretty-print ready-for-recursive-result)
        ; (pretty-print 'test5)
        ; (pretty-print (append paths dryed))
        (if (equal? ready-for-recursive-result initial)
          initial
          (dedupe (apply append (map (lambda (item) (reify substitutions item (append paths dryed))) ready-for-recursive-result)))))]))

(define (private-substitute origin single-substitution)
  (cond 
    ;correspond to walk-left
    [(equal? origin (car single-substitution)) 
      (match single-substitution
        [((? variable? head) '= tail) tail]
        [((? variable? head) ': tail) tail]
        [else origin])]
    [(list? origin)
     (map (lambda (item) (private-substitute item single-substitution)) origin)]
    [(vector? origin)
     (vector-map (lambda (item) (private-substitute item single-substitution)) origin)]
    [else origin]))

(define (private-dry target)
  (cond 
    [(list? target) (apply append (map private-dry target))]
    [(vector? target) (apply append (map private-dry (vector->list target)))]
    [(identifier-reference? target) `(,target)]
    [(variable? target) `(,target)]
    [(equal? target 'something?) '(something?)]
    [else `(,target)]))

(define (construct-substitutions-between-index-nodes substitutions left-index-node right-index-node symbol)
  (map 
    (lambda (pair)
      (list (car pair) symbol (cadr pair)))
    (cartesian-product 
      `(,(index-node-variable left-index-node))
      `(,(index-node-variable right-index-node)))))

(define (construct-parameter-variable-products-with substitutions parameter-index-nodes)
  (let ([l (length parameter-index-nodes)]
        [variables-list
        (map 
          (lambda (index-node) `(,(index-node-variable index-node)))
          parameter-index-nodes)])
    (if (or (zero? l) (= 1 l))
      (map list variables-list)
      (letrec ([flat-tree
            (lambda (tree) 
              (if (pair? tree)
                (append (flat-tree (car tree)) (flat-tree (cadr tree)))
                (list tree)))])
        (map flat-tree (apply cartesian-product variables-list))))))

(define (construct-lambdas-with return-variables parameter-variable-products)
  (map 
    (lambda (item)
      (list (car item) '<- (cadr item)))
    (cartesian-product return-variables parameter-variable-products)))

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