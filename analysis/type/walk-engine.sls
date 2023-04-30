(library (scheme-langserver analysis type walk-engine)
  (export 
    walk
    reify
    add-to-substitutions

    construct-substitutions-between-index-nodes
    construct-parameter-variable-products-with
    construct-lambdas-with

    debug:substitution-sorted? 

    substitution-compare)
  (import 
    (chezscheme)

    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util cartesian-product)
    (scheme-langserver util natural-order-compare)
    (scheme-langserver util binary-search)

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
    [(substitutions target-expression) (reify substitutions target-expression '())]
    [(substitutions target-expression memory) 
      (cond
        [(variable? target-expression) 
          ;only extend memory at here
          (let* ([new-memory `(,@memory ,target-expression)]
              [walk-results (private-filter-not-null (filter (lambda (item) (not (contain? new-memory item))) (map caddr (walk substitutions target-expression))))]
              [reified-results (apply append (map (lambda (item) (reify substitutions item new-memory)) walk-results))])
            `(,@reified-results ,target-expression))] 
        [(or (list? target-expression) (vector? target-expression))
          (let* ([is-list? (list? target-expression)]
              [normalized (if is-list? target-expression (vector->list target-expression))]
              [reified-list (map (lambda (item) (reify substitutions item memory)) normalized)]
              [cartesian-product-list (apply cartesian-product (filter (lambda (item) (not (null? item))) reified-list))]
              [depth (length normalized)]
              [result (map (lambda (single-cartesian-product) (private-tree-flat single-cartesian-product depth)) cartesian-product-list)])
            (if is-list? result (map list->vector result)))]
        [else `(,target-expression)])]))

(define (private-filter-not-null list-instance)
  (filter (lambda (item) (not (null? item))) list-instance))

(define (private-tree-flat single-cartesian-product depth)
  (if (< 1 depth)
    `(,@(private-tree-flat (car single-cartesian-product) (- depth 1)) ,(cadr single-cartesian-product))
    `(,single-cartesian-product)))

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
  (binary-search 
    (list->vector substitutions) 
    substitution-compare 
    `(,left '? '?))
  ; (filter (lambda (substitution) (equal? (car substitution) left)) substitutions)
  )

(define (debug:substitution-sorted? substitutions)
  (let loop ([l substitutions]
      [s (sort substitution-compare substitutions)])
    (cond 
      [(and (null? l) (null? s)) #t]
      [(or (null? l) (null? s)) #f]
      [(equal? (car (car l)) (car (car s))) (loop (cdr l) (cdr s))]
      [else 
        (pretty-print 'debug:sorted-origin)
        (pretty-print (car l))
        (pretty-print 'debug:sorted-sorted)
        (pretty-print (car s))
        #f])))

(define (private-walk-right substitutions right)
  (filter (lambda (substitution) (equal? (caddr substitution) right)) substitutions))

(define (substitution-compare item0 item1)
  (natural-order-compare 
    (variable->uuid->string (car item0))
    (variable->uuid->string (car item1))))

(define add-to-substitutions 
  (case-lambda 
    [(target) (list target)]
    [(substitutions target)
      (if (null? target)
        substitutions
        (dedupe (merge substitution-compare substitutions (list target))))]))
)