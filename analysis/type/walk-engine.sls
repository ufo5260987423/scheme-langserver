(library (scheme-langserver analysis type walk-engine)
  (export 
    walk:index-node->single-variable-list
    walk:supper-type-list
    reify
    add
    remove)
  (import 
    (chezscheme)

    (scheme-langserver util sub-list)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis type variable)

    (ufo-match))

(define reify
  (case-lambda 
    ;suppose target is atom
    ;in substitutions we have following forms
    ; [((? variable? head) '= tail) tail] tail is supposed as list of variables
    ; [((? index-node? head) ': (? variable? tail)) tail]
    ; [((? variable? head) ': (? identifier-reference? tail)) tail]
    ; this following two are for type rules
    ; [((? identifier-reference? head) '< (? identifier-reference? tail)) tail]
    ; [((? identifier-reference? head) '> (? identifier-reference? tail)) tail]
    [(substitutions target) (reify substitutions target '())]
    [(substitutions target paths)
      (let loop ([body (private-dry target)]
          [result `(,target)]
          [new-paths paths])
        (if (null? body)
          (if (= (length paths) (new-paths))
            result
            (apply append (map (lambda (item) (reify substitutions item new-paths)) result)))
          (let* ([current (car body)])
            (loop 
              (cdr body)
              (if (contain? new-paths current)
                result
                (map 
                  (lambda (single-substitution) 
                    (map 
                      (lambda (single-target)
                        (private-substitute single-target single-substitution))
                      result))
                  (walk substitutions current)))
              (dedupe `(,@new-paths ,current))))))]))

(define (private-substitute origin single-substitution)
  (cond 
    ;correspond to walk-left
    [(equal? origin (car single-substitution)) 
      (match target-substitutions
        [((? variable? head) '= tail) tail]
        [((? index-node? head) ': (? variable? tail)) tail]
        [((? variable? head) ': (? identifier-reference? tail)) tail]
        [else origin])]
    [(list? origin) 
      (map (lambda (item) (private-substitute item single-substitution) origin))]
    [else origin]))

(define (private-dry target)
  (cond 
    [(list? target) (apply append (map private-dry target))]
    [(index-node? target) `(,target)]
    [(identifier-reference? target) `(,target)]
    [else '()]))

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

(define (walk:supper-type-list substitutions identifier-refeference)
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
  (map caddr (filter (lambda(substitution) (equal? (car substitution left)) substitutions))))

(define (private-walk-right substitutions right)
  (map car (filter (lambda(substitution) (equal? (caddr substitution right)) substitutions))))

(define add-to-substitutions 
  (case-lambda 
    [(target) (list target)]
    [(substitutions target)
      (if (contain? substitutions target)
        substitution
        `(,@substitutions ,target))]))

(define (remove-from-substitution substitutions target)
  (if (list? target)
    (fold-left 
      (lambda (substitutions-tmp item) 
        (remove-from-substitution substitutions-tmp item)) 
      substitutions
      target)
    (filter
      (lambda (substitution) (not (contain? (private-dry substitution) target)))
      substitutions)))

(define (private-dry target)
  (if (list? target)
    (apply append (map private-dry target))
    `(,target)))
)