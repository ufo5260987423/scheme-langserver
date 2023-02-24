(library (scheme-langserver analysis type walk-engine)
  (export 
    walk
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


(define walk-*
  (case-lambda 
    [(substitutions target) (walk-* substitutions target '())]
    [(substitutions target paths)
      (cond
        [(null? target) '()]
        [(contain? paths target) '()]
        [(list? target) (map (lambda (sub-target) (walk-* substitutions sub-target paths)) target)]
        [else 
          (walk-*
            (map private-digest (walk substitutions target))
            `(,@path ,target)) ])]))

(define (private-digest target-substitutions)
  (match target-substitutions
    ; [(? index-node? target) `(,target ,@(walk-* substitutions target (append paths target)))]
    ; [(? variable? target) `(,target ,@(walk-* substitutions target (append paths target)))]
    ; head is acturally the target for our privaate-walk-left
    [(head '= tail) tail]
    [((? index-node? head) ': (? variable? tail)) tail]
    [(head ': (? identifier-reference? tail)) tail]
    [((? identifier-reference head) '< (? identifier-reference tail)) tail]
    ; [((? identifier-reference head) '> (? identifier-reference tail)) ]
    [else '()]))

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
    (fold-left (lambda (item) (remove-from-substitution substitutions item)) target)
    (filter
      (lambda (substitution) (not (contain? (private-dry substitution) target)))
      substitutions)))

(define (private-dry target)
  (if (list? target)
    (apply append (map private-dry target))
    `(,target)))
)