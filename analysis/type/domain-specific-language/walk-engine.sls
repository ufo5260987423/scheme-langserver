(library (scheme-langserver analysis type domain-specific-language walk-engine)
  (export 
    walk
    reify

    debug:substitution-sorted?)
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
    (scheme-langserver analysis type domain-specific-language variable)
    (scheme-langserver analysis type substitutions util)

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
        [(null? target-expression) '(())]
        [(variable? target-expression) 
          ;only extend memory at here
          (let* ([new-memory `(,@memory ,target-expression)]
              [walk-results 
                (filter 
                  (lambda (item) 
                    (and (not (null? item)) (not (contain? new-memory item)))) 
                  (map caddr (walk substitutions target-expression)))]
              [reified-results 
                (dedupe 
                  (apply append 
                    (map 
                      (lambda (item) 
                        (reify substitutions item new-memory))
                      (dedupe walk-results))))])
          (pretty-print 'target-expression)
          (pretty-print (length walk-results))
          (pretty-print (length reified-results))
          (print-graph #t)
          (pretty-print target-expression)
          (pretty-print walk-results)
            `(,@reified-results ,target-expression))] 
        [(list? target-expression) 
          (apply cartesian-product (map (lambda (item) (reify substitutions item memory)) target-expression))]
        [else 
        `(,target-expression)])]))

(define (walk substitutions target)
  (binary-search 
    (list->vector substitutions) 
    substitution-compare 
    `(,target '? '?)))

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
)