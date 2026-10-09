(library (scheme-langserver analysis identifier expanders dispatch-matcher)
  (export
    convert-pattern
    dsl+index-node->bindings
    gather-context)
  (import
    (chezscheme)
    (scheme-langserver analysis identifier expanders pattern)
    (scheme-langserver virtual-file-system index-node))

;Chez-native pattern machinery (TODO-12): a transcription of Chez Scheme's s/syntax.ss convert-pattern (L6448-6486) and the $syntax-dispatch matcher
;family (L7393-7554), operating on this project's index-node trees instead of Chez syntax objects. The algorithm shape is kept identical to upstream
;so future divergence checks are a diff against s/syntax.ss. Literal identifiers compare by symbol equality at this level (no wrap/hygiene),
;matching what the retired matcher did.
;Evaluation-order note (same as Chez): pattern variables accumulate in reverse occurrence order in BOTH pvars (convert-pattern, which walks cdr
;before car) and the matcher's accumulator (which recurses into cdr first), so the two zip aligned.

;pattern datum + literal keys -> (values dsl pvars) dsl ::= _ | any | (dsl . dsl)                      pair tree down to a non-pair tail
;| #(each <dsl>)                    p ... | #(each+ <dsl> (<dsl> ...) <dsl>) x ... y ... . z | #(free-id <key>) | #(atom <object>) | #(vector <dsl>)
(define (convert-pattern pattern keys)
  (define cvt
    (lambda (p n ids)
      (cond
        [(symbol? p)
          (cond
            [(memq p keys)
              (values (vector 'free-id p) ids)]
            [(eq? p '_) (values '_ ids)]
            [else (values 'any (cons (cons p n) ids))])]
        [(pair? p)
          (cond
            ;(x dots . proper-end-of-two) i.e. exactly (x dots)
            [(and (pair? (cdr p)) (null? (cddr p)) (eq? '... (cadr p)))
              (let-values ([(q ids) (cvt (car p) (+ n 1) ids)])
                (values (if (eq? q 'any) 'each-any (vector 'each q)) ids))]
            ;(x dots y ... . z): one ellipsized head, fixed middle, tail
            [(and (pair? (cdr p)) (eq? '... (cadr p)))
              (let*-values
                ([(z ids) (cvt (improper-tail (cddr p)) n ids)]
                 [(ys ids)
                   (let loop ([rest (cddr p)] [acc '()] [ids ids])
                     (if (or (not (pair? rest)) (eq? '... (car rest)))
                       (values (reverse acc) ids)
                       (let-values ([(y ids) (cvt (car rest) n ids)])
                         (loop (cdr rest) (cons y acc) ids))))]
                 [(x ids) (cvt (car p) (+ n 1) ids)])
                (values (vector 'each+ x ys z) ids))]
            [else
              (let*-values ([(y ids) (cvt (cdr p) n ids)]
                            [(x ids) (cvt (car p) n ids)])
                (values (cons x y) ids))])]
        [(null? p) (values '() ids)]
        [(vector? p)
          (let-values ([(q ids) (cvt (vector->list p) n ids)])
            (values (vector 'vector q) ids))]
        [else (values (vector 'atom p) ids)])))
  (cvt pattern 0 '()))

(define (improper-tail p)
  (let loop ([rest p])
    (cond
      [(not (pair? rest)) rest]
      [(eq? '... (car rest)) (loop (cdr rest))]
      [else (loop (cdr rest))])))

;;--------------------------------------------------------------------------- matcher

(define private:node-datum
  (lambda (node)
    (guard (e [else '()])
      (annotation-stripped (index-node-datum/annotations node)))))

;this project's index-node trees represent quoted forms ('x and (quote x)) with datum (quote x) but WITHOUT the quote head among the children, so a
;(quote p) pattern cannot match pairwise the way Chez's syntax objects do. Mirror of pattern.sls's match-list-like special case: when quote is a
;declared literal and the pattern is exactly (quote <p>), bind <p> against a synthesized content node instead. The literals list is fixed per macro, so
;a parameter avoids threading it through every matcher function.
(define private:literals (make-parameter '()))

;match one node against dsl, accumulating 'any values in r
(define private:match
  (lambda (node p r)
    (cond
      [(not r) #f]
      [(eq? p '_) r]
      [(eq? p 'any) (cons node r)]
      [(pair? p)
       (if (and (private:quote-pattern? p) (quoted-node? node))
         (private:match (quote-content-node node) (cadr p) r)
         (private:match-children (index-node-children node) p r))]
      [(eq? p 'each-any)
       (let ([l (private:match-each-any (index-node-children node) '())])
         (and l (cons l r)))]
      [(null? p) (and (null? (index-node-children node)) r)]
      [else
        (case (vector-ref p 0)
          [(each)
            (let ([children (index-node-children node)])
              (if (null? children)
                (private:match-empty (vector-ref p 1) r)
                (let ([r* (private:match-each children (vector-ref p 1) '())])
                  (and r* (private:combine r* r)))))]
          [(each-any)
            (let ([l (private:match-each-any (index-node-children node) '())])
              (and l (cons l r)))]
          [(each+)
            (let-values ([(xr* y-pat r)
                (private:match-each+ (index-node-children node) (vector-ref p 1)
                  (vector-ref p 2) (vector-ref p 3) r)])
              (and r (null? y-pat)
                (if (null? xr*)
                  (private:match-empty (vector-ref p 1) r)
                  (private:combine xr* r))))]
          [(free-id)
            (and (symbol? (private:node-datum node))
              (eq? (private:node-datum node) (vector-ref p 1))
              r)]
          [(atom)
            (and (equal? (vector-ref p 1) (private:node-datum node)) r)]
          [(vector)
            (private:match-children (index-node-children node) (vector-ref p 1) r)]
          [else #f])]))
)

;walk a dsl pair tree against a list of child nodes; the non-pair tail of the dsl (each/each+/any/atom/...) applies to the remaining children
(define private:match-children
  (lambda (children p r)
    (cond
      [(pair? p)
        (and (pair? children)
          (private:match (car children) (car p)
            (private:match-children (cdr children) (cdr p) r)))]
      [(null? p) (and (null? children) r)]
      [else (private:match-dsl-tail children p r)])))

;does this dsl pair tree denote a (quote <p>) pattern with quote declared literal and exactly one tail pattern?
(define private:quote-pattern?
  (lambda (p)
    (let ([head (car p)])
      (and (vector? head)
        (eq? 'free-id (vector-ref head 0))
        (eq? 'quote (vector-ref head 1))
        (memq 'quote (private:literals))
        (pair? (cdr p))
        (null? (cddr p))))))

;apply a non-pair dsl to a LIST of remaining nodes (a proper-list tail). each/each-any/each+ consume the list; any binds it; a vector dsl cannot
;match a list tail.
(define private:match-dsl-tail
  (lambda (nodes p r)
    (cond
      [(eq? p 'any) (cons nodes r)]
      [(eq? p '_) r]
      [(eq? p 'each-any)
        (let ([l (private:match-each-any nodes '())])
          (and l (cons l r)))]
      [(not (vector? p)) #f]
      [else
      (case (vector-ref p 0)
      [(each)
        (if (null? nodes)
          (private:match-empty (vector-ref p 1) r)
          (let ([r* (private:match-each nodes (vector-ref p 1) '())])
            (and r* (private:combine r* r))))]
      [(each-any)
        (let ([l (private:match-each-any nodes '())])
          (and l (cons l r)))]
      [(each+)
        (let-values ([(xr* y-pat r)
            (private:match-each+ nodes (vector-ref p 1)
              (vector-ref p 2) (vector-ref p 3) r)])
          (and r (null? y-pat)
            (if (null? xr*)
              (private:match-empty (vector-ref p 1) r)
              (private:combine xr* r))))]
      [(free-id)
        (and (null? nodes) r (private:match-empty p r))]
      [(atom)
        (and (null? nodes) (equal? (vector-ref p 1) '()) r)]
      [else #f])])))

;Chez match-each: every child matches p; collect per-iteration value lists
(define private:match-each
  (lambda (nodes p r)
    (cond
      [(null? nodes) (reverse r)]
      [else
        (let ([first (private:match (car nodes) p '())])
          (and first
            (private:match-each (cdr nodes) p (cons first r))))])))

(define private:match-each-any
  (lambda (nodes r)
    (cond
      [(null? nodes) (reverse r)]
      [else (private:match-each-any (cdr nodes) (cons (car nodes) r))])))

;Chez match-each+: recurse to the right end first; the fixed y patterns consume the rightmost nodes one each, x collects the rest, and the
;remaining improper tail goes to z (Chez s/syntax.ss L7436-7453)
(define private:match-each+
  (lambda (nodes x-pat y-pats z-pat r)
    (let f ([nodes nodes] [r r])
      (cond
        [(pair? nodes)
         (let-values ([(xr* y-pat r) (f (cdr nodes) r)])
           (if r
             (if (null? y-pat)
               (let ([xr (private:match (car nodes) x-pat '())])
                 (if xr
                   (values (cons xr xr*) y-pat r)
                   (values #f #f #f)))
               (values '() (cdr y-pat) (private:match (car nodes) (car y-pat) r)))
             (values #f #f #f)))]
        [else (values '() y-pats (private:match-tail-z nodes z-pat r))]))))

(define private:match-tail-z
  (lambda (nodes z-pat r)
    (cond
      [(eq? z-pat 'any) (cons nodes r)]
      [(eq? z-pat '_) r]
      [(eq? z-pat 'each-any)
       (let ([l (private:match-each-any nodes '())])
         (and l (cons l r)))]
      [(null? z-pat) (and (null? nodes) r)]
      [(pair? z-pat) (private:match-children nodes z-pat r)]
      [(vector? z-pat)
        (case (vector-ref z-pat 0)
          [(free-id)
            (and (null? nodes) r (private:match-empty z-pat r))]
          [(atom)
            (and (null? nodes) (equal? (vector-ref z-pat 1) '()) r)]
          [else #f])]
      [else #f])))

(define private:match-empty
  (lambda (p r)
    (cond
      [(null? p) r]
      [(eq? p '_) r]
      [(eq? p 'any) (cons '() r)]
      [(pair? p)
        (private:match-empty (car p) (private:match-empty (cdr p) r))]
      [(eq? p 'each-any) (cons '() r)]
      [else
        (case (vector-ref p 0)
          [(each) (private:match-empty (vector-ref p 1) r)]
          [(each+)
            (private:match-empty (vector-ref p 1)
              (private:match-empty (reverse (vector-ref p 2))
                (private:match-empty (vector-ref p 3) r)))]
          [(free-id atom) r]
          [(vector) (private:match-empty (vector-ref p 1) r)]
          [else r])])))

;transpose per-iteration value lists into per-'any nested lists
(define private:combine
  (lambda (r* r)
    (if (null? (car r*))
      r
      (cons (map car r*) (private:combine (map cdr r*) r)))))

;template-side ellipsis context, derived from the MATCH pattern datum directly (names live in the datum, not the name-erased dsl): an alist
;of (variable . #t) for variables at the terminal of a pair's cdr chain -- the improper-tail binder, cf. the old pattern record's cdr? flag,
;which make-pattern set on the last child of every pair form. Variables absent from the alist behave as #f. expand->index-node-compound-list's
;bindings-product uses the flag to decide whether an ellipsed value list may be distributed element-wise (proper position) or must stay whole
;(cdr position containing call-site nodes).
(define (gather-context pattern-datum keys)
  (define (walk-tail rest)
    ;rest sits in a cdr position: a pair continues the chain (its car is a mid-list element, not a terminal); a non-pair is the terminal.
    (cond
      [(pair? rest) (append (walk (car rest) #f) (walk-tail (cdr rest)))]
      [(null? rest) '()]
      [else (walk rest #t)]))
  (define (walk p terminal?)
    (cond
      [(symbol? p)
        (if (or (memq p keys) (eq? p '_) (not terminal?))
          '()
          `((,p . #t)))]
      [(pair? p) (append (walk (car p) #f) (walk-tail (cdr p)))]
      [else '()]))
  (walk pattern-datum #f))

;entry: dsl + pvars + call-site index-node + literals -> bindings alist or #f
(define (dsl+index-node->bindings dsl pvars call-node literals)
  (parameterize ([private:literals literals])
    (let ([r (private:match call-node dsl '())])
      (and r
        (let loop ([vars pvars] [vals r] [acc '()])
          (cond
            [(null? vars) (reverse acc)]
            [(null? vals) #f]
            [else
              (loop (cdr vars) (cdr vals)
                (cons (cons (car (car vars)) (car vals)) acc))]))))))
)
