(library (scheme-langserver analysis type logic)
  (export 
    conde

    symbolo
    numbero
    absento

    ==
    =/=
    fresh
    run
    run*)
  (import (chezscheme))
;;; 28 November 02014 WEB
;;;
;;; * Fixed missing unquote before E in 'drop-Y-b/c-dup-var'
;;;
;;; * Updated 'rem-xx-from-d' to check against other constraints after
;;; unification, in order to remove redundant disequality constraints
;;; subsumed by absento constraints.

;;; newer version: Sept. 18 2013 (with eigens)
;;; Jason Hemann, Will Byrd, and Dan Friedman
;;; E = (e* . x*)*, where e* is a list of eigens and x* is a list of variables.
;;; Each e in e* is checked for any of its eigens be in any of its x*.  Then it fails.
;;; Since eigen-occurs-check is chasing variables, we might as will do a memq instead
;;; of an eq? when an eigen is found through a chain of walks.  See eigen-occurs-check.
;;; All the e* must be the eigens created as part of a single eigen.  The reifier just
;;; abandons E, if it succeeds.  If there is no failure by then, there were no eigen
;;; violations.

;c->constraint
;return an empty constraint store
; Constraint store object.
; Mapping of representative variable to constraint record. Constraints
; are always on the representative element and must be moved / merged
; when that element changes.
(define empty-c '(() () () () () () ()))

;Suppose a dynamic window with width 7
;It's corresponding with emtpy-c
(define c->B (lambda (c) (car c)))
(define c->E (lambda (c) (cadr c)))
;S->list 0f substitutions
(define c->S (lambda (c) (caddr c)))
; D->list of disequality constraints
(define c->D (lambda (c) (cadddr c)))
(define c->Y (lambda (c) (cadddr (cdr c))))
(define c->N (lambda (c) (cadddr (cddr c))))
;T->type constraint
(define c->T (lambda (c) (cadddr (cdddr c))))

;3.3
;p52
;For minikanren, a goal g is a function that maps a substitution s to an ordered sequence of zero or more values
;these values are almost always substitutions
;To create this function g, notate lambda as lambdag@.
;N.B.: the argument a is different from lambdaf@
(define-syntax lambdag@
  (syntax-rules (:)
  ;binding c with something in e
    ((_ (c) e) (lambda (c) e))
    ;these clauses are different from the origin version
    ;B E S and c->B...they're capturing multiple values in c and attach
    ((_ (c : B E S) e)
     (lambda (c)
       (let ((B (c->B c)) (E (c->E c)) (S (c->S c)))
         e)))
    ((_ (c : B E S D Y N T) e)
     (lambda (c)
       (let ((B (c->B c)) (E (c->E c)) (S (c->S c)) (D (c->D c))
	     (Y (c->Y c)) (N (c->N c)) (T (c->T c)))
         e)))))

;3.1 
;p47
;in substitution ((y 5) (x y))
; this is called triangular substitution, 
; advantage: extension remains a constant-time operation
; disadvantage: variable lookup is expensive
; chapter 4 explore serveral ways to improve the efficency

; Association object.
; Describes an association mapping the lhs to the rhs. Returned by
; unification to describe the associations that were added to the
; substitution (whose representation is opaque) and used to represent
; disequality constraints.
;left hand side
(define lhs car)
;right hand side
(define rhs cdr)

(define eigen-tag (vector 'eigen-tag))

(define (eigen-var) (vector eigen-tag))


(define eigen?
  (lambda (x)
    (and (vector? x) (eq? (vector-ref x 0) eigen-tag))))

;p46 3.1
; var is for logic variable
(define (var dummy) (vector dummy))

;p46 3.1
(define (var? x)
  ;eigen-tag is specific vector
  ; a var is not a eigen-tag
    (and (vector? x) (not (eq? (vector-ref x 0) eigen-tag))))

;3.1 
;p48
;looking up a variable in a triangular substitution
;v->logic variable
;s->substitution
; (define trees '((pine . cones) (oak . acorns) (maple . seeds)))
;      ⇒ ((pine . cones) (oak . acorns) (maple . seeds))
; (assq 'pine trees)
;      ⇒ (pine . cones)
;e.g.(p51): (walk 'x ((z 6) (y 5) (x (y z)))) -> (y z)
;N.B.: It's holy the wholy same to origin version
;return a procedure contain origin S (wrapped with lambda to keep context), this means with respect to S and substitute with new pr.
; Term, Substitution -> Term
(define (walk logic-variable substitution-list)
    (cond
      ((and (var? logic-variable) (assq logic-variable substitution-list)) =>
       (lambda (pr) (walk (rhs pr) substitution-list)))
      (else logic-variable)))

;8.3
;p96
;return a mini-substitution c containing only the new associations added during unification
(define prefix-S
  (lambda (S+ S)
    (cond
      ((eq? S+ S) '())
      (else (cons (car S+)
              (prefix-S (cdr S+) S))))))

;3.1 
;p49
;p50
;N.B.: Unification is a process of making two different logical atomic expressions identical by finding a substitution
;unifies term u and term v (these two may be logic variable, value, or even substitution) 
;with respect to a substitution s, and return a potentially extended substitution if succeeds, 
;and return #f if fails or would introduce a circularity
; Term, Term, Substitution -> UnificationResult
;x->logic variable
;v->value or other logic variable
;s->substitution, and is not the whole list of substitutions(this is S)
(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        ((or (eigen? u) (eigen? v)) #f)
        ((equal? u v) s)
        (else #f)))))

; wholy same
;check if x v are exist in s.
;x->logic variable
;v-> something
;s->substitution
(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

(define eigen-occurs-check
  (lambda (e* x s)
    (let ((x (walk x s)))
      (cond
        ((var? x) #f)
        ((eigen? x) (memq x e*))
        ((pair? x)
         (or
           (eigen-occurs-check e* (car x) s)
           (eigen-occurs-check e* (cdr x) s)))
        (else #f)))))

(define empty-f (lambda () (mzero)))

;3.1
;p46
;extend substitution s with check
;wholy same
(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (cons `(,x . ,v) s)))))

(define unify*
  (lambda (S+ S)
    (unify (map lhs S+) (map rhs S+) S)))

;3.3
;p53
;Its goal is to distinguish mzero, unit, choice, inc

; SearchStream: #f | SuspendedStream | State | (Pair State SuspendedStream)
; SuspendedStream: (-> SearchStream)

; Match on search streams. The State type must not be a pair with a procedure
; in its cdr, lest a single result be interpreted as multiple results.
;
; (() e0)     failure
; ((f^) e1)    suspension for interleaving. separate from success or failure to ensure
;              it goes all the way to the top of the tree.
;              here f^ is just to distinguish thie clause with the first one 
;              also here, f^ can be bind with c-fin and used in e1
; ((c) e2)    single result. Used rather than (cons c (lambda () #f))
;              to avoid returning to search a part of the tree that
;              will inevitably fail.
; ((c f) e3)  multiple results. `f` is a thunk to avoid unnecessary computation
;              in the case that the LHS the last answer needed to satisfy the
;              query. It also triggers interleaving; the search looks for
;              answers in alternate branches before returning.
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ([stream e])
       (cond
       ;mzero->#f
         ((not stream) e0)
       ;unit->(lambda(c) c)
         ((procedure? stream)  (let ((f^ stream)) e1))
        ;choice->cons
         ((not (and (pair? stream)
                 (procedure? (cdr stream))))
          (let ((c^ stream)) e2))
          ;inc->closure
         (else (let ([c (car stream)] [f (cdr stream)])
                 e3)))))))

; (fresh (x:id ...) g:Goal ...+) -> Goal
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (c : B E S D Y N T)
       (lambda ()
         (let ([x (var 'x)] ...)
           (let ((B (append `(,x ...) B)))
             (bind* (g0 `(,B ,E ,S ,D ,Y ,N ,T)) g ...))))))))

(define-syntax eigen
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (c : B E S)
       (let ((x (eigen-var)) ...)
         ((fresh () (eigen-absento `(,x ...) B) g0 g ...) c))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

;3.3
;p55
;bind combines succsessive goals
; SearchStream, Goal -> SearchStream
;g->goal
(define (bind stream goal)
    (case-inf stream 
      (() (mzero))
      ;apply current stream to g
      ((f) (lambda () (bind (f) goal)))
      ((c) (goal c))
      ((c f) (mplus (goal c) (lambda () (bind (f) goal))))))

;3.3
; p56
;transform stream to list
;N.B.: It replace exist with fresh comparing to origin version 
;and is significantly different
(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
       (lambda ()
       ;N.B.: here write a procedure-application like (proc empty-c)
         ((fresh (q) g0 g ...
            (lambdag@ (final-c)
              (let ((z ((reify q) final-c)))
                (cons z empty-f))))
          empty-c))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x) (fresh (q0 q1 q ...) g0 g ... (== `(,q0 ,q1 ,q ...) x))))))

;the same to origin
(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

;not same to origin
; Int, SuspendedStream -> (ListOf SearchResult)
(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ; origin:((a) a) 
         ((c) (cons c '()))
         ;origin:((a f)
        ;  (cons (car a)
        ;    (take (and n (- n 1)) f)))
        ;Clearly, origin value stream is a list like (a (f (f ..)))
        ;Now, contrant is a list of (old-contrant f f f f)
         ((c f) (cons c
                  (take (and n (- n 1)) f))))))))

;3.3
;p54
;conde is a goal constructor that combines successive conde clauses using mplus∗ .
;and all of the stream values will be interleaved
; (conde [g:Goal ...] ...+) -> Goal
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (lambda ()
         (mplus*
           (bind* (g0 c) g ...)
           (bind* (g1 c) g^ ...) 
           ...))))))
;3.3
;p54
;append new things to stream
; a different append
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                    (lambda () (mplus* e ...))))))

;3.3
;p54
; SearchStream, SuspendedStream -> SearchStream,
; f is a thunk to avoid unnecesarry computation in the case that the
; first answer produced by c-inf is enough to satisfy the query.
;here, f^ means following stream
;c means current stream
;we can find that: a stream is consisted with many closure
; and all closure is to extend current stream by adding new f to the head
(define (mplus stream f)
    (case-inf stream 
      (() (f))
      ((f^) (lambda () (mplus (f) f^)))
      ;choice is acturally cons
      ; extend current stream c-inf
      ((c) (cons c f))
      ;extend and continue result?
      ;with respect to case-inf, origin contiuation is (car cadr)
      ;in this clause, will be (car (cadr caddr)...)
      ((c f^) (cons c (lambda () (mplus (f) f^))))))


;3.4
; p58
;stream returned by the first goal is passed to bind*
(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (lambda ()
         (ifa ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))
;the same
(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifa b ...))
         ((f) (lambda () (loop (f))))
         ((a) (bind* c-inf g ...))
         ((a f) (bind* c-inf g ...)))))))

;3.4
; p58
;a singletion stream is passed to bind*
;this stream contains the first value of stream returned by the first goal
(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (lambda ()
         (ifu ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))
;the same
(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifu b ...))
         ((f) (lambda () (loop (f))))
         ((c) (bind* c-inf g ...))
         ((c f) (bind* (unit c) g ...)))))))
;3.3
;p52
;e.g.:(mzero)->empty stream of values
(define mzero (lambda () #f))

;3.3
;p52
;e.g.:(unit a)->stream just contain a
(define unit (lambda (c) c))

(define tagged?
  (lambda (S Y y^)
    (exists (lambda (y) (eqv? (walk y S) y^)) Y)))

(define untyped-var?
  (lambda (S Y N t^)
    (let ((in-type? (lambda (y) (eq? (walk y S) t^))))
      (and (var? t^)
           (not (exists in-type? Y))
           (not (exists in-type? N))))))

;3.4
; p57
; can be used to access the values of variables
(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (c : B E S)
       (let ((x (walk* x S)) ...)
         ((fresh () g g* ...) c))))))

;3.2
;p51
;e.g.(p51): (walk* 'x ((z 6) (y 5) (x (y z)))) -> (5 6)
;the same
(define walk*
  (lambda (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) S) (walk* (cdr v) S)))
        (else v)))))

;3.2
;p51
;v-> walk*-ed term
;s->starts out as empty-s
;return a reifyed name substitution, associating logic variables to distinct symbols of the form _.n
(define reify-S
  (lambda  (v S)
    (let ((v (walk v S)))
      (cond
        ((var? v)
         (let ((n (length S)))
           (let ((name (reify-name n)))
             (cons `(,v . ,name) S))))
        ((pair? v)
         (let ((S (reify-S (car v) S)))
           (reify-S (cdr v) S)))
        (else S)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define drop-dot
  (lambda (X)
    (map (lambda (t)
           (let ((a (lhs t))
                 (d (rhs t)))
             `(,a ,d)))
         X)))

(define sorter
  (lambda (ls)
    (list-sort lex<=? ls)))

(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

(define anyvar?
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyvar? (car u) r)
           (anyvar? (cdr u) r)))
      (else (var? (walk u r))))))

(define anyeigen?
  (lambda (u r)
    (cond
      ((pair? u)
       (or (anyeigen? (car u) r)
           (anyeigen? (cdr u) r)))
      (else (eigen? (walk u r))))))

(define member*
  (lambda (u v)
    (cond
      ((equal? u v) #t)
      ((pair? v)
       (or (member* u (car v)) (member* u (cdr v))))
      (else #f))))

;;;

(define drop-N-b/c-const
  (lambdag@ (c : B E S D Y N T)
    (let ((const? (lambda (n)
                    (not (var? (walk n S))))))
      (cond
        ((find const? N) =>
         (lambda (n) `(,B ,E ,S ,D ,Y ,(remq1 n N) ,T)))
        (else c)))))

(define drop-Y-b/c-const
  (lambdag@ (c : B E S D Y N T)
    (let ((const? (lambda (y)
                    (not (var? (walk y S))))))
      (cond
	((find const? Y) =>
         (lambda (y) `(,B ,E ,S ,D ,(remq1 y Y) ,N ,T)))
        (else c)))))

(define remq1
  (lambda (elem ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) elem) (cdr ls))
      (else (cons (car ls) (remq1 elem (cdr ls)))))))

(define same-var?
  (lambda (v)
    (lambda (v^)
      (and (var? v) (var? v^) (eq? v v^)))))

(define find-dup
  (lambda (f S)
    (lambda (set)
      (let loop ((set^ set))
        (cond
          ((null? set^) #f)
          (else
           (let ((elem (car set^)))
             (let ((elem^ (walk elem S)))
               (cond
                 ((find (lambda (elem^^)
                          ((f elem^) (walk elem^^ S)))
                        (cdr set^))
                  elem)
                 (else (loop (cdr set^))))))))))))

(define drop-N-b/c-dup-var
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-dup same-var? S) N) =>
       (lambda (n) `(,B ,E ,S ,D ,Y ,(remq1 n N) ,T)))
      (else c))))

(define drop-Y-b/c-dup-var
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-dup same-var? S) Y) =>
       (lambda (y)
         `(,B ,E ,S ,D ,(remq1 y Y) ,N ,T)))
      (else c))))

(define var-type-mismatch?
  (lambda (S Y N t1^ t2^)
    (cond
      ((num? S N t1^) (not (num? S N t2^)))
      ((sym? S Y t1^) (not (sym? S Y t2^)))
      (else #f))))

(define term-ununifiable?
  (lambda (S Y N t1 t2)
    (let ((t1^ (walk t1 S))
          (t2^ (walk t2 S)))
      (cond
        ((or (untyped-var? S Y N t1^) (untyped-var? S Y N t2^)) #f)
        ((var? t1^) (var-type-mismatch? S Y N t1^ t2^))
        ((var? t2^) (var-type-mismatch? S Y N t2^ t1^))
        ((and (pair? t1^) (pair? t2^))
         (or (term-ununifiable? S Y N (car t1^) (car t2^))
             (term-ununifiable? S Y N (cdr t1^) (cdr t2^))))
        (else (not (eqv? t1^ t2^)))))))

(define T-term-ununifiable?
  (lambda (S Y N)
    (lambda (t1)
      (let ((t1^ (walk t1 S)))
        (letrec
            ((t2-check
              (lambda (t2)
                (let ((t2^ (walk t2 S)))
                  (cond
                    ((pair? t2^) (and
                                  (term-ununifiable? S Y N t1^ t2^)
                                  (t2-check (car t2^))
                                  (t2-check (cdr t2^))))
                    (else (term-ununifiable? S Y N t1^ t2^)))))))
          t2-check)))))

(define num?
  (lambda (S N n)
    (let ((n (walk n S)))
      (cond
        ((var? n) (tagged? S N n))
        (else (number? n))))))

(define sym?
  (lambda (S Y y)
    (let ((y (walk y S)))
      (cond
        ((var? y) (tagged? S Y y))
        (else (symbol? y))))))

(define drop-T-b/c-Y-and-N
  (lambdag@ (c : B E S D Y N T)
    (let ((drop-t? (T-term-ununifiable? S Y N)))
      (cond
        ((find (lambda (t) ((drop-t? (lhs t)) (rhs t))) T) =>
         (lambda (t) `(,B ,E ,S ,D ,Y ,N ,(remq1 t T))))
        (else c)))))

(define move-T-to-D-b/c-t2-atom
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((exists (lambda (t)
               (let ((t2^ (walk (rhs t) S)))
                 (cond
                   ((and (not (untyped-var? S Y N t2^))
                         (not (pair? t2^)))
                    (let ((T (remq1 t T)))
                      `(,B ,E ,S ((,t) . ,D) ,Y ,N ,T)))
                   (else #f))))
             T))
      (else c))))

(define terms-pairwise=?
  (lambda (pr-a^ pr-d^ t-a^ t-d^ S)
    (or
     (and (term=? pr-a^ t-a^ S)
          (term=? pr-d^ t-a^ S))
     (and (term=? pr-a^ t-d^ S)
          (term=? pr-d^ t-a^ S)))))

(define T-superfluous-pr?
  (lambda (S Y N T)
    (lambda (pr)
      (let ((pr-a^ (walk (lhs pr) S))
            (pr-d^ (walk (rhs pr) S)))
        (cond
          ((exists
               (lambda (t)
                 (let ((t-a^ (walk (lhs t) S))
                       (t-d^ (walk (rhs t) S)))
                   (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S)))
             T)
           (for-all
            (lambda (t)
              (let ((t-a^ (walk (lhs t) S))
                    (t-d^ (walk (rhs t) S)))
                (or
                 (not (terms-pairwise=? pr-a^ pr-d^ t-a^ t-d^ S))
                 (untyped-var? S Y N t-d^)
                 (pair? t-d^))))
            T))
          (else #f))))))

(define drop-from-D-b/c-T
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((find
           (lambda (d)
             (exists
                 (T-superfluous-pr? S Y N T)
               d))
         D) =>
         (lambda (d) `(,B ,E ,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define drop-t-b/c-t2-occurs-t1
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((find (lambda (t)
               (let ((t-a^ (walk (lhs t) S))
                     (t-d^ (walk (rhs t) S)))
                 (mem-check t-d^ t-a^ S)))
             T) =>
             (lambda (t)
               `(,B ,E ,S ,D ,Y ,N ,(remq1 t T))))
      (else c))))

(define split-t-move-to-d-b/c-pair
  (lambdag@ (c : B E S D Y N T)
    (cond
      ((exists
         (lambda (t)
           (let ((t2^ (walk (rhs t) S)))
             (cond
               ((pair? t2^) (let ((ta `(,(lhs t) . ,(car t2^)))
                                  (td `(,(lhs t) . ,(cdr t2^))))
                              (let ((T `(,ta ,td . ,(remq1 t T))))
                                `(,B ,E ,S ((,t) . ,D) ,Y ,N ,T))))
               (else #f))))
         T))
      (else c))))

(define find-d-conflict
  (lambda (S Y N)
    (lambda (D)
      (find
       (lambda (d)
	 (exists (lambda (pr)
		   (term-ununifiable? S Y N (lhs pr) (rhs pr)))
		 d))
       D))))

(define drop-D-b/c-Y-or-N
  (lambdag@ (c : B E S D Y N T)
    (cond
      (((find-d-conflict S Y N) D) =>
       (lambda (d) `(,B ,E ,S ,(remq1 d D) ,Y ,N ,T)))
      (else c))))

(define cycle
  (lambdag@ (c)
    (let loop ((c^ c)
               (fns^ (LOF))
               (n (length (LOF))))
      (cond
        ((zero? n) c^)
        ((null? fns^) (loop c^ (LOF) n))
        (else
         (let ((c^^ ((car fns^) c^)))
           (cond
             ((not (eq? c^^ c^))
              (loop c^^ (cdr fns^) (length (LOF))))
             (else (loop c^ (cdr fns^) (sub1 n))))))))))

(define absento
  (lambda (u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((mem-check u v S) (mzero))
        (else (unit `(,B ,E ,S ,D ,Y ,N ((,u . ,v) . ,T))))))))

(define eigen-absento
  (lambda (e* x*)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((eigen-occurs-check e* x* S) (mzero))
        (else (unit `(,B ((,e* . ,x*) . ,E) ,S ,D ,Y ,N ,T)))))))

(define mem-check
  (lambda (u t S)
    (let ((t (walk t S)))
      (cond
        ((pair? t)
         (or (term=? u t S)
             (mem-check u (car t) S)
             (mem-check u (cdr t) S)))
        (else (term=? u t S))))))

(define term=?
  (lambda (u t S)
    (cond
      ((unify u t S) =>
       (lambda (S0)
         (eq? S0 S)))
      (else #f))))

(define ground-non-<type>?
  (lambda (pred)
    (lambda (u S)
      (let ((u (walk u S)))
        (cond
          ((var? u) #f)
          (else (not (pred u))))))))
;; moved
(define ground-non-symbol?
  (ground-non-<type>? symbol?))

(define ground-non-number?
  (ground-non-<type>? number?))

(define symbolo
  (lambda (u)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((ground-non-symbol? u S) (mzero))
        ((mem-check u N S) (mzero))
        (else (unit `(,B ,E ,S ,D (,u . ,Y) ,N ,T)))))))

(define numbero
  (lambda (u)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((ground-non-number? u S) (mzero))
        ((mem-check u Y S) (mzero))
        (else (unit `(,B ,E ,S ,D ,Y (,u . ,N) ,T)))))))
;; end moved

;8.2
;p113
;8.3
;p117
(define =/= ;; moved
  (lambda (u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (let ((pfx (prefix-S S0 S)))
             (cond
               ((null? pfx) (mzero))
               (else (unit `(,B ,E ,S (,pfx . ,D) ,Y ,N ,T)))))))
        (else c)))))
;8.2
;p113
;origin version is 3.3 p53
(define (== u v)
    (lambdag@ (c : B E S D Y N T)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (cond
             ((==fail-check B E S0 D Y N T) (mzero))
             (else (unit `(,B ,E ,S0 ,D ,Y ,N ,T))))))
        (else (mzero)))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define ==fail-check
  (lambda (B E S0 D Y N T)
    (cond
      ((eigen-absento-fail-check E S0) #t)
      ((atomic-fail-check S0 Y ground-non-symbol?) #t)
      ((atomic-fail-check S0 N ground-non-number?) #t)
      ((symbolo-numbero-fail-check S0 Y N) #t)
      ((=/=-fail-check S0 D) #t)
      ((absento-fail-check S0 T) #t)
      (else #f))))

(define eigen-absento-fail-check
  (lambda (E S0)
    (exists (lambda (e*/x*) (eigen-occurs-check (car e*/x*) (cdr e*/x*) S0)) E)))

(define atomic-fail-check
  (lambda (S A pred)
    (exists (lambda (a) (pred (walk a S) S)) A)))

(define symbolo-numbero-fail-check
  (lambda (S A N)
    (let ((N (map (lambda (n) (walk n S)) N)))
      (exists (lambda (a) (exists (same-var? (walk a S)) N))
        A))))

(define absento-fail-check
  (lambda (S T)
    (exists (lambda (t) (mem-check (lhs t) (rhs t) S)) T)))

(define =/=-fail-check
  (lambda (S D)
    (exists (d-fail-check S) D)))

(define d-fail-check
  (lambda (S)
    (lambda (d)
      (cond
        ((unify* d S) =>
	 (lambda (S+) (eq? S+ S)))
        (else #f)))))

; Reification
; S - substitution
; T - type constraints
; A - absento constriants
; D - disequality constraints
;3.2
;p51
;v-> arbitrary value (perhaps containing variables)
;s-> substitution
;return reified value of v
;e.g.:(reify (5 x (#t y x) z) empty-s) -> ( 5 _.0 ( #t _.1 _.0 ) _.2 )
(define reify
  (lambda (x)
  ;here c is the stream
    (lambda (c)
      (let ((c (cycle c)))
        (let* ((S (c->S c))
             (D (walk* (c->D c) S))
             (Y (walk* (c->Y c) S))
             (N (walk* (c->N c) S))
             (T (walk* (c->T c) S)))
        (let ((v (walk* x S)))
          (let ((R (reify-S v '())))
            (reify+ v R
                    (let ((D (remp
                              (lambda (d)
                                (let ((dw (walk* d S)))
                                  (or
                                    (anyvar? dw R)
                                    (anyeigen? dw R))))
                               (rem-xx-from-d c))))
                      (rem-subsumed D))
                    (remp
                     (lambda (y) (var? (walk y R)))
                     Y)
                    (remp
                     (lambda (n) (var? (walk n R)))
                     N)
                    (remp (lambda (t)
                            (or (anyeigen? t R) (anyvar? t R))) T)))))))))

(define reify+
  (lambda (v R D Y N T)
    (form (walk* v R)
          (walk* D R)
          (walk* Y R)
          (walk* N R)
          (rem-subsumed-T (walk* T R)))))

(define form
  (lambda (v D Y N T)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (ft (sorter T)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (ft (if (null? ft) ft
                    (let ((ft (drop-dot ft)))
                      `((absento . ,ft))))))
        (cond
          ((and (null? fd) (null? fy)
                (null? fn) (null? ft))
           v)
          (else (append `(,v) fd fn fy ft)))))))

(define sort-D
  (lambda (D)
    (sorter
     (map sort-d D))))

(define sort-d
  (lambda (d)
    (list-sort
       (lambda (x y)
         (lex<=? (car x) (car y)))
       (map sort-pr d))))

(define drop-dot-D
  (lambda (D)
    (map drop-dot D)))

(define lex<-reified-name?
  (lambda (r)
    (char<?
     (string-ref
      (datum->string r) 0)
     #\_)))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define rem-subsumed
  (lambda (D)
    (let rem-subsumed ((D D) (d^* '()))
      (cond
        ((null? D) d^*)
        ((or (subsumed? (car D) (cdr D))
             (subsumed? (car D) d^*))
         (rem-subsumed (cdr D) d^*))
        (else (rem-subsumed (cdr D)
                (cons (car D) d^*)))))))

(define subsumed?
  (lambda (d d*)
    (cond
      ((null? d*) #f)
      (else
        (let ((d^ (unify* (car d*) d)))
          (or
            (and d^ (eq? d^ d))
            (subsumed? d (cdr d*))))))))

(define rem-xx-from-d
  (lambdag@ (c : B E S D Y N T)
    (let ((D (walk* D S)))
      (remp not
            (map (lambda (d)
                   (cond
                     ((unify* d S) =>
                      (lambda (S0)
                        (cond
                          ((==fail-check B E S0 '() Y N T) #f)
                          (else (prefix-S S0 S)))))
                     (else #f)))
                 D)))))

(define rem-subsumed-T
  (lambda (T)
    (let rem-subsumed ((T T) (T^ '()))
      (cond
        ((null? T) T^)
        (else
         (let ((lit (lhs (car T)))
               (big (rhs (car T))))
           (cond
             ((or (subsumed-T? lit big (cdr T))
                  (subsumed-T? lit big T^))
              (rem-subsumed (cdr T) T^))
             (else (rem-subsumed (cdr T)
                     (cons (car T) T^))))))))))

(define subsumed-T?
  (lambda (lit big T)
    (cond
      ((null? T) #f)
      (else
       (let ((lit^ (lhs (car T)))
             (big^ (rhs (car T))))
         (or
           (and (eq? big big^) (member* lit^ lit))
           (subsumed-T? lit big (cdr T))))))))

(define LOF
  (lambda ()
    `(,drop-N-b/c-const ,drop-Y-b/c-const ,drop-Y-b/c-dup-var
      ,drop-N-b/c-dup-var ,drop-D-b/c-Y-or-N ,drop-T-b/c-Y-and-N
      ,move-T-to-D-b/c-t2-atom ,split-t-move-to-d-b/c-pair
      ,drop-from-D-b/c-T ,drop-t-b/c-t2-occurs-t1)))
)