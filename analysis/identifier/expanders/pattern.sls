(library (scheme-langserver analysis identifier expanders pattern)
  (export
    make-pattern
    pattern?
    pattern-type
    pattern-content
    pattern-children
    pattern-parent
    gather-context

    context:ellipsed?
    pattern+index-node->pair-list
    pattern->pairs->generator)
  (import 
    (chezscheme)
    (ufo-coroutines)
    (scheme-langserver util contain)
    (scheme-langserver util sub-list)

    (scheme-langserver virtual-file-system index-node))

;This is based on a rough idea that R6RS syntax-rules and syntax-case mainly consist of patterns and templates. 
;Considering with the incomplete code faced by scheme-langserver, it's necessarey to facilatate some fault tolerante features as followings:
;First, patterns are supposed to be strictly correct.
;Second, templates are not supposed to be strictly correct and this will give some tolerance. 
;Or in other words, though R6RS says the template is literalliy a pattern variable, here we can't just do so.

(define-record-type pattern 
  (fields 
;r6rs 11.19
;underscore/pattern-variable/literal-identifier/list-form/pair-form/ellipse-list-form/ellipse-pair-form/vector-form/ellipse-vector-form/equal?-datum
    (immutable type)
    (immutable content)
    (immutable children)
    (mutable parent))
  (protocol
    (lambda (new)
      (lambda (successed-matched-pattern-expression)
        (cond 
          [(equal? successed-matched-pattern-expression '_) (new 'underscore successed-matched-pattern-expression '() #f)]
          [(equal? successed-matched-pattern-expression '...) (new 'ellipse successed-matched-pattern-expression '() #f)]
          [(symbol? successed-matched-pattern-expression) (new 'pattern-variable/literal-identifier successed-matched-pattern-expression '() #f)]

          [(list? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (if (contain? successed-matched-pattern-expression '...) 'ellipse-list-form 'list-form) 
                    successed-matched-pattern-expression 
                    (map make-pattern successed-matched-pattern-expression)
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              p)]
          [(pair? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        (if (equal? '... (car rest)) 
                          'ellipse-pair-form
                          (loop (cdr rest)))
                        'pair-form))
                    successed-matched-pattern-expression
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        `(,(make-pattern (car rest)) . ,(loop (cdr rest)))
                        `(,(make-pattern rest))))
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              p)]
          [(vector? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (if (contain? (vector->list successed-matched-pattern-expression) '...) 'ellipse-vector-form 'vector-form) 
                    successed-matched-pattern-expression
                    (map make-pattern (vector->list successed-matched-pattern-expression))
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              p)]
          [else (new 'equal?-datum successed-matched-pattern-expression '() #f)])))))

; (define (fuzzy context pair-list) '())

; (define (pattern+template->generator pattern template)
;   (case (pattern-type template)
;     [underscore fuzzy]
;     [(list-form vector-form pair-form)
;       (lambda (context pair-list) 
;         (let ([r (map (lambda (child) ((pattern+template->generator child) context pair-list)) (pattern-children template))])
;           (case (pattern-type template)
;             [list-form r]
;             [vector-form (list->vector r)]
;             [pair-form `(,@(reverse (cdr (reverse r))) . ,(car (reverse r)))])))]
;     [ellipse-list-form 
;       (lambda (context pair-list) 
;       ;Here, we need to be ready for sequentially subtemplates processing.
;         (let* ([children-vector (list->vector (pattern-children template))]
;             [l (vector-length children-vector)])
;           (let loop ([i 0])
;             (cond 
;               [(< (+ 1 i) l)
;                 (if (equal? 'ellipse (pattern-type (vector-ref children-vector (+ 1 i))))
;                   ;process subtemplate
;                   `()
;                   `(,((pattern+template->generator pattern (vector-ref children-vector i)) context pair-list) . ,((loop (+ 1 i)) context pair-list)))]
;               [(< i l)
;                 `(,(pattern+template->generator pattern (vector-ref children-vector i)))]
;               [else fuzzy]))))]
;     [pattern-variable/literal-identifier 
;       (lambda (context pair-list)
;         (let ([target-pattern (assoc context (pattern-content template))])
;           (if target-pattern
;             (let ([result (find (lambda (p) (equal? (car target-pattern) p)) pair-list)])
;               (if result
;                 (cdr result)
;                 (pattern-content template)))
;             (pattern-content template))))]
;     [else fuzzy]))

; (define (private:subtemplate->generator ellipsed-pattern context pair-list)
;   (case (pattern-type ellipsed-pattern)
;     [pattern-variable/literal-identifier 
;       (lambda (context pair-list)
;         (let* ([target-pattern (assoc context (pattern-content template))])
;           (if target-pattern
;             (let ([result (find (lambda (p) (equal? (car target-pattern) p)) pair-list)])
;               (if result
;                 (cdr result)
;                 (pattern-content template)))
;             (pattern-content template))))
;     ]
;   )
; )

;suppose pattern-type is pattern-variable/literal-identifier 
(define (pattern->pairs->generator pattern)
  (if (recursive:pattern-ellipsed? pattern)
    (lambda (pair-list)
      (let* ([ancestor-vector (list->vector (private:ancestors pattern))]
          [pair-vector (list->vector pair-list)]
          [max-i (vector-length pair-vector)]
          [max-j (vector-length ancestor-vector)])
        (init-iterator
          (lambda (yield)
            (let loop ([i 0] [j 0] [yielded? #f] [backwarded? #f])
              (cond 
                [(= i max-i) '()]

                [(and 
                    (= j (- max-j 1))
                    (equal? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)))
                  (if (and yielded? backwarded?) (yield '...))
                  (yield (cdr (vector-ref pair-vector i)))
                  (loop (+ 1 i) j #t #f)]
                [(= j (- max-j 1)) (loop (+ 1 i) 0 yielded? #t)]

                [(equal? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)) (loop (+ 1 i) (+ 1 j) yielded? yielded?)]
                [(recursive:ancestor? (vector-ref ancestor-vector j) (car (vector-ref pair-vector i))) (loop i (+ 1 j) yielded? backwarded?)]

                [else (loop (+ 1 i) 0 yielded? #t)]))))))
    (lambda (pair-list)
      (let ([t (find (lambda (p) (equal? (car p) pattern)) pair-list)])
        (lambda () 
          (if t (cdr t) #f))))))

(define (private:ancestors pattern)
  (if (pattern? pattern)
    `(,@(private:ancestors (pattern-parent pattern)) ,pattern)
    '()))

(define (recursive:ancestor? ancestor child)
  (cond 
    [(not (pattern? ancestor)) #f]
    [(not (pattern? child)) #f]
    [(equal? ancestor (pattern-parent child)) #t]
    [else (recursive:ancestor? ancestor (pattern-parent child))]))

;the pattern must match the index-node.
(define (pattern+index-node->pair-list pattern index-node)
  `((,pattern . ,index-node) .
    ,(let ([p-c (pattern-children pattern)]
        [i-c (index-node-children index-node)])
      (case (pattern-type pattern)
        [(list-form vector-form pair-form)
          (let loop ([rest-patterns p-c] [rest-index-nodes i-c])
            (if (null? rest-patterns)
              '()
              `(,@(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)) . ,(loop (cdr rest-patterns) (cdr rest-index-nodes)))))]
        [(ellipse-list-form ellipse-vector-form ellipse-pair-form)
          (let loop (
              [rest-patterns (reverse p-c)] 
              [rest-index-nodes (reverse i-c)])
            (case (pattern-type (car rest-patterns))
              [ellipse 
                (let curr-loop ([head-patterns (reverse (cddr rest-patterns))]
                    [head-index-nodes (reverse rest-index-nodes)])
                  (if (null? head-patterns)
                    (apply append 
                      (map 
                        (lambda (i) (pattern+index-node->pair-list (cadr rest-patterns) i))
                        head-index-nodes))
                    `(,@(pattern+index-node->pair-list (car head-patterns) (car head-index-nodes)) . ,(curr-loop (cdr head-patterns) (cdr head-index-nodes)))))]
              [else 
                `(,@(loop (cdr rest-patterns) (cdr rest-index-nodes)) .
                  ,(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)))]))]
        [else '()]))))

(define (gather-context pattern)
  (case (pattern-type pattern)
    [pattern-variable/literal-identifier `((,(pattern-content pattern) . ,pattern))]
    [(ellipse-list-form list-form ellipse-vector-form vector-form ellipse-pair-form pair-form) (apply append (map gather-context (pattern-children pattern)))]
    [else '()]))

(define (context:ellipsed? context pattern-variable/literal-identifier)
  (let* ([v-p (assoc pattern-variable/literal-identifier context)]
      [p (cdr v-p)])
    (recursive:pattern-ellipsed? p)))

(define (recursive:pattern-ellipsed? pattern)
  (cond 
    [(not pattern) #f]
    [(not (pattern-parent pattern)) #f]
    [(private:pattern-ellipsed? pattern) #t]
    [else (recursive:pattern-ellipsed? (pattern-parent pattern))]))

(define (private:count-ellipse-level pattern)
  (cond 
    [(not pattern) 0]
    [(not (pattern-parent pattern)) 0]
    [(private:pattern-ellipsed? pattern) (+ 1 (private:count-ellipse-level (pattern-parent pattern)))]
    [else (+ 0 (private:count-ellipse-level (pattern-parent pattern)))]))

(define (private:pattern-ellipsed? pattern)
  (cond 
    [(not pattern) #f]
    [(not (pattern-parent pattern)) #f]
    [(contain? '(ellipse-list-form ellipse-vector-form ellipse-pair-form) (pattern-type (pattern-parent pattern)))
      (let* ([parent (pattern-parent pattern)]
          [rest (list-after (pattern-children parent) pattern)])
        (if (not (null? rest))
          (equal? 'ellipse (pattern-type (car rest)))
          #f))]
    [else #f]))
)