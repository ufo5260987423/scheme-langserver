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
    pattern+context->pairs->iterator)
  (import 
    (chezscheme)
    (ufo-coroutines)
    (ufo-try)
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

; (define context+template+pairs->expansion
;   (case-lambda 
;     [(pattern-context template-pattern pair-list)
;       (context+template+pairs->expansion pattern-context `(,template-pattern) pair-list '() '() 0)]
;     [(pattern-context template-patterns pair-list iterators context ellipsed-level)
;       (if (null? template-patterns)
;         '()
;         (let* ([pattern (car tempalte-patterns)]
;             [type (pattern-type pattern)]
;             [content (pattern-content pattern)])
;           (case type
;             [pattern-variable/literal-identifier 
;               (or 
;                 ;previous value
;                 (assoc context content)
;                 (let* ([pre-iterator (assoc iterators content)]
;                     [iterator 
;                       (if pre-iterator
;                         (cdr pre-iterator)
;                         ((pattern->pairs->iterator content pattern-context) pair-list))]
;                     [value (iterator)])
;                   (cond 
;                     [(equal? 'have-no-such-pattern-refference value) 
;                       `(,content . ,(context+template+pairs->expansion pattern-context (cdr template-patterns) pair-list iterators context ellipsed-level))]
;                     ; [(and (index-node? value) (= ellipsed-level (private:count-ellipse-level )))]
;                     [(index-node? value)
;                       `(,value . 
;                         ,(context+template+pairs->expansion pattern-context (cdr template-patterns) pair-list 
;                           (if pre-iterator
;                             iterators 
;                             `((,content . ,iterator) . ,iterators))
;                           `((,content . ,value) . ,context) ellipsed-level))]
;                     ;(dive-into-an-ellipsed-form . 1)
;                     ; [(and (private:dive-into? value) (not (zero? ellipsed-level)))
;                     ;   (context+template+pairs->expansion pattern-context (cdr template-patterns) pair-list 
;                     ;     (if pre-iterator
;                     ;       iterators 
;                     ;       `((,content . ,iterator) . ,iterators))
;                     ;     context ellipsed-level)]
;                     [(private:dive-into? value) 
;                       (cond 
;                         [(= ellipsed-level (cdr value))
;                           (context+template+pairs->expansion pattern-context template-patterns pair-list 
;                             (if pre-iterator
;                               iterators 
;                               `((,content . ,iterator) . ,iterators))
;                             context ellipsed-level)]
;                         [(< ellipsed-level (cdr value)) 

;                         ]
;                         [(> ellipsed-level (cdr value)) (raise 'auxillary-ellipse-in-template)])
;                     ]
;                     [else (raise value)]
;                     ))
;                     )]
;             [(list-form vector-form pair-form)
;               ((case type 
;                   [list-form (lambda (a) a)]
;                   [vector-form list->vector]
;                   [pair-form (lambda (a) `(,(car a) . ,@(cdr a)))])
;                 (context+template+pairs->expansion pattern-context (pattern-children pattern) pair-list iterators context ellipsed-level))]
;             [(ellipse-list-form ellipse-vector-form ellipse-pair-form)
;               ((case type 
;                   [list-form (lambda (a) a)]
;                   [vector-form list->vector]
;                   [pair-form (lambda (a) `(,(car a) . ,@(cdr a)))])
;                 (let* ([children (pattern-children pattern)]
;                     [children-vec (list->vector children)]
;                     [max-l (length children)])
;                   (let loop ([i 0] [repeat-i #f])
;                     (cond 
;                       [(zero? max-l) '()]
;                       [(= i max-l) '()]
;                       [(equal? 'ellipse (pattern-type (vector-ref children-vec i))) '()]
;                       [else 
;                         (let loop ([current-ellipse-index (+ 1 i)])
;                           (cond 
;                             [(or 
;                               (= current-ellipse-index max-l) 
;                               (not (equal? 'ellipse (pattern-type (vector-ref children-vec current-ellipse-index)))))
;                             (try 
;                               `(,@(context+template+pairs->expansion pattern-context `(,(vector-ref children-vec i)) pair-list iterators context (+ ellipsed-level (- current-ellipse-index i 1))) . ,(loop i))
;                               (except c
;                                 ;escape from current ellipse-form to outter ellipse-form
;                                 [(equal? c '...) (loop (+ 2 i))]
;                                 ;stop current iteration
;                                 [(equal? c 'stop-iteration) (loop (+ 2 i))]
;                                 [else ]))
;                             ]
;                             [(equal? 'ellipse (pattern-type (vector-ref children-vec current-ellipse-index)))
;                               (loop (+ 1 i))]))]))))])))]))

;suppose pattern-type is pattern-variable/literal-identifier 
(define (pattern+context->pairs->iterator pattern-content context)
  (let* ([pre-pattern (assoc pattern-content context)]
      [pattern (if pre-pattern (cdr pre-pattern) pre-pattern)])
    (cond 
      [(not pre-pattern) (lambda (pair-list) 'have-no-such-pattern-refference)]
      [(recursive:pattern-ellipsed? pattern)
        (lambda (pair-list)
          (let* ([ancestor-vector (list->vector (private:ancestors pattern))]
              [pair-vector (list->vector pair-list)]
              [max-i (vector-length pair-vector)]
              [max-j (vector-length ancestor-vector)]
              [level (private:count-ellipse-level pattern)])
            (init-iterator
              (lambda (yield)
                (let loop ([i 0] [j 0])
                  (cond 
                    [(= i max-i) '()]

                    [(and 
                        (= j (- max-j 1))
                        (equal? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)))
                      (if (and 
                          (private:pattern-ellipsed? pattern) 
                          (yield `(dive-into-an-ellipsed-form . ,level)))
                        (loop i j)
                        ;procedure
                        (if (yield (cdr (vector-ref pair-vector i)))
                          (loop i j)
                          (loop (+ 1 i) j)))]
                    [(= j (- max-j 1)) 
                      ;list
                      (if (yield 'escape-from-target-form)
                        (loop i j)
                        (loop (+ 1 i) 0))]

                    [(equal? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)) 
                      ;list
                      (if (and 
                          (private:pattern-ellipsed? (vector-ref ancestor-vector j)) 
                          (yield `(dive-into-an-ellipsed-form . ,(private:count-ellipse-level (vector-ref ancestor-vector j)))))
                        (loop i j)
                        (loop (+ 1 i) (+ 1 j)))]
                    [(recursive:ancestor? (vector-ref ancestor-vector j) (car (vector-ref pair-vector i))) (loop i (+ 1 j))]

                    [else (loop (+ 1 i) 0)]))))))]
      [else 
        (lambda (pair-list)
          (let ([t (find (lambda (p) (equal? (car p) pattern)) pair-list)])
            (if t (cdr t) (raise 'pattern-not-match))))])))

(define (private:ancestors pattern)
  (if (pattern? pattern)
    `(,@(private:ancestors (pattern-parent pattern)) ,pattern)
    '()))

(define (private:dive-into? ready)
  (and (pair? ready) (integer? (cdr ready)) (equal? 'dive-into-an-ellipsed-form (car ready))))

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