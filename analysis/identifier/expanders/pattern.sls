(library (scheme-langserver analysis identifier expanders pattern)
  (export
    make-pattern
    pattern?
    pattern-type
    pattern-content
    pattern-children
    pattern-parent
    pattern-exposed-literals
    gather-context

    expansion->printable-object 
    context:ellipsed?
    pattern+index-node->pair-list
    pattern+index-node->bindings
    pattern+context->pairs->iterator
    expand->index-node-compound-list
    generate-binding)
  (import 
    (chezscheme)
    (ufo-coroutines)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)
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
    (mutable parent)
    (mutable exposed-literals)
    (mutable cdr?))
  (protocol
    (lambda (new)
      (lambda (successed-matched-pattern-expression)
        (cond 
          [(eq? successed-matched-pattern-expression '_) (new 'underscore successed-matched-pattern-expression '() #f '() #f)]
          [(eq? successed-matched-pattern-expression '...) (new 'ellipse successed-matched-pattern-expression '() #f '() #f)]
          [(symbol? successed-matched-pattern-expression) (new 'pattern-variable/literal-identifier successed-matched-pattern-expression '() #f `(,successed-matched-pattern-expression) #f)]

          [(list? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (if (contain? successed-matched-pattern-expression '...) 'ellipse-list-form 'list-form) 
                    successed-matched-pattern-expression 
                    (map make-pattern successed-matched-pattern-expression)
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [(pair? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        (if (eq? '... (car rest)) 
                          'ellipse-pair-form
                          (loop (cdr rest)))
                        'pair-form))
                    successed-matched-pattern-expression
                    (let loop ([rest successed-matched-pattern-expression])
                      (if (pair? rest)
                        `(,(make-pattern (car rest)) . ,(loop (cdr rest)))
                        `(,(make-pattern rest))))
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              (let ([children (pattern-children p)])
                (if (not (null? children))
                  (pattern-cdr?-set! (car (reverse children)) #t)))
              p)]
          [(vector? successed-matched-pattern-expression) 
            (let ([p
                  (new 
                    (if (contain? (vector->list successed-matched-pattern-expression) '...) 'ellipse-vector-form 'vector-form) 
                    successed-matched-pattern-expression
                    (map make-pattern (vector->list successed-matched-pattern-expression))
                    #f
                    '()
                    #f)])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [else (new 'equal?-datum successed-matched-pattern-expression '() #f '() #f)])))))

(define (expansion->printable-object expansion)
  (cond 
    [(list? expansion) (map expansion->printable-object expansion)]
    [(vector? expansion) (vector-map expansion->printable-object expansion)]
    [(pair? expansion) 
      `(,(expansion->printable-object (car expansion)) . ,(expansion->printable-object (cdr expansion)))]
    [(index-node? expansion)
      (annotation-stripped (index-node-datum/annotations expansion))]
    [else expansion]))

(define (expand->index-node-compound-list template-pattern bindings pattern-context)
  (let* ([type (pattern-type template-pattern)]
      [content (pattern-content template-pattern)]
      [children (pattern-children template-pattern)])
    (case type
      [pattern-variable/literal-identifier 
        (let ([pre-result (assq content bindings)])
          (if pre-result
            (if (eq? 'have-no-such-pattern-reference (cdr pre-result)) 
              content
              (cdr pre-result))
            content))]
      [(list-form vector-form pair-form) 
        ((case type 
            [list-form (lambda (a) a)]
            [vector-form list->vector]
            [pair-form (lambda (a)
              (let ([last (car (reverse a))])
                (if (and (list? last) (not (null? last)))
                  (append (reverse (cdr (reverse a))) last)
                  (let loop ([rest a])
                    (if (null? (cdr rest))
                      (car rest)
                      (cons (car rest) (loop (cdr rest))))))))])
          (map (lambda (c) (expand->index-node-compound-list c bindings pattern-context)) children))]
      [(ellipse-list-form ellipse-vector-form ellipse-pair-form)
        ((case type 
            [ellipse-list-form (lambda (a) a)]
            [ellipse-vector-form list->vector]
            [ellipse-pair-form (lambda (a) `(,(reverse (cdr (reverse a))) . ,(car (reverse a))))])
          (let* ([max-i (length children)]
              [children-vec (list->vector children)])
            (let loop ([i 0])
              (cond 
                [(>= i max-i) '()]
                [(and 
                    (< (+ 1 i) max-i)
                    (eq? 'ellipse (pattern-type (vector-ref children-vec (+ 1 i)))))
                  (let* ([current-template-pattern (vector-ref children-vec i)]
                      [template-ellipsed-level 
                        (let curr-loop ([j 1])
                          (if (and 
                              (< (+ 1 i j) max-i)
                              (eq? 'ellipse (pattern-type (vector-ref children-vec (+ 1 i j)))))
                            (curr-loop (+ 1 j))
                            j))]
                      [exposed-literals (pattern-exposed-literals current-template-pattern)]
                      [pre-new-bindings (filter (lambda (p) (contain? exposed-literals (car p))) bindings)]
                      [new-bindings-list (private:bindings-product->list template-ellipsed-level pre-new-bindings pattern-context)])
                    `(,@(map (lambda (c) (expand->index-node-compound-list (vector-ref children-vec i) c pattern-context)) new-bindings-list) . ,(loop (+ 1 template-ellipsed-level i))))]
                [else 
                  (if (eq? 'ellipse (pattern-type (vector-ref children-vec i)))
                    (loop (+ 1 i))
                    `(,(expand->index-node-compound-list (vector-ref children-vec i) bindings pattern-context) . ,(loop (+ 1 i))))]))))]
      [else (raise 'illegal-tempate)])))

;need cdr? paramter
(define (private:bindings-product->list times bindings pattern-context)
  (if (zero? times)
    bindings
    (map 
      (lambda (new-bindings) (private:bindings-product->list (- times 1) new-bindings pattern-context))
      (fold-left 
        (lambda (left right)
          (cond 
            [(null? left) right]
            [(= 1 (length left))
              (map 
                (lambda (r) (append (car left) r))
                right)]
            [(= 1 (length right))
              (map 
                (lambda (l) (append l (car right)))
                left)]
            [else (map append left right)]))
        '()
        (map 
          (lambda (kv-pair)
            (let* ([k (car kv-pair)]
                [vs (cdr kv-pair)]
                [pre-origin-pattern (assoc k pattern-context)]
                [origin-pattern (if pre-origin-pattern (cdr pre-origin-pattern) #f)]
                [cdr? (if origin-pattern (pattern-cdr? origin-pattern) #f)]
                [split? (and cdr? (find index-node? vs))])
              (if (and (list? vs) (not split?))
                (map (lambda (v) `((,k . ,v))) vs)
                `((,kv-pair)))))
          bindings)))))

(define (generate-binding literal iterator)
  ;In ellipsed pair form, won't cdr cause error? I don't think so.
  (if (procedure? iterator)
    (let* ([first-var (iterator)]
        [is-literal? (symbol? first-var)])
      (if is-literal?
        `(,literal . ,first-var)
        (let* ([l 
              (let loop ([var first-var])
                (if (eq? var 'stop-iteration)
                  '()
                  `(,var . ,(loop (iterator)))))]
            [v (list->vector l)]
            [max-i (length l)]
            [tmp (make-vector max-i '())])
          (let loop ([i 0] [ancestors '()] [result '()])
        (cond 
          [(and (= i max-i) (not (null? ancestors)) (not (null? (cdr ancestors))))
            ;process rest ancestors
            (vector-set! tmp (cadr ancestors)
              (append 
                (vector-ref tmp (cadr ancestors))
                `(,(vector-ref tmp (car ancestors)))))
            (loop i (cdr ancestors) result)]
          [(and (= i max-i) (not (null? ancestors)))
            ;process rest ancestors
            (loop i (cdr ancestors) (append result (vector-ref tmp (car ancestors))))]
          [(= i max-i) `(,literal . ,result)]

          [(list? (vector-ref v i))
            (loop (+ 1 i) ancestors (append result (vector-ref v i)))]
          [(index-node? (vector-ref v i))
            (vector-set! tmp (car ancestors) 
              (append 
                (vector-ref tmp (car ancestors))
                `(,(vector-ref v i))))
            (loop (+ 1 i) ancestors result)]

          [(and (private:dive-into-an-ellipsed-form? (vector-ref v i)) (null? ancestors))
            (loop (+ 1 i) `(,i . ,ancestors) result)]
          [(and (private:dive-into-an-ellipsed-form? (vector-ref v i)) (<= (cdr (vector-ref v i)) (cdr (vector-ref v (car ancestors)))) (not (null? (cdr ancestors))))
            (vector-set! tmp (cadr ancestors) 
              (append 
                (vector-ref tmp (cadr ancestors))
                `(,(vector-ref tmp (car ancestors)))))
            (loop i (cdr ancestors) result)]
          [(and (private:dive-into-an-ellipsed-form? (vector-ref v i)) (<= (cdr (vector-ref v i)) (cdr (vector-ref v (car ancestors)))))
            (loop 
              (+ 1 i) 
              `(,i . ,(cdr ancestors)) 
              (append result (vector-ref tmp (car ancestors))))]

          [(private:dive-into-an-ellipsed-form? (vector-ref v i))
            (loop (+ 1 i) `(,i . ,ancestors) result)]

          [(and (private:dive-into-an-ellipsed-leaf? (vector-ref v i)) (null? ancestors))
            (loop (+ 1 i) `(,i . ,ancestors) result)]
          [(and (private:dive-into-an-ellipsed-leaf? (vector-ref v i)) (<= (cdr (vector-ref v i)) (cdr (vector-ref v (car ancestors)))) (not (null? (cdr ancestors))))
            (vector-set! tmp (cadr ancestors) 
              (append 
                (vector-ref tmp (cadr ancestors))
                `(,(vector-ref tmp (car ancestors)))))
            (loop i (cdr ancestors) result)]
          [(and (private:dive-into-an-ellipsed-leaf? (vector-ref v i)) (<= (cdr (vector-ref v i)) (cdr (vector-ref v (car ancestors)))))
            (loop 
              (+ 1 i) 
              `(,i . ,(cdr ancestors)) 
              (append result (vector-ref tmp (car ancestors))))]
          [(private:dive-into-an-ellipsed-leaf? (vector-ref v i))
            (loop (+ 1 i) `(,i . ,ancestors) result)]
          
          [(and 
              (eq? 'escape-from-target-form (vector-ref v i))
              (null? ancestors))
            ;no matching elements for ellipsed pattern variable
            (loop (+ 1 i) ancestors result)]
          [(and 
              (eq? 'escape-from-target-form (vector-ref v i))
              (private:dive-into-an-ellipsed-leaf? (vector-ref v (car ancestors)))
              (null? (cdr ancestors)))
            (loop 
              (+ 1 i) 
              (cdr ancestors)
              (append result (vector-ref tmp (car ancestors))))]
          [(and 
              (eq? 'escape-from-target-form (vector-ref v i))
              (private:dive-into-an-ellipsed-leaf? (vector-ref v (car ancestors))))
            (vector-set! tmp (cadr ancestors) 
              (append 
                (vector-ref tmp (cadr ancestors))
                `(,(vector-ref tmp (car ancestors)))))
            (loop (+ 1 i) (cdr ancestors) result)]
          [else 
            ;well, not leaf
          (loop (+ 1 i) ancestors result)])))))
    `(,literal . ,iterator)))

;suppose pattern-type is pattern-variable/literal-identifier 
(define (pattern+context->pairs->iterator pattern-content context)
  (let* ([pre-pattern (assoc pattern-content context)]
      [pattern (if pre-pattern (cdr pre-pattern) pre-pattern)])
    (cond 
      [(not pre-pattern) (lambda (_) pattern-content)]
      [(recursive:pattern-ellipsed? pattern)
        (lambda (pair-list)
          (let* ([ancestor-vector (list->vector (private:ancestors pattern))]
              [pair-vector (list->vector pair-list)]
              [max-i (vector-length pair-vector)]
              [max-j (vector-length ancestor-vector)]
              [level (private:count-ellipse-level pattern)]
              [cdr? (pattern-cdr? pattern)])
            (init-iterator
              (lambda (yield)
                (let loop ([i 0] [j 0])
                  (cond 
                    [(= i max-i) '()]

                    [(and 
                        (= j (- max-j 1))
                        (eq? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)))
                      (if (and 
                          (private:pattern-ellipsed? pattern) 
                          (yield `(dive-into-an-ellipsed-leaf . ,level)))
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

                    [(eq? (car (vector-ref pair-vector i)) (vector-ref ancestor-vector j)) 
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
          (let ([t (find (lambda (p) (eq? (car p) pattern)) pair-list)])
            (if t (cdr t) (raise 'pattern-not-match))))])))

(define (private:ancestors pattern)
  (if (pattern? pattern)
    `(,@(private:ancestors (pattern-parent pattern)) ,pattern)
    '()))

(define (private:dive-into-an-ellipsed-form? ready)
  (and (pair? ready) (integer? (cdr ready)) (eq? 'dive-into-an-ellipsed-form (car ready))))

(define (private:dive-into-an-ellipsed-leaf? ready)
  (and (pair? ready) (integer? (cdr ready)) (eq? 'dive-into-an-ellipsed-leaf (car ready))))

(define (recursive:ancestor? ancestor child)
  (cond 
    [(not (pattern? ancestor)) #f]
    [(not (pattern? child)) #f]
    [(eq? ancestor (pattern-parent child)) #t]
    [else (recursive:ancestor? ancestor (pattern-parent child))]))

;the pattern must match the index-node.
(define (pattern+index-node->pair-list pattern index-node)
  `((,pattern . ,index-node) .
    ,(let ([p-c (pattern-children pattern)]
        [i-c (index-node-children index-node)]
        [i-e (guard (e [else '()])
               (annotation-stripped (index-node-datum/annotations index-node)))])
      (case (pattern-type pattern)
        [(list-form vector-form pair-form)
          (let loop ([rest-patterns p-c] [rest-index-nodes i-c])
            (cond
              [(null? rest-patterns) '()]
              [(null? rest-index-nodes)
               (apply append
                 (map (lambda (p) (pattern+index-node->pair-list p (make-index-node '() '() '() '() #f '() '() '() '()))) rest-patterns))]
              [(and (eq? 'pair-form (pattern-type pattern)) (null? (cdr rest-patterns)))
               (if (null? (cdr rest-index-nodes))
                 `((,(car rest-patterns) . ,(car rest-index-nodes)))
                 `((,(car rest-patterns) . ,rest-index-nodes)))]
              [else
               `(,@(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes))
                 . ,(loop (cdr rest-patterns) (cdr rest-index-nodes)))]))]
        [pair-form
          `(,(pattern+index-node->pair-list (car p-c) (car i-c)) .
            ,(fold-left
              (lambda (left right-index-node)
                `(,@left . ,(pattern+index-node->pair-list (cadr p-c) right-index-node)))
              '()
              (cdr i-c)))]
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
        [ellipse-pair-form
          (if (list? i-e)
            (let loop (
                [rest-patterns (cdr (reverse p-c))] 
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
                    ,(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)))]))
            (let loop (
                [rest-patterns (cdr (reverse p-c))] 
                [rest-index-nodes (cdr (reverse i-c))])
              (case (pattern-type (car rest-patterns))
                [ellipse 
                  (let curr-loop ([head-patterns (reverse (cddr rest-patterns))]
                      [head-index-nodes (reverse rest-index-nodes)])
                    (if (null? head-patterns)
                      (append 
                        (apply append 
                          (map 
                            (lambda (i) (pattern+index-node->pair-list (cadr rest-patterns) i))
                            head-index-nodes))
                        (pattern+index-node->pair-list (car (reverse p-c)) (car (reverse i-c))))
                      `(,@(pattern+index-node->pair-list (car head-patterns) (car head-index-nodes)) . ,(curr-loop (cdr head-patterns) (cdr head-index-nodes)))))]
                [else 
                  `(,@(loop (cdr rest-patterns) (cdr rest-index-nodes)) .
                    ,(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)))])))]
        [else '()]))))

;Chez-style single-pass matcher (cf. Chez s/syntax.ss $syntax-dispatch):
;walks the call-site index-node tree once, collecting bindings directly,
;instead of the pairs+iterator+generate-binding three-pass pipeline.
;pattern variables are distinguished from literals via the literals list
;(available at macro definition time).
;Returns: bindings alist ((var . value) ...), where value is an index-node,
;a list of index-nodes (ellipsed), or nested lists; #f if not matched;
;the symbol 'fallback if the pattern shape exceeds this matcher's coverage
;(caller should fall back to pattern+index-node->pair-list et al).
(define (pattern+index-node->bindings pattern index-node literals)
  (let ([r (private:match-pattern pattern index-node literals '())])
    (cond
      [(eq? r 'fallback) 'fallback]
      [(not r) #f]
      [else r])))

(define (private:empty-node)
  (make-index-node '() '() '() '() #f '() '() '() '()))

(define (private:node-datum node)
  (guard (e [else '()])
    (annotation-stripped (index-node-datum/annotations node))))

(define (private:match-pattern pattern node literals bindings)
  (case (pattern-type pattern)
    [(underscore ellipse) bindings]
    [(pattern-variable/literal-identifier)
      (let ([sym (pattern-content pattern)])
        (if (memq sym literals)
          (and (eq? sym (private:node-datum node)) bindings)
          (cons (cons sym node) bindings)))]
    [(equal?-datum)
      (and (equal? (pattern-content pattern) (private:node-datum node)) bindings)]
    [(list-form vector-form)
      (private:match-children (pattern-children pattern) (index-node-children node) literals bindings)]
    [(pair-form)
      (private:match-pair-children (pattern-children pattern) (index-node-children node) literals bindings)]
    [(ellipse-list-form ellipse-vector-form)
      (private:match-ellipse (pattern-children pattern) (index-node-children node) literals bindings)]
    [(ellipse-pair-form)
      (private:match-ellipse-pair (pattern-children pattern) (index-node-children node) literals bindings)]
    [else 'fallback]))

(define (private:match-children patterns nodes literals bindings)
  ;fixed-length pairwise; exhausted nodes get empty placeholder nodes (fault tolerance)
  (let loop ([ps patterns] [ns nodes] [b bindings])
    (cond
      [(null? ps) b]
      [(null? ns)
        (let ([r (private:match-pattern (car ps) (private:empty-node) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr ps) '() r)]))]
      [else
        (let ([r (private:match-pattern (car ps) (car ns) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr ps) (cdr ns) r)]))])))

(define (private:improper->proper improper)
  (let loop ([rest improper] [acc '()])
    (cond
      [(null? rest) (reverse acc)]
      [(pair? rest) (loop (cdr rest) (cons (car rest) acc))]
      [else (reverse (cons rest acc))])))

(define (private:match-pair-children improper-patterns nodes literals bindings)
  ;children of a pair-form: improper pattern list whose last element has cdr? #t
  ;and consumes the remaining nodes as a whole (single node if exactly one left,
  ;a list of nodes if more, an empty placeholder node if none -- mirroring
  ;pattern+index-node->pair-list)
  (let ([patterns (private:improper->proper improper-patterns)])
    (let loop ([ps patterns] [ns nodes] [b bindings])
      (cond
        [(null? ps) b]
        [(null? (cdr ps))
          (cond
            [(null? ns) (private:match-pattern (car ps) (private:empty-node) literals b)]
            [else (private:match-rest (car ps) ns literals b)])]
        [(null? ns)
          (let ([r (private:match-pattern (car ps) (private:empty-node) literals b)])
            (cond
              [(eq? r 'fallback) 'fallback]
              [(not r) #f]
              [else (loop (cdr ps) '() r)]))]
        [else
          (let ([r (private:match-pattern (car ps) (car ns) literals b)])
            (cond
              [(eq? r 'fallback) 'fallback]
              [(not r) #f]
              [else (loop (cdr ps) (cdr ns) r)]))]))))

(define (private:match-rest pattern nodes literals bindings)
  ;match a cdr-position pattern against a list of remaining nodes
  (case (pattern-type pattern)
    [(underscore ellipse) bindings]
    [(pattern-variable/literal-identifier)
      (let ([sym (pattern-content pattern)])
        (if (memq sym literals)
          (and (null? nodes) bindings)
          (cons (cons sym (if (null? (cdr nodes)) (car nodes) nodes)) bindings)))]
    [(list-form vector-form)
      (private:match-children (pattern-children pattern) nodes literals bindings)]
    [(ellipse-list-form ellipse-vector-form)
      (private:match-ellipse (pattern-children pattern) nodes literals bindings)]
    [else 'fallback]))

(define (private:split-ellipse-segments children)
  ;split children into ((pattern . ellipsis-count) ...); consecutive ellipses
  ;merge into the preceding pattern's count (ellipsis depth)
  (let loop ([cs children] [acc '()])
    (cond
      [(null? cs) (reverse acc)]
      [(eq? 'ellipse (pattern-type (car cs))) (loop (cdr cs) acc)]
      [else
        (let count-loop ([rest (cdr cs)] [n 0])
          (if (and (not (null? rest)) (eq? 'ellipse (pattern-type (car rest))))
            (count-loop (cdr rest) (+ 1 n))
            (loop rest (cons (cons (car cs) n) acc))))])))

(define (private:match-ellipse children nodes literals bindings)
  (let ([segments (private:split-ellipse-segments children)])
    (cond
      ;multiple separate ellipses impose equal-length constraints this
      ;matcher does not implement; defer to the pairs+iterator path
      [(> (length (filter (lambda (s) (> (cdr s) 0)) segments)) 1) 'fallback]
      [else (private:match-ellipse-segments (reverse segments) (reverse nodes) literals bindings)])))

(define (private:match-ellipse-segments rev-segments rev-nodes literals bindings)
  ;walk segments right-to-left; rev-nodes is the reversed node children.
  ;The (single) ellipsed segment collects bindings from ALL remaining nodes
  ;in original left-to-right order (nested ellipsis recurses per node).
  (cond
    [(null? rev-segments) bindings]
    [(> (cdar rev-segments) 0)
      (let* ([segment-pattern (caar rev-segments)]
          [ordered (reverse rev-nodes)]
          [results (map (lambda (n) (private:match-pattern segment-pattern n literals '())) ordered)])
        (cond
          [(or (member #f results) (member 'fallback results)) 'fallback]
          [else
            (let* ([vars (pattern-exposed-literals segment-pattern)]
                [collected
                  (map
                    (lambda (v) (cons v (map (lambda (rb) (cdr (assq v rb))) results)))
                    vars)]
                [new-bindings (append collected bindings)])
              ;segments to the left of the ellipsed one share the same nodes:
              ;the fixed prefix patterns match the first nodes one-to-one
              (private:match-prefix (cdr rev-segments) ordered literals new-bindings))]))]
    [(null? rev-nodes)
      (let ([r (private:match-pattern (caar rev-segments) (private:empty-node) literals bindings)])
        (cond
          [(eq? r 'fallback) 'fallback]
          [(not r) #f]
          [else (private:match-ellipse-segments (cdr rev-segments) '() literals r)]))]
    [else
      (let ([r (private:match-pattern (caar rev-segments) (car rev-nodes) literals bindings)])
        (cond
          [(eq? r 'fallback) 'fallback]
          [(not r) #f]
          [else (private:match-ellipse-segments (cdr rev-segments) (cdr rev-nodes) literals r)]))]))

(define (private:match-prefix rev-prefix-segments ordered literals bindings)
  ;fixed segments left of the ellipsed one: pairwise against the first nodes
  (let loop ([segs (reverse rev-prefix-segments)] [ns ordered] [b bindings])
    (cond
      [(null? segs) b]
      [(null? ns)
        (let ([r (private:match-pattern (caar segs) (private:empty-node) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr segs) '() r)]))]
      [else
        (let ([r (private:match-pattern (caar segs) (car ns) literals b)])
          (cond
            [(eq? r 'fallback) 'fallback]
            [(not r) #f]
            [else (loop (cdr segs) (cdr ns) r)]))])))

(define (private:match-ellipse-pair improper-children nodes literals bindings)
  ;(P ... . tail) shape only; anything more complex defers to the legacy path
  (let ([children (private:improper->proper improper-children)])
    (if (and (= (length children) 3)
          (eq? 'ellipse (pattern-type (cadr children))))
      (let ([segment-pattern (car children)]
          [tail-pattern (caddr children)]
          [rev (reverse nodes)])
        (let ([b1
            (if (null? rev)
              (private:match-pattern tail-pattern (private:empty-node) literals bindings)
              (private:match-pattern tail-pattern (car rev) literals bindings))])
          (cond
            [(eq? b1 'fallback) 'fallback]
            [(not b1) #f]
            [else
              (let* ([ordered (if (null? rev) '() (reverse (cdr rev)))]
                  [results (map (lambda (n) (private:match-pattern segment-pattern n literals '())) ordered)])
                (cond
                  [(or (member #f results) (member 'fallback results)) 'fallback]
                  [else
                    (let* ([vars (pattern-exposed-literals segment-pattern)]
                        [collected
                          (map
                            (lambda (v) (cons v (map (lambda (rb) (cdr (assq v rb))) results)))
                            vars)])
                      (append collected b1))]))])))
      'fallback)))

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
          (eq? 'ellipse (pattern-type (car rest)))
          #f))]
    [else #f]))
)