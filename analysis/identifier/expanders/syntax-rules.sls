(library (scheme-langserver analysis identifier expanders syntax-rules)
  (export 
    syntax-rules->generator:map+expansion
    make-generator-for-clauses)
  (import
    (chezscheme)
    (ufo-match-steer)

    (scheme-langserver analysis identifier expanders pattern)
    (scheme-langserver analysis identifier expanders dispatch-matcher)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver util path)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document))

;; ---- Signature extraction for fast clause filtering ----

(define (private:param-shape param)
  (cond
    [(null? param) 'null]
    [(symbol? param) 'symbol]
    [(pair? param) 'pair]
    [(vector? param) 'vector]
    [else 'other]))

(define (private:fixed-prefix params)
  (cond
    [(null? params) '()]
    [(and (not (null? (cdr params))) (eq? '... (cadr params)))
      (private:fixed-prefix (cddr params))]
    [else (cons (car params) (private:fixed-prefix (cdr params)))]))

(define (private:extract-signature pattern-expr)
  (let ([params (cdr pattern-expr)])
    (if (list? params)
      (let ([fixed (private:fixed-prefix params)])
        (cons (length fixed) (map private:param-shape fixed)))
      (let ([prefix 
              (let loop ([p params])
                (if (pair? p) (cons (car p) (loop (cdr p))) '()))])
        (cons (length prefix) (map private:param-shape prefix))))))

(define (private:shape-match? shape param)
  (case shape
    [(symbol) #t]
    [(null) (null? param)]
    [(pair) (or (pair? param) (null? param))]
    [(vector) (vector? param)]
    [else #t]))

(define (private:params-shapes-match? shapes params)
  (if (or (null? shapes) (null? params))
    (and (null? shapes) (null? params))
    (and (private:shape-match? (car shapes) (car params))
      (private:params-shapes-match? (cdr shapes) (cdr params)))))

(define (private:signature-match? signature input-expr)
  (let ([input-params (cdr input-expr)]
      [min-len (car signature)]
      [shapes (cdr signature)])
    (and 
      (list? input-params)
      (>= (length input-params) min-len)
      (private:params-shapes-match? shapes 
        (let loop ([lst input-params] [n (length shapes)])
          (if (or (zero? n) (null? lst)) 
            '() 
            (cons (car lst) (loop (cdr lst) (- n 1)))))))))

;; ---- Nested-macro guard ----

; If a syntax-rules template contains nested macro definitions
; (let-syntax, letrec-syntax, syntax-rules, define-syntax),
; the auto-resolver cannot handle them.  Detect this at definition
; time and install a no-op generator so that define-syntax:attach-generator
; does not crash and the macro falls back to hand-written rules.
; Templates may contain let-syntax/letrec-syntax with inner syntax-rules
; (e.g. match-two, match-check-identifier, match-check-ellipsis).
; The auto-resolver already has full let-syntax/letrec-syntax support
; (let-syntax-process + let-syntax:attach-generator), so nested macros
; inside templates are fine.  We only reject define-syntax in templates
; because runtime define-syntax is almost always a mistake and our
; scope tracking for it is not fully tested.

;; ---- Main generator factory ----

;input-index-node is supposed have the form of `(syntax-rules ...)`
(define (syntax-rules->generator:map+expansion root-file-node root-library-node document input-index-node)
  (match-index-node input-index-node
    ;clause means pattern and template
    ;:_ for the head: both call sites guarantee it -- the interpreter
    ;registers this as the second-pass rule of the 'syntax-rules special
    ;form, and the cache-reattach path case-matches (car expr) explicitly
    [(:_ (literals ...) clauses **1)
      (make-generator-for-clauses input-index-node
        (map index-node-expression literals)
        clauses
        (lambda (clause-expression) (car (reverse clause-expression))))]
    [else #f]))

; Shared logic for building a generator from clause index-nodes.
; extract-template is a function that takes a clause-expression datum
; and returns the bare template (for syntax-rules it is the last element;
; for syntax-case it is the unwrapped syntax form).
;NB: templates that themselves contain define-syntax (macro-generating
;macros) are no longer rejected here. Building the generator is pure
;definition-time work and stays inert until invoked; any expansion cascade
;through generated define-syntax forms is bounded by the recursion guard
;(memory cap) in expansion-wrap.sls. Telemetry (fallback/mismatch counters)
;now covers these macros too.
(define (make-generator-for-clauses input-index-node literals clause-index-nodes extract-template)
  (let ([clause-selector (private:compile-clause-selector literals clause-index-nodes)]
        ;definition-time per-clause compilation (Chez convert-pattern style):
        ;pattern tree, template tree and variable context are built once,
        ;not per call
        [clause-data-vector
          (list->vector
            (map
              (lambda (clause-index-node)
                (let* ([clause-expression (index-node-expression clause-index-node)]
                    [pattern-expression (car clause-expression)]
                    ;template tree per clause; the ellipsis context is derived
                    ;from the match pattern datum directly (dispatch-matcher's
                    ;gather-context -- the old pattern record existed only for
                    ;its cdr? flags and is gone); matching goes through the
                    ;Chez-transcribed dispatch-matcher (TODO-12)
                    [template-pattern (make-pattern (extract-template clause-expression))])
                  (let-values ([(dsl pvars) (convert-pattern pattern-expression literals)])
                    (vector dsl pvars template-pattern (gather-context pattern-expression literals)))))
              clause-index-nodes))]
        ;memoize the selector's transcription: keyed by the call-site datum,
        ;it maps to (clause-index . instantiated-template-datum). Only this
        ;node-independent stage is cached -- bindings/compound/skeleton/pairs
        ;embed call-site nodes and must be rebuilt per call site even for
        ;identical datums (89% of cascade expansions are exact duplicates).
        [selector-cache (make-hashtable equal-hash equal?)])
      (index-node-expansion-generator-set! input-index-node
        (lambda (local-root-file-node local-root-library-node local-document local-index-node)
          (let* ([local-expression (index-node-expression local-index-node)]
              [index+expansion
                (if clause-selector
                  (or (hashtable-ref selector-cache local-expression #f)
                    (let ([confirmed (private:selector-confirm clause-selector local-expression)])
                      (when confirmed
                        (hashtable-set! selector-cache local-expression confirmed))
                      confirmed))
                  (private:confirm-clause literals clause-index-nodes local-expression))])
          (if (not index+expansion)
            #f
            (let* ([index (car index+expansion)]
                [clause-data (vector-ref clause-data-vector index)]

                [dsl (vector-ref clause-data 0)]
                [pvars (vector-ref clause-data 1)]
                [template-pattern (vector-ref clause-data 2)]
                [pattern-context (vector-ref clause-data 3)]
                ;Chez-style dispatch matcher; anything but an alist means the
                ;match failed -- decline the expansion rather than mis-analyze
                [bindings (dsl+index-node->bindings dsl pvars local-index-node literals)])
            (if (not (list? bindings))
              #f
              (let* (
                  [callee-compound-index-node-list (expand->index-node-compound-list template-pattern bindings pattern-context)]

                [expansion-index-node
                  (init-index-node
                    (index-node-parent input-index-node)
                    (car (source-file->annotations
                      (with-output-to-string (lambda () (pretty-print (cdr index+expansion))))
                      (uri->path (document-uri local-document)))))]
                [matching-pairs (private:expansion+index-node->pairs callee-compound-index-node-list expansion-index-node)])
              `(,matching-pairs . ,expansion-index-node))))))))))

(define (private:take list n)
  (if (or (zero? n) (null? list))
    '()
    (cons (car list) (private:take (cdr list) (- n 1)))))


;these two parameter are supposed to be correct and this procedure won't do fault-tolerant things.
(define (private:expansion+index-node->pairs compound-list index-node)
  (reverse (private:expansion+index-node->pairs-rev compound-list index-node '())))

(define (private:expansion+index-node->pairs-rev compound-list index-node acc)
  (let* ([expression (index-node-expression index-node)]
      [children (index-node-children index-node)])
    (cond 
      [(index-node? compound-list)
        (let ([compound-children (index-node-children compound-list)])
          (let* ([len-cc (length compound-children)]
              [len-ch (length children)]
              [min-len (min len-cc len-ch)]
              [extra-children (if (> len-ch min-len) (list-tail children min-len) '())])
            (let ([acc0 (cons `(,index-node ,compound-list positional) acc)])
              (let ([acc1
                  (let loop ([lefts (private:take compound-children min-len)]
                      [rights (private:take children min-len)]
                      [a acc0])
                    (if (or (null? lefts) (null? rights))
                      a
                      (loop (cdr lefts) (cdr rights)
                        (private:expansion+index-node->pairs-rev (car lefts) (car rights) a))))])
                (let ([acc2
                    (let loop ([ecs extra-children] [a acc1])
                      (if (null? ecs)
                        a
                        (let* ([exp-child (car ecs)]
                            [sym (index-node-expression exp-child)])
                          (if (symbol? sym)
                            (let ([compound-match (find (lambda (c) (eq? sym (index-node-expression c))) compound-children)])
                              (if compound-match
                                (loop (cdr ecs) (cons `(,exp-child ,compound-match symbolic) a))
                                (loop (cdr ecs) a)))
                            (loop (cdr ecs) a)))))])
                  acc2)))))]
      [(list? compound-list) 
        (let ([len-c (length compound-list)]
            [len-ch (length children)])
          (let loop ([lefts (if (> len-c len-ch) (private:take compound-list len-ch) compound-list)]
              [rights (if (> len-ch len-c) (private:take children len-c) children)]
              [a acc])
            (if (or (null? lefts) (null? rights))
              a
              (loop (cdr lefts) (cdr rights)
                (private:expansion+index-node->pairs-rev (car lefts) (car rights) a)))))]
      [(vector? compound-list) 
        (private:expansion+index-node->pairs-rev (vector->list compound-list) index-node acc)]
      [(pair? compound-list) 
        (private:expansion+index-node->pairs-rev `(,(car compound-list) ,(cdr compound-list)) index-node acc)]
      [(and (symbol? compound-list) (symbol? expression))
        (cons `(,index-node ,compound-list symbolic) acc)]
      ;symbol won't get pairs
      [else acc])))

;Compile the clause selector once per macro definition, so call sites pay a
;procedure call instead of recompiling the whole syntax-case form via eval
;(measured: ~72% of generator body cost was this per-call compilation).
;Returns #f when compilation fails, keeping private:confirm-clause as fallback.
(define (private:compile-clause-selector literals clause-index-nodes)
  (guard (e [else #f])
    (eval
      `(lambda (input-expression)
        (syntax-case input-expression ,literals
          ,@(map
              (lambda (index clause-index-node)
                (let ([clause-expression (index-node-expression clause-index-node)])
                  `(,(car clause-expression)
                    #',(cons index (car (reverse clause-expression))))))
              (iota (length clause-index-nodes))
              clause-index-nodes)
          (else #f))))))

(define (private:selector-confirm selector input-expression)
  (syntax->datum (selector input-expression)))

(define (private:confirm-clause literals clause-index-nodes input-expression)
  (let loop ([rest clause-index-nodes] [index 0])
    (if (null? rest) 
      #f
      (let* ([current-clause-index-node (car rest)]
          [current-clause-expression (index-node-expression current-clause-index-node)]
          [pattern-expression (car current-clause-expression)]
          [signature (private:extract-signature pattern-expression)])
        (if (private:signature-match? signature input-expression)
          (let* ([pre-target 
            `(syntax-case ',input-expression ,literals 
              (,(car current-clause-expression) 
                ;result
                #'(,index . ,(car (reverse current-clause-expression))))
              (else #f))]
              [target (syntax->datum (eval pre-target))])
            (if target target (loop (cdr rest) (+ 1 index))))
          (loop (cdr rest) (+ 1 index)))))))
) ; end library