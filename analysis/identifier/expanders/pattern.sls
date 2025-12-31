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
    pattern+index-node->pair-list)
  (import 
    (chezscheme)
    (scheme-langserver util contain)
    (scheme-langserver util sub-list)

    (scheme-langserver virtual-file-system index-node))

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

;the pattern must match the index-node.
(define (pattern+index-node->pair-list pattern index-node)
  `((,pattern . ,index-node) .
    ,(let ([p-c (pattern-children pattern)]
        [i-c (index-node-children index-node)]
        [p-p (pattern-parent pattern)]
        [i-p (index-node-parent index-node)])
      (case (pattern-type pattern)
        [(list-form vector-form pair-form)
          (let loop ([rest-patterns p-c] [rest-index-nodes i-c])
            (if (null? rest-patterns)
              '()
              `(,@(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)) . ,(loop (cdr rest-patterns) (cdr rest-index-nodes)))))]
        [(ellipse-list-form ellipse-vector-form ellipse-pair-form)
          (let loop (
              [rest-patterns (reverse p-c)] 
              [rest-index-nodes (reverse i-c)]
              [current-result '()])
            (case (pattern-type (car rest-patterns))
              [ellipse 
                (let curr-loop ([head-patterns (reverse (cddr rest-patterns))]
                    [head-index-nodes (reverse rest-index-nodes)])
                  (if (null? head-patterns)
                    (map 
                      (lambda (i) `(,(cadr rest-patterns) . ,i))
                      head-index-nodes)
                    `(,@(pattern+index-node->pair-list (car head-patterns) (car head-index-nodes)) ,@(curr-loop (cdr head-patterns) (cdr head-index-nodes)) . , current-result)))]
              [else 
                (loop (cdr rest-patterns) (cdr rest-index-nodes) `(,@(pattern+index-node->pair-list (car rest-patterns) (car rest-index-nodes)) . ,current-result))]))]
        [else '()]))))

(define (gather-context pattern)
  (case (pattern-type pattern)
    [pattern-variable/literal-identifier `((,(pattern-content pattern) . ,pattern))]
    [(ellipse-list-form list-form ellipse-vector-form vector-form ellipse-pair-form pair-form) (apply append (map gather-context (pattern-children pattern)))]
    [else '()]))

(define (context:ellipsed? context pattern-variable/literal-identifier)
  (let* ([v-p (assoc pattern-variable/literal-identifier context)]
      [p (cdr v-p)])
    (private:pattern-ellipsed? p)))

(define (private:pattern-ellipsed? pattern)
  (cond 
    [(not pattern) #f]
    [(not (pattern-parent pattern)) #f]
    [(contain? '(ellipse-list-form ellipse-vector-form ellipse-pair-form) (pattern-type (pattern-parent pattern)))
      (let* ([parent (pattern-parent pattern)]
          [rest (list-after (pattern-children parent) pattern)])
        (if (not (null? rest))
          (if (equal? 'ellipse (pattern-type (car rest)))
            #t
            (private:pattern-ellipsed? parent))
          #f))]
    [else (private:pattern-ellipsed? (pattern-parent pattern))]))
)