(library (scheme-langserver analysis identifier expanders pattern)
  (export
    make-pattern
    pattern?
    pattern-type
    pattern-content
    pattern-children
    pattern-parent
    gather-context
    context:ellipsed?)
  (import 
    (chezscheme)
    (scheme-langserver util contain)
    (scheme-langserver util sub-list))

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

(define (gather-context pattern)
  (case (pattern-type pattern)
    [pattern-variable/literal-identifier `((,(pattern-content pattern) . ,pattern))]
    [(ellipse-list-form list-form ellipse-vector-form vector-form ellipse-pair-form pair-form) (apply append (map gather-context (pattern-children pattern)))]
    [else '()]))

(define (context:ellipsed? context pattern-variable/literal-identifier)
  (let* ([v-p (assoc pattern-variable/literal-identifier context)]
      [p (cdr v-p)])
    (if p
      (let loop ([current-pattern p] [parent-pattern (pattern-parent p)])
        (cond
          [(not parent-pattern) #f]
          [(contain? '(ellipse-list-form ellipse-vector-form ellipse-pair-form) (pattern-type parent-pattern)) 
            (let ([rest (list-after (pattern-children parent-pattern) current-pattern)])
              (if (not (null? rest))
                (equal? 'ellipse (pattern-type (car rest)))
                #f))]
          [else (loop parent-pattern (pattern-parent parent-pattern))]))
      #f)))
)