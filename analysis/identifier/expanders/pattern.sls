(library (scheme-langserver analysis identifier expanders pattern)
  (export
    make-pattern
    pattern?
    pattern-type
    pattern-content
    pattern-children
    pattern-parent
    pattern-exposed-literals

    expansion->printable-object
    expand->index-node-compound-list

    quoted-node?
    quote-content-node)
  (import
    (chezscheme)
    (scheme-langserver util contain)
    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node))

;R6RS syntax-rules and syntax-case mainly consist of patterns and templates.
;Since scheme-langserver faces incomplete code, some fault-tolerant features
;are necessary:
;First, patterns are supposed to be strictly correct.
;Second, templates are not supposed to be strictly correct and this gives
;some tolerance. Or in other words, though R6RS says the template is
;literally a pattern, here we can't just do so.

(define-record-type pattern
  (fields
    ;r6rs 11.19
    ;underscore/pattern-variable/literal-identifier/list-form/pair-form/ellipse-list-form/ellipse-pair-form/vector-form/ellipse-vector-form/equal?-datum
    (immutable type)
    (immutable content)
    (immutable children)
    (mutable parent)
    (mutable exposed-literals))
  (protocol
    (lambda (new)
      (lambda (successed-matched-pattern-expression)
        (cond
          [(eq? successed-matched-pattern-expression '_) (new 'underscore successed-matched-pattern-expression '() #f '())]
          [(eq? successed-matched-pattern-expression '...) (new 'ellipse successed-matched-pattern-expression '() #f '())]
          [(symbol? successed-matched-pattern-expression) (new 'pattern-variable/literal-identifier successed-matched-pattern-expression '() #f `(,successed-matched-pattern-expression))]

          [(list? successed-matched-pattern-expression)
            (let ([p
                  (new
                    (if (contain? successed-matched-pattern-expression '...) 'ellipse-list-form 'list-form)
                    successed-matched-pattern-expression
                    (map make-pattern successed-matched-pattern-expression)
                    #f
                    '())])
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
                    '())])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [(vector? successed-matched-pattern-expression)
            (let ([p
                  (new
                    (if (contain? (vector->list successed-matched-pattern-expression) '...) 'ellipse-vector-form 'vector-form)
                    successed-matched-pattern-expression
                    (map make-pattern (vector->list successed-matched-pattern-expression))
                    #f
                    '())])
              (map (lambda (child) (pattern-parent-set! child p)) (pattern-children p))
              (pattern-exposed-literals-set! p (dedupe (apply append (map pattern-exposed-literals (pattern-children p)))))
              p)]
          [else (new 'equal?-datum successed-matched-pattern-expression '() #f '())])))))

(define (expansion->printable-object expansion)
  (cond
    [(list? expansion) (map expansion->printable-object expansion)]
    [(vector? expansion) (vector-map expansion->printable-object expansion)]
    [(pair? expansion)
      `(,(expansion->printable-object (car expansion)) . ,(expansion->printable-object (cdr expansion)))]
    [(index-node? expansion)
      (index-node-expression expansion)]
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
      [else (raise 'illegal-template)])))

;one expansion round per ellipsis level: bindings-product distributes the
;ellipsed variables' value lists over template-ellipsed-level rounds
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
                [pre-origin-pair (assoc k pattern-context)]
                [cdr? (if pre-origin-pair (cdr pre-origin-pair) #f)]
                [split? (and cdr? (find index-node? vs))])
              (if (and (list? vs) (not split?))
                (map (lambda (v) `((,k . ,v))) vs)
                `((,kv-pair)))))
          bindings)))))

;index-node represents quoted forms (both 'x and (quote x) in source) with
;datum (quote x) but WITHOUT the quote head among the children (tokenizer
;abbreviation convention). A (quote p) pattern whose first position is the
;literal quote therefore cannot use the default pairwise matching; bind the
;second pattern position to a synthesized content node instead.
(define (quoted-node? node)
  (let ([datum (index-node-expression node)]
      [children (index-node-children node)])
    (and (pair? datum)
      (eq? 'quote (car datum))
      (or (null? children)
        (not (eq? 'quote (index-node-expression (car children))))))))

(define (quote-content-node node)
  (let* ([datum (index-node-expression node)]
      [content (cadr datum)]
      [source
        (or (guard (e [else #f]) (annotation-source (index-node-datum/annotations node)))
          (guard (e [else #f])
            (annotation-source (index-node-datum/annotations (index-node-parent node))))
          (make-source-object
            (make-source-file-descriptor "quote-content" (open-bytevector-input-port #vu8()))
            0 0))]
      [annotation (make-annotation content source content)])
    (make-index-node (index-node-parent node)
      (index-node-start node)
      (index-node-end node)
      annotation
      #f
      '() '() '() '())))

)
