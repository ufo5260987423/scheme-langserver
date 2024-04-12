(library (scheme-langserver virtual-file-system index-node)
  (export 
    debug:print-expression
    debug:print-expression&variable
    debug:recursive-print-expression&variable

    pick-index-node-from
    pick-index-node-parent-of
    pick-index-node-with-mapper 
    pick-index-node-cover-mapper

    get-root-ancestor

    make-index-node
    index-node?
    index-node-parent
    index-node-start
    index-node-end
    index-node-datum/annotations
    index-node-variable

    index-node-children
    index-node-children-set!
    index-node-references-export-to-other-node
    index-node-references-export-to-other-node-set!
    index-node-references-import-in-this-node
    index-node-references-import-in-this-node-set!
    index-node-excluded-references
    index-node-excluded-references-set!

    unquote-splicing?
    unquote?
    quote?
    quasiquote?

    unsyntax-splicing?
    unsyntax?
    syntax?
    quasisyntax?

    find-leaves 

    init-index-node
    is-first-child?
    is-leaf?
    is-ancestor?
    cover?
    clear-references-for)
  (import 
    (chezscheme)
    (scheme-langserver util dedupe)
    (scheme-langserver analysis type domain-specific-language variable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type index-node
  (fields
    (immutable parent)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)
    (immutable variable)

    (mutable children)
    (mutable references-export-to-other-node)
    (mutable references-import-in-this-node)
    (mutable excluded-references))
  (protocol
    (lambda (new)
      (lambda (parent start end datum/annotations children references-export-to-other-node references-import-in-this-node excluded-references)
        (new parent start end datum/annotations (make-variable) children references-export-to-other-node references-import-in-this-node excluded-references)))))

(define (unquote-splicing? index-node document)
  (private index-node document 'unquote-splicing))

(define (unquote? index-node document)
  (private index-node document 'unquote))

(define (quote? index-node document)
  (private index-node document 'quote))

(define (quasiquote? index-node document)
  (private index-node document 'quasiquote))

(define (syntax? index-node document)
  (private index-node document 'syntax))

(define (quasisyntax? index-node document)
  (private index-node document 'quasisyntax))

(define (unsyntax? index-node document)
  (private index-node document 'unsyntax))

(define (unsyntax-splicing? index-node document)
  (private index-node document 'unsyntax-splicing))

(define (private index-node document target)
  (let ([expression (annotation-stripped (index-node-datum/annotations index-node))])
    (if (pair? expression)
        (equal? target (car expression))
        #f)))

(define (debug:print-expression index-node)
  (pretty-print (annotation-stripped (index-node-datum/annotations index-node))))

(define (debug:print-expression&variable index-node)
  (debug:print-expression index-node)
  (pretty-print (index-node-variable index-node)))

(define (debug:recursive-print-expression&variable index-node)
  (debug:print-expression&variable index-node)
  (map debug:recursive-print-expression&variable (index-node-children index-node)))

(define (find-leaves index-node-list)
  (fold-left 
    (lambda (result index-node)
      (append 
        result
        (if (is-leaf? index-node)
          (list index-node)
          (find-leaves (index-node-children index-node)))))
    '()
    index-node-list))

(define (init-index-node parent datum/annotation)
  (let* ([source (annotation-source datum/annotation)]
        [node (make-index-node parent (source-object-bfp source) (source-object-efp source) datum/annotation '() '() '() '())]
        [annotation-list (annotation-expression datum/annotation)])
    (index-node-children-set! 
      node 
      (if (list? annotation-list)
        (filter 
          (lambda (item) (not (null? item)))
          (map 
            (lambda(e) 
              (if (annotation? e)
                (init-index-node node e)
                '()))
            annotation-list))
        '()))
    node))

(define (is-leaf? index-node)
  (null? (index-node-children index-node)))

(define (cover? index-node position)
  (and (<= (index-node-start index-node) position) (> (index-node-end index-node) position)))

(define (is-ancestor? ancestor target)
  (if (null? target)
    #f
    (if (equal? ancestor (index-node-parent target))
      #t
      (is-ancestor? ancestor (index-node-parent target)))))

(define (is-first-child? index-node)
  (if (null? (index-node-parent index-node))
    #f
    (equal? index-node (car (index-node-children (index-node-parent index-node))))))

(define (get-root-ancestor index-node)
  (if (null? (index-node-parent index-node))
    index-node
    (get-root-ancestor (index-node-parent index-node))))

(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (map clear-references-for (index-node-children index-node)))

(define (pick-index-node-cover-mapper target-index-node-list mapper-vector)
  (let ([start (index-node-start (car target-index-node-list))]
      [end (index-node-end (car (reverse target-index-node-list)))]
      [v-l (vector-length mapper-vector)]
      [vector-end (vector-ref mapper-vector (- (vector-length mapper-vector) 1))])
    (let loop ([i start]
        [result '()])
      (if (< i v-l)
        (loop
          (+ i 1)
          (if (= -1 (vector-ref mapper-vector i))
            (if (null? result)
              `(,(list i i))
              (let* ([r (reverse result)]
                  [last (car r)]
                  [heads (reverse (cdr r))]
                  [s (car last)]
                  [e (cadr last)])
                (if (= i (+ 1 e))
                  (append heads `(,(list s i)))
                  (append result `(,(list i i))))))
            result))
        (dedupe
          (apply append 
            (map 
              (lambda (item) 
                (pick-index-node-parent-of target-index-node-list (car item) (+ 1 (cadr item))))
              result)))))))

(define (pick-index-node-with-mapper origin-index-node target-index-node-list mapper-vector)
  (let ([start (index-node-start origin-index-node)]
      [end (index-node-end origin-index-node)])
    (let loop ([i start])
      (if (< i end)
        (if (= -1 (vector-ref mapper-vector i))
          '()
          (loop (+ i 1)))
        (pick-index-node-parent-of 
          target-index-node-list
          (vector-ref mapper-vector start)
          (vector-ref mapper-vector end))))))

; It's for partially indexing to speed up document synchronize. However, I notice that the semantic changing is within a document and not a index-node, which make comparing two indexing, copying identifier references and indexing changes much more difficult. Further works need more help.
(define (pick-index-node-parent-of index-node-list position0 position1)
  (let ([f
      (dedupe (filter 
        (lambda (item) (and (<= (index-node-start item) position0) (>= (index-node-end item) position1)))
        index-node-list))])
    (if (null? f)
      '()
      (let ([c (pick-index-node-parent-of (apply append (map index-node-children index-node-list)) position0 position1)])
        (if (null? c) f c)))))

(define (pick-index-node-from index-node-list position)
  (let ([result 
      (find index-node?
        (map 
          (lambda (index-node) (pick-index-node-by index-node position)) 
          index-node-list))])
    (if result result '())))

(define (pick-index-node-by index-node position)
  (if (and (<= (index-node-start index-node) position) (> (index-node-end index-node) position))
    (let loop ([children (index-node-children index-node)])
      (if (null? children)
        index-node
        (let ([result (pick-index-node-by (car children) position)])
          (if (null? result)
            (loop (cdr children))
            result))))
    '()))
)