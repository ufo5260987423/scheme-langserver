(library (scheme-langserver virtual-file-system index-node)
  (export 
    pick-index-node-from
    pick-index-node-parent-of
    pick-index-node-with-mapper 

    make-index-node
    index-node?
    index-node-parent
    index-node-start
    index-node-end
    index-node-datum/annotations

    index-node-children
    index-node-children-set!
    index-node-references-export-to-other-node
    index-node-references-export-to-other-node-set!
    index-node-references-import-in-this-node
    index-node-references-import-in-this-node-set!
    index-node-excluded-references
    index-node-excluded-references-set!

    init-index-node
    is-first-child?
    is-leaf?
    cover?
    clear-references-for)
  (import 
    (chezscheme)
    (scheme-langserver util dedupe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type index-node
  (fields
    (immutable parent)
    (immutable start)
    (immutable end)
    (immutable datum/annotations)

    (mutable children)
    (mutable references-export-to-other-node)
    (mutable references-import-in-this-node)
    (mutable excluded-references)))

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

(define (is-first-child? index-node)
  (if (null? (index-node-parent index-node))
    #f
    (equal? index-node (car (index-node-children (index-node-parent index-node))))))

(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (map clear-references-for (index-node-children index-node)))

(define (pick-index-node-with-mapper origin-index-node target-index-node-list mapper-vector)
  (let ([start (index-node-start origin-index-node)]
      [end (index-node-end origin-index-node)])
    (let loop ([i start]
        [mapper (vector-ref mapper-vector start)])
      (if (< i end)
        (if (= -1 mapper)
          '()
          (loop (+ i 1) (vector-ref mapper-vector (+ i 1))))
        (pick-index-node-parent-of 
          target-index-node-list
          (vector-ref mapper-vector start)
          (vector-ref mapper-vector end))))))

; It's for partially indexing to speed up document synchronize. However, I notice that the semantic changing is within a document and not a index-node, which make comparing two indexing, copying identifier references and indexing changes much more difficult. Further works need more help.
(define (pick-index-node-parent-of index-node-list position0 position1)
  (let ([result (pick-index-node-from index-node-list position0)])
    (if (null? result)
      result
      (let* ([start (index-node-start result)]
          [end (index-node-end result)])
        (if (and (<= start position0) (>= end position1))
          result
          (pick-index-node-parent-of `(,(index-node-parent result)) position0 position1))))))

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