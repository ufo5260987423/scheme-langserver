(library (scheme-langserver virtual-file-system index-node)
  (export 
    pick-index-node-from
    pick-index-node-parent-of

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
    index-node-should-have-type-set!
    index-node-actural-have-type-set!
    index-node-should-have-type
    index-node-actural-have-type

    init-index-node
    is-first-child?
    clear-references-for)
  (import (chezscheme))

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
    (mutable excluded-references)
    (mutable should-have-type)
    (mutable actural-have-type))
    (protocol
      (lambda (new)
        (lambda (parent start end datum/annotations children export import exclude)
          (new parent start end datum/annotations children export import exclude '() '())))))

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

(define (is-first-child? index-node)
  (if (null? (index-node-parent index-node))
    #f
    (equal? index-node (car (index-node-parent index-node)))))

(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (map clear-references-for (index-node-children index-node)))

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