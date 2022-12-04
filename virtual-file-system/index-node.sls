(library (scheme-langserver virtual-file-system index-node)
  (export 
    pick-index-node-from

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

(define (clear-references-for index-node)
  (index-node-references-export-to-other-node-set! index-node '())
  (index-node-references-import-in-this-node-set! index-node '())
  (map clear-references-for (index-node-children index-node)))

(define (pick-index-node-from index-node-list position)
  (find index-node?
    (map 
      (lambda (index-node) (pick-index-node-by index-node position)) 
      index-node-list)))

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