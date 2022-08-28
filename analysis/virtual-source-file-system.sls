(library (scheme-langserver analysis virtual-source-file-system)
  (export 
    init-virtual-source-file-system )
  (import 
    (chezscheme) 
    (ufo-match) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver util io))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type source-file-node 
  (fields
    (immutable name)
    (immutable parent)
    (immutable vritual-file-nodes)))

(define init-virtual-source-file-system 
  (case-lambda 
    [(file-node) 
      (if (null? (file-node-parent file-node))
        (init-virtual-source-file-system file-node (make-source-file-node '() '() file-node))
        (init-virtual-source-file-system (file-node-parent file-node)))]
    [(file-node root-source-file-node) 
      (if (file-node-folder? file-node)
        (map 
          (lambda (child-node) (init-virtual-source-file-system child-node root-source-file-node))
          (file-node-children file-node))
        (let* ([document-instance (file-node-document file-node)]
            [index-node-instance (document-index-node document-instance)]
            [expression (annotation-stripped (index-node-datum/annotations index-node-instance))])
            ;;rule
          (match expression 
            [('library (name **1) rest ... ) (generate name root-source-file-node file-node)])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (walk-source list-instance node)
  (if (null? list-instance)
    node
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [children (find 
              (lambda(child-node) (equal? head (source-file-node-name child-node))) 
              (source-file-node-children node))])
      (if (null? children)
        '()
        (walk-source rest (car children))))))

(define (generate list-instance node virtual-file-node)
  (if (null? list-instance)
    (source-file-node-virtual-file-nodes-set! (append (source-file-node-virtual-file-nodes node) `(,virtual-file-node)))
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [children (find 
              (lambda(child-node) (equal? head (source-file-node-name child-node))) 
              (source-file-node-children node))]
          [child-node (if (null? children)
              (make-source-file-node head node '())
              (car children))])
      (if (null? children)
        (begin
          (source-file-node-children-set! 
            node 
            (append (source-file-node-children node) child-node))
          (generate list-instance node virtual-file-node))
        (generate rest child-node virtual-file-node)))))
)