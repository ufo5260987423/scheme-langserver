(library (scheme-langserver virtual-file-system library-node)
  (export 
    delete-library-node-from-tree

    make-library-node
    library-node?
    library-node-name
    library-node-parent
    library-node-file-nodes
    library-node-file-nodes-set!
    library-node-children
    library-node-children-set!
    library-node-name->string
    walk-library)
  (import (rnrs))

(define-record-type library-node 
  (fields
    (immutable name)
    (immutable parent)
    (mutable children)
    (mutable file-nodes))
  (protocol 
    (lambda (new)
      (case-lambda 
        [(name parent children file-nodes) (new name parent children file-nodes)]
        [(list-instance library-node virtual-file-node)
          (if (null? list-instance)
            (begin
              (library-node-file-nodes-set! library-node (append (library-node-file-nodes library-node) `(,virtual-file-node)))
              library-node)
            (let* ([head (car list-instance)]
                  [rest (cdr list-instance)]
                  [child (find 
                      (lambda (child-node) (equal? head (library-node-name child-node))) 
                      (library-node-children library-node))])
              (make-library-node
                rest 
                (if child
                  child
                  (let ([child (make-library-node head library-node '() '())])
                    (library-node-children-set! library-node 
                      (append (library-node-children library-node) `(,child)))
                    child))
                virtual-file-node)))]))))

(define (library-node-name->string target-library-node)
  (cond 
    [(null? target-library-node) ""]
    [(null? (library-node-parent target-library-node)) ""]
    [else 
      (string-append 
        (library-node-name->string (library-node-parent target-library-node))
        " "
        (private-symbol-list->string (library-node-name target-library-node)))]))

(define (private-symbol-list->string target-list)
  (cond 
    [(null? target-list) "()"]
    [(list? target-list) 
      (fold-left
        (lambda (l r) (if (equal? "" l) r (string-append l " " r)))
        ""
        (map private-symbol-list->string target-list))]
    [(symbol? target-list) (symbol->string target-list)]
    [(number? target-list) (number->string target-list)]))

(define (delete-library-node-from-tree current-library-node)
  (library-node-children-set!
    (library-node-parent current-library-node)
    (filter 
      (lambda (library-node)
        (not (equal? library-node current-library-node)))
      (library-node-children (library-node-parent current-library-node)))))

(define (walk-library list-instance current-library-node)
  (if (null? list-instance)
    current-library-node
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [child (find 
              (lambda (child-node) (equal? head (library-node-name child-node))) 
              (library-node-children current-library-node))])
      (if child
        (walk-library rest child)
        '()))))
)