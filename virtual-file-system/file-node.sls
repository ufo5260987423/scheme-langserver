(library (scheme-langserver virtual-file-system file-node)
  (export 
    make-file-node
    file-node
    file-node?
    file-node-children-set!
    file-node-children
    file-node-folder?
    file-node-parent
    file-node-name
    file-node-path
    file-node-document-set!
    file-node-document

    walk-file
    folder-or-scheme-file?
    search-end-with 
    scheme-file?)
  (import (chezscheme)
    (only (srfi :13 strings) string-prefix? string-suffix?))

;;todo: add file change notify
(define-record-type file-node 
  (fields
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
    (immutable path)
    (immutable name)
    (immutable parent)
    (immutable folder?)

    (mutable children)
    (mutable document)))

(define (walk-file node path)
  (if (equal? (file-node-path node) path)
    node
    (if (string-prefix? (file-node-path node) path)
      (let* ([result (map 
              (lambda (new-node) (walk-file new-node path)) 
              (file-node-children node))]
            [child (find (lambda (child-node) (not (null? child-node))) result)])
        (if child child '()))
      '())))

(define (folder-or-scheme-file? path)
  (or (file-directory? path) (scheme-file? path)))

(define (scheme-file? path)
  (and 
    (not (file-directory? path))
    (not 
      (equal? #f (find (lambda (suffix) (string-suffix? suffix path))
      '( ".sps" ".sls" ".scm" ".ss"))))))

(define (search-end-with node path-list)
  (cond 
    [(equal? (file-node-name node) (car (reverse path-list))) node]
    [(file-node-folder? node) (map (lambda (n) (search-end-with n path-list)) (file-node-children node))]
    [else '()]))
)