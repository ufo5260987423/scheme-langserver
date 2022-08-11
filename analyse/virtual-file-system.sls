(library (scheme-langserver analyse virtual-file-system)
  (export 
    node-children
    node-folder?
    node-parent
    node-name
    node-path
    init-virtual-file-system 
    folder-or-scheme-file?)
  (import 
    (chezscheme) 
    (scheme-langserver util path)
    (only (srfi :13 strings) string-suffix?))

(define-record-type node 
  (fields
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable path)
    (immutable name)
    (immutable parent)
    (immutable folder?)
    (mutable children)
  ))

(define (init-virtual-file-system path parent my-filter)
  (if (my-filter path)
    (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [node (make-node path name parent folder? '())]
          [children (if folder?
              (map 
                (lambda(p) 
                  (init-virtual-file-system 
                    (string-append path (list->string (list (directory-separator))) p) 
                    node 
                    my-filter)) 
                (directory-list path))
              '())])
      (node-children-set! node (filter (lambda(p) (not (null? p))) children))
      node)
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (folder-or-scheme-file? path)
  (if (file-directory? path) 
    #t
    (find (lambda(t) (or t #f))
      (map (lambda (suffix) (string-suffix? suffix path)) 
      '(".sps" ".sls" ".scm" ".ss")))))
)