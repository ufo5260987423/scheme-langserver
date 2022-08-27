(library (scheme-langserver analysis virtual-file-system file-node)
  (export 
    file-node
    file-node?
    file-node-children
    file-node-folder?
    file-node-parent
    file-node-name
    file-node-path
    init-virtual-file-system 
    folder-or-scheme-file?)
  (import 
    (chezscheme) 
    (scheme-langserver analyse virtual-file-system document)
    (scheme-langserver util path)
    (scheme-langserver util try)
    (only (srfi :13 strings) string-prefix? string-suffix?))

(define-record-type file-node 
  (fields
  ; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  ;; 有个root-uri属性
    (immutable path)
    (immutable name)
    (immutable parent)
    (immutable folder?)
    (mutable children)
    (mutable document)
  ))

(define (init-virtual-file-system path parent my-filter)
  (if (my-filter path)
    (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [document (if folder? '() 
              (try
                (init-document (path->uri path))
                ;;todo diagnostic
                (except e
                  [else '()])))]
          [node (make-file-node path name parent folder? '() document)]
          [children (if folder?
              (map 
                (lambda(p) 
                  (init-virtual-file-system 
                    (string-append path (list->string (list (directory-separator))) p) 
                    node 
                    my-filter)) 
                (directory-list path))
              '())])
      (file-node-children-set! node (filter (lambda(p) (not (null? p))) children))
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