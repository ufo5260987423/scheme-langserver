(library (scheme-langserver analyse virtual-file-system)
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
    (scheme-langserver analyse document)
    (scheme-langserver analyse index)
    (scheme-langserver util path)
    (scheme-langserver util io)
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
    (mutable index)
  ))

(define (init-virtual-file-system path parent my-filter)
  (if (my-filter path)
    (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [document (if folder? '() (make-document (path->uri path) (read-string path)))]
          [index (if folder? '() (init-index-node '() (source-file->annotation (document-text document))))]
          [node (make-file-node path name parent folder? '() document '())]
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