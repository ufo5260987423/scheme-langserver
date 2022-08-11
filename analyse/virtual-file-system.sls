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

(define (init-virtual-file-system path parent filter)
  (if (filter path)
    (let* ([name (uri->name (path->uri path))] 
          [folder? (file-directory? path)]
          [children (if (folder?) (directory-list path) '())]
          [node (make-node path (uri->name (path->uri path)) parent folder? '())])
      (if folder?
        (node-children-set!  node (map (lambda(p) 
              (init-virtual-file-system (string-append path "/" p) node filter)) children)))
      node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (folder-or-scheme-file? path)
  (if (file-directory? path) 
    #t
    (not (null? (memq #t
      (map 
        (lambda (suffix) (string-suffix? path suffix)) 
        '(".sps" ".sls" ".scm" ".ss")))))))
)