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
    resolve-uri->file-node
    folder-or-scheme-file?
    search-end-with 
    scheme-file?)
  (import (chezscheme)
    (scheme-langserver util path)
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

; Resolve a text-document URI to a file-node in the virtual file system.
; Tries uri->path first (handles percent-encoding), then falls back to
; stripping the "file://" prefix directly, which works around clients
; that send malformed URIs.
(define (resolve-uri->file-node root-file-node uri)
  (let ([pre-file-node (walk-file root-file-node (uri->path uri))])
    (if (not (null? pre-file-node))
      pre-file-node
      (let ([fallback-path 
              (if (uri-is-path? uri)
                (substring uri 7 (string-length uri))
                uri)])
        (walk-file root-file-node fallback-path)))))

(define (folder-or-scheme-file? path)
  (or (file-directory? path) (scheme-file? path)))

(define (scheme-file? path)
  (and 
    (not (file-directory? path))
    (not 
      (equal? #f (find (lambda (suffix) (string-suffix? suffix path))
      '( ".sps" ".sls" ".scm" ".ss"))))))

(define (search-end-with node suffix)
  (cond 
    [(file-node-folder? node) 
      (apply append (map (lambda (n) (search-end-with n suffix)) (file-node-children node)))]
    [(string-suffix? suffix (file-node-path node)) 
      `(,node)]
    [else '()]))
)