(library (scheme-langserver analysis dependency rules load)
  (export load-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver util path)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (load-process root-file-node document index-node)
  (let ([current-absolute-path (uri->path (document-uri document))])
    (match-index-node index-node
      [('load (:= index-node-expression (? string? path)) . rest) 
        (guard-for document index-node 'load '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (append 
          (private:target-file-node-list root-file-node current-absolute-path path)
          (private:children-load-process root-file-node document index-node))]
      [('load-library (:= index-node-expression (? string? path)) . rest) 
        (guard-for document index-node 'load-library '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (append 
          (private:target-file-node-list root-file-node current-absolute-path path)
          (private:children-load-process root-file-node document index-node))]
      [('load-program (:= index-node-expression (? string? path)) . rest) 
        (guard-for document index-node 'load-program '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (append 
          (private:target-file-node-list root-file-node current-absolute-path path)
          (private:children-load-process root-file-node document index-node))]
      [else (private:children-load-process root-file-node document index-node)])))

(define (private:target-file-node-list root-file-node current-absolute-path path)
  (let ([target-file-node 
          (cond
            [(path-absolute? path) (walk-file root-file-node path)]
            [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
            [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
    (if (null? target-file-node) target-file-node `(,target-file-node))))

(define (private:children-load-process root-file-node document index-node)
  (apply append (map (lambda (child) (load-process root-file-node document child)) (index-node-children index-node))))
)
