(library (scheme-langserver analysis dependency rules load)
  (export load-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (load-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [current-absolute-path (uri->path (document-uri document))])
    (match expression
      [('load (? string? path) dummy ...) 
        (guard-for index-node 'load '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (let ([target-file-node 
              (cond
                ; [(not (string? path)) '()]
                [(path-absolute? path) (walk-file root-file-node path)]
                [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
          (append 
            (if (null? target-file-node) target-file-node `(,target-file-node)) 
            (apply append (map (lambda (index-node) (load-process root-file-node document index-node)) (index-node-children index-node)))))]
      [('load-library (? string? path) dummy ...) 
        (guard-for index-node 'load '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (let ([target-file-node 
              (cond
                ; [(not (string? path)) '()]
                [(path-absolute? path) (walk-file root-file-node path)]
                [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
          (append 
            (if (null? target-file-node) target-file-node `(,target-file-node)) 
            (apply append (map (lambda (index-node) (load-process root-file-node document index-node)) (index-node-children index-node)))))]
      [('load-program (? string? path) dummy ...) 
        (guard-for index-node 'load '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (let ([target-file-node 
              (cond
                ; [(not (string? path)) '()]
                [(path-absolute? path) (walk-file root-file-node path)]
                [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
          (append 
            (if (null? target-file-node) target-file-node `(,target-file-node)) 
            (apply append (map (lambda (index-node) (load-process root-file-node document index-node)) (index-node-children index-node)))))]
      [else (apply append (map (lambda (index-node) (load-process root-file-node document index-node)) (index-node-children index-node)))])))
)