(library (scheme-langserver analysis identifier rules library-involve)
  (export library-involve-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)

    (scheme-langserver analysis identifier rules lambda)
    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules lambda-define)
    (scheme-langserver analysis identifier rules lambda-import)
    )

(define (library-involve-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)])
    (match expression
      [('library (library-identifiers **1) _ **1 ) 
        (map 
          (lambda (child-node) (match-involve root-file-node document library-identifiers child-node))
          (index-node-children index-node))])
      index-node))
(define (match-involve root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [current-absolute-path (uri->path (document-uri document))])
    (match expression
      [('involve (? string? path)) 
        (guard-for index-node 'involve '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (let ([target-file-node 
              (cond
                [(path-absolute? path) (walk-file root-file-node path)]
                [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" patht))])])
          (if (not (null? target-file-node))
            (let* ([document (file-node-document target-file-node)])
            '()
            )
          ))
      ])))
)