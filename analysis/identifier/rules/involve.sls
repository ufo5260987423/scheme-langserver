(library (scheme-langserver analysis identifier rules involve)
  (export involve-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)

    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

(define (involve-process root-file-node document library-identifiers index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
        [expression (annotation-stripped ann)]
        [parent-index-node (index-node-parent index-node)]
        [current-absolute-path (uri->path (document-uri document))])
    (match expression
      [('involve (? string? path)) 
        (guard-for index-node 'involve '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
        (let ([target-file-node 
              (cond
                [(path-absolute? path) (walk-file root-file-node path)]
                [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) (directory-separator) (path-rest path)))]
                [else (walk-file root-file-node (string-append (path-parent current-absolute-path) (directory-separator) patht))])])
          (if (not (null? target-file-node))
            (let* ([document (file-node-document target-file-node)]
                [references (document-reference-list document)])
              (index-node-references-import-in-this-node-set! 
                parent
                (append 
                  (index-node-references-import-in-this-node parent)
                  references)))))])))
)