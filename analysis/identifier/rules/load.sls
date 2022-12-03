(library (scheme-langserver analysis identifier rules load)
  (export load-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)
    (scheme-langserver util try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
(define (load-process root-file-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [library-identifier (get-nearest-ancestor-library-identifier index-node)]
      [parent-index-node (index-node-parent index-node)]
      [current-absolute-path (uri->path (document-uri document))])
    (try
      (match expression
        [('load(? string? path)) 
          (guard-for document index-node 'involve '(chezscheme) '(rnrs) '(rnrs base) '(scheme))
          (let ([target-file-node 
                (cond
                  ; [(not (string? path)) '()]
                  [(path-absolute? path) (walk-file root-file-node path)]
                  [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                  [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
            (if (not (null? target-file-node))
              (let* ([document (file-node-document target-file-node)]
                  [references 
                    (if (null? library-identifier)
                      (document-reference-list document)
                      (map 
                        (lambda (reference) 
                          (make-identifier-reference
                            (identifier-reference-identifier reference)
                            (identifier-reference-document reference)
                            (identifier-reference-index-node reference)
                            library-identifier
                            (identifier-reference-type reference)))
                        (document-reference-list document)))])
                (index-node-references-import-in-this-node-set! 
                  parent-index-node
                  (append 
                    (index-node-references-import-in-this-node parent-index-node)
                    references)))))]
        [else '()])
      (except c
        [else '()]))))
)