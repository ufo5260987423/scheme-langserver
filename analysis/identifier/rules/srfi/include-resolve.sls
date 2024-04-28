(library (scheme-langserver analysis identifier rules srfi include-resolve)
  (export include-resolve-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)
    (scheme-langserver util try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
(define (include-resolve-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [library-identifier (get-nearest-ancestor-library-identifier index-node)]
      [parent-index-node (index-node-parent index-node)]
      [current-absolute-path (uri->path (document-uri document))])
    (try
      (match expression
        [(_ ((? string? lib-path) **1) (? string? file-name))
          (let ([target-file-node (search-end-with root-file-node `(,@lib-path ,file-name))])
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
                            (identifier-reference-initialization-index-node reference)
                            library-identifier
                            (identifier-reference-type reference)
                            (identifier-reference-parents reference)
                            (identifier-reference-type-expressions reference)))
                        (document-reference-list document)))])
                (index-node-references-import-in-this-node-set! 
                  parent-index-node
                  (sort-identifier-references
                    (append 
                      (index-node-references-import-in-this-node parent-index-node)
                      references))))))]
        [else '()])
      (except c
        [else '()]))))
)