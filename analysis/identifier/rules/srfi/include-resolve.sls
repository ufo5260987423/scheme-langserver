(library (scheme-langserver analysis identifier rules srfi include-resolve)
  (export include-resolve-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util dedupe)
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
      [parent-index-node (index-node-parent index-node)]
      [current-absolute-path (uri->path (document-uri document))])
    (try
      (match expression
        [(_ ((? string? lib-path) **1) (? string? file-name))
          (let ([suffix (fold-left (lambda (l r) (string-append r "/" l)) file-name (reverse lib-path))])
            (map 
              (lambda (target-file-node)
                (let* ([target-document (file-node-document target-file-node)]
                    [references (document-reference-list document)])
                  (index-node-references-import-in-this-node-set! 
                    parent-index-node
                    (ordered-dedupe 
                      (sort-identifier-references
                        (append 
                          (index-node-references-import-in-this-node parent-index-node)
                          references))))))
              (search-end-with root-file-node suffix)))]
        [else '()])
      (except c
        [else '()]))))
)