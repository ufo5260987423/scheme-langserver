(library (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
  (export include-resolve-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util path)
    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
(define (include-resolve-process root-file-node root-library-node document index-node step-without-document)
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
                (let ([target-document (file-node-document target-file-node)])
                  (if (document-refreshable? target-document) 
                    (begin 
                      (document-ordered-reference-list-set! document (find-meta '(chezscheme)))
                      (step-without-document target-document)))
                  (append-references-into-ordered-references-for 
                    document 
                    parent-index-node 
                    (document-ordered-reference-list target-document))))
              (search-end-with root-file-node suffix)))]
        [else '()])
      (except c
        [else '()]))))
)