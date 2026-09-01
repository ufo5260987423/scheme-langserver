(library (scheme-langserver analysis identifier self-defined-rules srfi include-resolve)
  (export include-resolve-process)
  (import 
    (chezscheme) 
    (ufo-match-steer)

    (scheme-langserver util path)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier meta)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
(define (private:library-ancestor index-node)
  (if (null? index-node)
    #f
    (if (index-node-shared-reference index-node)
      (private:library-ancestor (index-node-parent index-node))
      (match-index-node index-node
        [('library . rest) index-node]
        [('define-library . rest) index-node]
        [else (private:library-ancestor (index-node-parent index-node))]))))

(define (include-resolve-process root-file-node root-library-node document index-node step-without-document)
  (let* ([current-absolute-path (uri->path (document-uri document))]
      [target-parent-index-node 
        (or (private:library-ancestor (index-node-parent index-node))
          (index-node-parent index-node))])
    (match-index-node index-node
      [(_ (:= index-node-expression (? string? file-name)))
        (let ([suffix file-name])
          (for-each 
            (lambda (target-file-node)
              (let ([target-document (file-node-document target-file-node)])
                (if (document-refreshable? target-document) 
                  (begin 
                    (document-ordered-reference-list-set! document (find-meta '(chezscheme)))
                    (step-without-document target-document)))
                (append-references-into-ordered-references-for 
                  document 
                  target-parent-index-node 
                  (document-ordered-reference-list target-document))))
            (search-end-with root-file-node suffix)))]
      [(_ lib-path-node (:= index-node-expression (? string? file-name)))
        (let ([suffix 
                (fold-left 
                  (lambda (l r) (string-append r "/" l)) 
                  file-name 
                  (reverse (map index-node-expression (index-node-children lib-path-node))))])
          (for-each 
            (lambda (target-file-node)
              (let ([target-document (file-node-document target-file-node)])
                (if (document-refreshable? target-document) 
                  (begin 
                    (document-ordered-reference-list-set! document (find-meta '(chezscheme)))
                    (step-without-document target-document)))
                (append-references-into-ordered-references-for 
                  document 
                  target-parent-index-node 
                  (document-ordered-reference-list target-document))))
            (search-end-with root-file-node suffix)))]
      [else '()])))
)
