(library (scheme-langserver analysis identifier rules load)
  (export load-process)
  (import 
    (chezscheme) 
    (ufo-match)

    (scheme-langserver util dedupe)
    (scheme-langserver util path)
    (ufo-try)

    (scheme-langserver analysis util)
    (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node))

;;todo more test
; todo: library process
(define (load-process root-file-node root-library-node document index-node)
  (let* ([ann (index-node-datum/annotations index-node)]
      [expression (annotation-stripped ann)]
      [library-identifier (get-nearest-ancestor-library-identifier index-node)]
      [parent-index-node (index-node-parent index-node)]
      [current-absolute-path (uri->path (document-uri document))])
    (try
      (match expression
        [(_ (? string? path)) 
          (let ([target-file-node 
                (cond
                  ; [(not (string? path)) '()]
                  [(path-absolute? path) (walk-file root-file-node path)]
                  [(equal? ".." (path-first path)) (walk-file root-file-node (string-append (path-parent (path-parent current-absolute-path)) "/" (path-rest path)))]
                  [else (walk-file root-file-node (string-append (path-parent current-absolute-path) "/" path))])])
            (if (not (null? target-file-node))
              (begin 
                (index-node-import-file-nodes-set! index-node `(,target-file-node))
                (let* ([target-document (file-node-document target-file-node)]
                    [references 
                      (if (null? library-identifier)
                        (document-ordered-reference-list target-document)
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
                          (document-ordered-reference-list target-document)))])
                  (append-references-into-ordered-references-for document parent-index-node references)))))]
        [else '()])
      (except c
        [else '()]))))
)