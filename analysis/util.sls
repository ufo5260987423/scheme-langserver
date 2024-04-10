(library (scheme-langserver analysis util)
  (export 
    do-nothing
    get-library-identifiers-list 
    get-nearest-ancestor-library-identifier)
  (import 
    (chezscheme) 
    
    (ufo-match)

    (scheme-langserver util dedupe)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

(define (do-nothing . fuzzy) (void))

(define (get-library-identifiers-list file-node)
    (let ([document (file-node-document file-node)])
        (if (null? document)
            '()
            (let ([index-node-list (document-index-node-list document)])
                (dedupe
                    (map 
                        (lambda (index-node)
                            (match (annotation-stripped (index-node-datum/annotations index-node))
                                [('library (name **1) _ ... ) name]
                                [('define-library (name **1) _ ... ) name]
                                [else '()]))
                        index-node-list))))))

(define (get-nearest-ancestor-library-identifier index-node)
    (if (null? index-node)
        '()
        (match (annotation-stripped (index-node-datum/annotations index-node))
            [('library (name **1) _ ... ) name]
            [('define-library (name **1) _ ... ) name]
            [else (get-nearest-ancestor-library-identifier (index-node-parent index-node))])))
)