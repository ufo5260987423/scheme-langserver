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

(define get-library-identifiers-list
    (case-lambda
        [(document) (get-library-identifiers-list document 'r6rs)]
        [(document top-environment)
            (let [(func (case top-environment
                            ['r6rs
                                (lambda (index-node)
                                (match (annotation-stripped (index-node-datum/annotations index-node))
                                    [('library (name **1) _ ... ) name]
                                    [else '()]))]
                            ['r7rs
                                (lambda (index-node)
                                (match (annotation-stripped (index-node-datum/annotations index-node))
                                    [('define-library (name **1) _ ... ) name]
                                    [else '()]))]
                            ['s7
                                (lambda (index-node)
                                (match (annotation-stripped (index-node-datum/annotations index-node))
                                    [('define-library (name **1) _ ... ) name]
                                    [else '()]))]))]
                (if (null? document)
                    '()
                    (let ([index-node-list (document-index-node-list document)])
                        (dedupe
                            (map func index-node-list)))))]))

(define (get-nearest-ancestor-library-identifier index-node)
    (if (null? index-node)
        '()
        (match (annotation-stripped (index-node-datum/annotations index-node))
            [('library (name **1) _ ... ) name]
            [('define-library (name **1) _ ... ) name]
            [else (get-nearest-ancestor-library-identifier (index-node-parent index-node))])))
)