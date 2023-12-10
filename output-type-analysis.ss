(import 
    (chezscheme)
    (scheme-langserver) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver util dedupe)
    (scheme-langserver util io)

    (scheme-langserver analysis type domain-specific-language interpreter)
    (scheme-langserver analysis type domain-specific-language inner-type-checker)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import))

(define (step-library-identifiers current-library-node)
    (fold-left 
        (lambda (left file-node)
            (let* ([target-document (file-node-document file-node)]
                    [index-node-list (document-index-node-list target-document)]
                    [identifier-list (apply append (map import-from-external-index-node index-node-list))]
                    [path (file-node-path file-node)])
                (pretty-print path)
                (append left 
                    (apply append 
                        (map 
                            (lambda (identifier-reference)
                                (append 
                                    (list 
                                        (symbol->string (identifier-reference-identifier identifier-reference))
                                        (library-node-name->string (identifier-reference-library-identifier identifier-reference))
                                        path)
                                    (filter 
                                        (lambda (i) (not (equal? i "something? ")))
                                        (dedupe 
                                            (map type:interpret->strings 
                                                (
                                                    ; type:recursive-interpret-result-list 
                                                    type:interpret-result-list 
                                                    (index-node-variable (identifier-reference-index-node identifier-reference)) 
                                                    (make-type:environment (document-substitution-list target-document))))))))
                            identifier-list)))))
        (apply append (map (lambda (i) (step-library-identifiers i)) (library-node-children current-library-node)))
        (library-node-file-nodes current-library-node)))

(let* ([target-path (car (command-line-arguments))] 
        [output-path (cadr (command-line-arguments))]
        [workspace (init-workspace target-path #t #f #t)]  
        [root-library-node (workspace-library-node workspace)])
    (write-lines 
        (step-library-identifiers root-library-node)
        output-path))