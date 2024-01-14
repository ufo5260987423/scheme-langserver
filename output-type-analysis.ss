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

(define (recursive-top identifier-reference)
  (if (null? (identifier-reference-parents identifier-reference))
    `(,identifier-reference)
    (apply append (map recursive-top (identifier-reference-parents identifier-reference)))))

(define (step-library-identifiers current-library-node port)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (write-string "library:\t" port)
    (write-string (library-node-name->string (library-node-name current-library-node)) port)
    (write-string "\n" port)
    (pretty-print (library-node-name current-library-node))
    (if (null? file-nodes)
      (map (lambda (c) (step-library-identifiers c port)) (library-node-children current-library-node))
      (let* ([file-node (car file-nodes)]
          [target-document (file-node-document file-node)]
          [index-node-list (document-index-node-list target-document)]
          [identifier-list (apply append (map import-from-external-index-node index-node-list))]
          [path (file-node-path file-node)])
        (pretty-print path)
        (write-string "path:\t\t" port)
        (write-string path port)
        (write-string "\n" port)
        (map 
          (lambda (identifier-reference)
            (write-string "identifier:\t" port)
            (write-string (symbol->string (identifier-reference-identifier identifier-reference)) port)
            (write-string "\n" port)
            (map 
              (lambda (s)
                (write-string "type:\t\t" port)
                (write-string s port)
                (write-string "\n" port))
              (filter 
                (lambda (i) (not (equal? i "something? ")))
                (dedupe 
                  (apply append 
                    (map 
                      (lambda (ir)
                        (apply append 
                          (map 
                            type:interpret->strings 
                            (if (or 
                                (null? (identifier-reference-document ir)) 
                                (not (null? (identifier-reference-type-expressions ir))))
                              (identifier-reference-type-expressions ir)
                              (type:recursive-interpret-result-list 
                                (index-node-variable (identifier-reference-index-node ir)) 
                                (make-type:environment (document-substitution-list (identifier-reference-document ir))))))))
                      (recursive-top identifier-reference)))))))
          identifier-list)
        (loop (cdr file-nodes))))))

(let* ([target-path (car (command-line-arguments))] 
    [output-path (cadr (command-line-arguments))]
    [workspace (init-workspace target-path #t #t #t)]  
    [root-library-node (workspace-library-node workspace)])
  (call-with-output-file output-path
    (lambda (port)
      (step-library-identifiers root-library-node port))))