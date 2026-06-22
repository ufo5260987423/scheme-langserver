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

(define (detect-file-filter path)
  (if (file-directory? (string-append path "/.akku"))
    'akku
    'txt))

(define (parse-library-name str)
  (read (open-string-input-port str)))

(define (recursive-top identifier-reference)
  (if (null? (identifier-reference-parents identifier-reference))
    `(,identifier-reference)
    (apply append (map recursive-top (identifier-reference-parents identifier-reference)))))

(define (dedupe-identifiers identifier-list)
  (let ([seen (make-eq-hashtable)])
    (filter
      (lambda (ref)
        (let ([id (identifier-reference-identifier ref)])
          (if (hashtable-ref seen id #f)
            #f
            (begin
              (hashtable-set! seen id #t)
              #t))))
      identifier-list)))

(define (write-identifier-types! identifier-reference port)
  (write-string "identifier:\t" port)
  (write-string (symbol->string (identifier-reference-identifier identifier-reference)) port)
  (write-string "\n" port)
  (map 
    (lambda (s)
      (write-string "type:\t\t" port)
      (write-string s port)
      (write-string "\n" port))
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
                  (type:interpret-result-list 
                    (identifier-reference-index-node ir))))))
          (recursive-top identifier-reference))))))

(define (step-library-identifiers current-library-node port)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (null? file-nodes)
      (map (lambda (c) (step-library-identifiers c port)) (library-node-children current-library-node))
      (let* ([file-node (car file-nodes)]
          [target-document (file-node-document file-node)]
          [index-node-list (document-index-node-list target-document)]
          [identifier-list (apply append 
                              (map 
                                (lambda (index-node) 
                                  (import-from-external-index-node target-document index-node)) 
                                index-node-list))]
          [library-name (string-append "(" (library-node-name->string current-library-node) ")")]
          [path (file-node-path file-node)])
        (let ([deduped (dedupe-identifiers identifier-list)])
          (unless (null? deduped)
            (pretty-print library-name)
            (pretty-print path)
            (write-string "library:\t" port)
            (write-string library-name port)
            (write-string "\n" port)
            (write-string "path:\t\t" port)
            (write-string path port)
            (write-string "\n" port)
            (map (lambda (ref) (write-identifier-types! ref port)) deduped)))
        (loop (cdr file-nodes))))))

(define (step-single-library library-node port)
  (let ([library-name (string-append "(" (library-node-name->string library-node) ")")]
        [file-nodes (library-node-file-nodes library-node)])
    (for-each
      (lambda (file-node)
        (let* ([path (file-node-path file-node)]
               [doc (file-node-document file-node)]
               [index-node-list (document-index-node-list doc)]
               [identifier-list (apply append
                               (map (lambda (index-node)
                                      (import-from-external-index-node doc index-node))
                                    index-node-list))])
          (let ([deduped (dedupe-identifiers identifier-list)])
            (unless (null? deduped)
              (pretty-print library-name)
              (pretty-print path)
              (write-string "library:\t" port)
              (write-string library-name port)
              (write-string "\n" port)
              (write-string "path:\t\t" port)
              (write-string path port)
              (write-string "\n" port)
              (for-each (lambda (ref) (write-identifier-types! ref port)) deduped)))))
      file-nodes)))

(let* ([args (command-line-arguments)]
       [argc (length args)])
  (cond
    [(= argc 2)
     (let ([target-path (car args)]
           [output-path (cadr args)])
       (let* ([file-filter (detect-file-filter target-path)]
              [workspace (init-workspace target-path file-filter 'r6rs #f #t)]
              [root-library-node (workspace-library-node workspace)])
         (call-with-output-file output-path
           (lambda (port)
             (step-library-identifiers root-library-node port)))))]
    [(= argc 3)
     (let ([target-path (car args)]
           [library-name-string (cadr args)]
           [output-path (caddr args)])
       (let* ([name-list (parse-library-name library-name-string)]
              [file-filter (detect-file-filter target-path)]
              [workspace (init-workspace target-path file-filter 'r6rs #f #t)]
              [root-library-node (workspace-library-node workspace)]
              [target-library-node (walk-library name-list root-library-node)])
         (if (null? target-library-node)
           (begin
             (display (string-append "Library not found: " library-name-string "\n"))
             (exit 1))
           (call-with-output-file output-path
             (lambda (port)
               (step-single-library target-library-node port))))))]
    [else
      (display "Usage:\n")
      (display "  Single library: scheme --script output-type-analysis.ss <dir> <lib-name> <out>\n")
      (display "  All libraries:  scheme --script output-type-analysis.ss <dir> <out>\n")
      (exit 1)]))
