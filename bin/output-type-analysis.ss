(import 
  (chezscheme)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)

  (scheme-langserver util dedupe)
  (scheme-langserver analysis type domain-specific-language interpreter)
  (scheme-langserver analysis type semantic-subtyping display)
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
  (put-string port "identifier:\t")
  (put-string port (symbol->string (identifier-reference-identifier identifier-reference)))
  (put-string port "\n")
  ;;P2: semantic compaction (same pipeline as hover): anti-chain
  ;;absorption is a semantic equality, so the report never loses
  ;;information -- dominated overloads collapse (e.g. the 73-signature
  ;;numeric-tower cross product of natural-order-compare). The final
  ;;dedupe on rendered strings covers candidates whose only observable
  ;;identity IS the rendering (bare index-node candidates are distinct
  ;;records yet print identically); hover does the same.
  (let* ([types
          (apply append
            (map
              (lambda (ir)
                (if (or
                    (null? (identifier-reference-document ir))
                    (not (null? (identifier-reference-type-expressions ir))))
                  (identifier-reference-type-expressions ir)
                  (type:interpret-result-list (identifier-reference-index-node ir))))
              (recursive-top identifier-reference)))]
      [limit 10])
    (call-with-values
      (lambda () (sem:compact-display types limit))
      (lambda (items total truncated?)
        (let ([lines
                (dedupe
                  (append
                    (apply append
                      (map
                        (lambda (item)
                          (map
                            (lambda (str)
                              (if (and (eq? 'rep (car item)) (> (caddr item) 1))
                                (string-append str "  (x" (number->string (caddr item)) ")")
                                str))
                            (type:interpret->strings (cadr item))))
                        items))
                    (if truncated?
                      (list (string-append "... " (number->string total) " candidates total"))
                      '())))])
          (for-each
            (lambda (str)
              (put-string port "type:\t\t")
              (put-string port str)
              (put-string port "\n"))
            lines))))))

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
            (put-string port "library:\t")
            (put-string port library-name)
            (put-string port "\n")
            (put-string port "path:\t\t")
            (put-string port path)
            (put-string port "\n")
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
              (put-string port "library:\t")
              (put-string port library-name)
              (put-string port "\n")
              (put-string port "path:\t\t")
              (put-string port path)
              (put-string port "\n")
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
