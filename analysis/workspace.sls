(library (scheme-langserver analysis workspace)
  (export 
    init-workspace
    init-virtual-file-system
    init-library-node
    init-index-node
    init-document
    init-references

    refresh-workspace
    refresh-workspace-for

    workspace?
    workspace-file-node
    workspace-file-node-set!
    workspace-library-node
    workspace-library-node-set!
    workspace-file-linkage

    with-workspace-write
    with-workspace-read

    pick
    generate-library-node)
  (import 
    (ufo-match)
    (chezscheme) 
    (only (srfi :13 strings) string-suffix?)

    (scheme-langserver util path)
    (scheme-langserver util try)
    (scheme-langserver util io)
    (scheme-langserver util synchronize)

    (scheme-langserver analysis util)
    
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis tokenizer)
    (scheme-langserver analysis identifier rules define-record-type)
    (scheme-langserver analysis identifier rules library-define)
    (scheme-langserver analysis identifier rules library-export)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis identifier rules lambda)
    (scheme-langserver analysis identifier rules syntax)
    (scheme-langserver analysis identifier rules let)
    (scheme-langserver analysis identifier rules load)

    (scheme-langserver analysis package-manager akku)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

(define-record-type workspace
  (fields
    (mutable file-node)
    (mutable library-node)
    (mutable file-linkage)
    (immutable facet)
    (immutable lock))
  (protocol
    (lambda (new)
      (lambda (file-node library-node file-linkage facet)
        (new file-node library-node file-linkage facet (make-reader-writer-lock))))))

(define-syntax with-workspace-write
    (syntax-rules () [(_ workspace e0 e1 ...) 
      (with-lock-write (workspace-lock workspace) e0 e1 ...) ]))

(define-syntax with-workspace-read
    (syntax-rules () [(_ workspace e0 e1 ...) 
      (with-lock-read (workspace-lock workspace) e0 e1 ...) ]))

(define (refresh-workspace workspace-instance)
  (let* ([path (file-node-path (workspace-file-node workspace-instance))]
      [root-file-node (init-virtual-file-system path '() akku-acceptable-file?)]
      [root-library-node (init-library-node root-file-node)]
      [file-linkage (init-file-linkage root-library-node)]
      [paths (get-init-reference-path file-linkage)])
    (init-references root-file-node root-library-node paths)
    (workspace-file-node-set! workspace-instance root-file-node)
    (workspace-library-node-set! workspace-instance root-library-node)
    (workspace-file-linkage-set! workspace-instance file-linkage)
    workspace-instance))

(define init-workspace
  (case-lambda 
    [(path) (init-workspace path 'akku )]
    [(path identifier) 
      (cond 
        [(equal? 'akku identifier) 
          (let* ([root-file-node (init-virtual-file-system path '() akku-acceptable-file?)]
              [root-library-node (init-library-node root-file-node)]
              [file-linkage (init-file-linkage root-library-node)]
              [paths (get-init-reference-path file-linkage)])
        ; (display "aaa")
        ; (newline)
            ; (init-references root-file-node root-library-node paths)
        ; (display "eee")
        ; (newline)
            (make-workspace root-file-node root-library-node file-linkage identifier))])]))

;; head -[linkage]->files
;; for single file
;; import 
;; init define let ...
;; export
(define (init-references root-file-node root-library-node target-paths)
  (let loop ([paths target-paths])
    (if (not (null? paths))
      (let* ([current-file-node (walk-file root-file-node (car paths))]
            [document (file-node-document current-file-node)]
            [index-node-list (document-index-node-list document)])
        (document-reference-list-set! document '())
        (map 
          (lambda (index-node)
            (clear-references-for index-node)
          ; (pretty-print "bbb")
            (import-process root-file-node root-library-node document index-node)
          ; (pretty-print "ccc")
            (walk-and-process root-file-node document index-node)
            (export-process root-file-node document index-node)
          ; (pretty-print "ddd")
            (document-reference-list-set! 
              document 
              (append (document-reference-list document) (index-node-references-export-to-other-node index-node))))
          index-node-list)
        (loop (cdr paths))))))

;; target-file-node<-[linkage]-other-file-nodes
;; add read/write-lock to above model
;; add file-change-notification
(define (refresh-workspace-for workspace-instance target-file-node text path-mode)
  (if (not (equal? document-text text))
    (let* ([linkage (workspace-file-linkage workspace-instance)]
        [old-library-identifier-list (get-library-identifier-list target-file-node)]
        [root-file-node (workspace-file-node workspace-instance)]
        [root-library-node (workspace-library-node workspace-instance)]
        [old-library-node-list 
          (filter (lambda (item) (not (null? item)))
            (map (lambda (old-library-identifier) (walk-library old-library-identifier root-library-node))
              old-library-identifier-list))]
        [target-document (file-node-document target-file-node)]
        [target-path (uri->path (document-uri target-document))]
        [new-index-nodes (map (lambda (item) (init-index-node '() item)) (source-file->annotations text target-path))])
      (document-text-set! target-document text)
      (document-index-node-list-set! target-document new-index-nodes)
;; BEGINE: some file may change their library-identifier or even do not have library identifier, their should be process carefully.
      (map 
        (lambda (old-library-node)
          (library-node-file-nodes-set! 
            old-library-node 
            (filter 
              (lambda (file-node)
                (not (equal? (file-node-path target-file-node) (file-node-path file-node))))
              (library-node-file-nodes old-library-node)))
          (if (and (null? (library-node-file-nodes old-library-node)) 
              (null? (library-node-children old-library-node)))
            (delete-library-node-from-tree old-library-node)))
        old-library-node-list)
;; END
      (let ([new-library-identifier-list (get-library-identifier-list target-file-node)])
        (map 
          (lambda (library-identifier)
            (if (walk-library library-identifier root-library-node)
              (generate-library-node library-identifier root-library-node target-file-node)))
          new-library-identifier-list)
        (let ([path (refresh-file-linkage&get-refresh-path linkage root-library-node target-file-node new-index-nodes new-library-identifier-list)])
          (cond 
            [(equal? path-mode 'previous+single+tail) (init-references root-file-node root-library-node path)]
            [(equal? path-mode 'single) (init-references root-file-node root-library-node `(,target-path))]
            [(equal? path-mode 'previous+single) 
              (let loop ([loop-body path] [result-path '()])
                (if (not (null? loop-body))
                  (if (equal? target-path (car loop-body))
                    (init-references root-file-node root-library-node (append result-path `(,(car loop-body))))
                    (loop (cdr loop-body) (append result-path `(,(car loop-body)))))))]
            [(equal? path-mode 'previous) 
              (let loop ([loop-body path] [result-path '()])
                (if (not (null? loop-body))
                  (if (equal? target-path (car loop-body))
                    (init-references root-file-node root-library-node result-path)
                    (loop (cdr loop-body) (append result-path `(,(car loop-body)))))))]
            [(equal? path-mode 'single+tail) 
              (let loop ([loop-body path] [result-path '()] [flag #f])
                (if (null? loop-body)
                  (init-references root-file-node root-library-node (append `(,target-path) result-path))
                  (if flag
                    (loop (cdr loop-body) (append result-path `(,(car loop-body))) flag)
                    (loop (cdr loop-body) result-path (equal? target-path (car loop-body))))))]
            [(equal? path-mode 'tail) 
              (let loop ([loop-body path] [result-path '()] [flag #f])
                (if (null? loop-body)
                  (init-references root-file-node root-library-node result-path)
                  (if flag
                    (loop (cdr loop-body) (append result-path `(,(car loop-body))) flag)
                    (loop (cdr loop-body) result-path (or flag (equal? target-path (car loop-body)))))))]
            [else (raise 'illegle-path-mode)]))))))

;; rules must be run as ordered
(define (walk-and-process root-file-node document index-node)
  ;;1
  (define-process root-file-node document index-node)
  (define-record-type-process root-file-node document index-node)
  ;;2
  (let-process root-file-node document index-node)
  (lambda-process root-file-node document index-node)
  (syntax-process root-file-node document index-node)
  (load-process root-file-node document index-node)

  (map 
    (lambda (child-index-node) 
      (walk-and-process root-file-node document child-index-node)) 
    (index-node-children index-node)))

(define (init-virtual-file-system path parent my-filter)
  (if (my-filter path)
    (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [document 
            (if folder? 
              '() 
              (init-document path))]
          [node (make-file-node path name parent folder? '() document)]
          [children (if folder?
              (map 
                (lambda(p) 
                  (init-virtual-file-system 
                    (string-append path 
                      (if (string-suffix? (list->string (list (directory-separator))) path)
                        ""
                        (list->string (list (directory-separator))))
                      p) 
                    node 
                    my-filter)) 
                (directory-list path))
              '())])
      (file-node-children-set! node (filter (lambda(p) (not (null? p))) children))
      node)
    '()))

(define (init-document path)
  (let ([uri (path->uri path)])
    (make-document 
      uri 
      (read-string path) 
      (map (lambda (item) (init-index-node '() item)) (source-file->annotations path))
      '())))

(define init-library-node
  (case-lambda 
    [(file-node) (init-library-node file-node (make-library-node '() '() '() '())) ]
    [(file-node root-library-node) 
      (if (file-node-folder? file-node)
        (map 
          (lambda (child-node) (init-library-node child-node root-library-node))
          (file-node-children file-node))
        (map 
          (lambda (library-identifier) (generate-library-node library-identifier root-library-node file-node))
          (get-library-identifier-list file-node)))
      root-library-node]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pick
  (case-lambda 
    ([node start-position end-position] 
      (let ([pick-with-range (lambda (node-new) (pick node-new start-position end-position))])
        (cond
          ((and 
              (<= start-position (index-node-start node))
              (>= end-position (index-node-end node)))
            `(,node))
          (else (apply append (map pick-with-range (index-node-children node)))))))
    ([node position] 
        (let ([in? (and 
              (<= position (index-node-end node))
              (>= position (index-node-start node)))]
              [has-children? (not (null? (index-node-children node)))]
              [pick-with-position (lambda (node-new) (pick node-new position))])
          (cond
            [(and in? has-children?) (apply append (map pick-with-position (index-node-children node)))] 
            [(and in? (not has-children?)) `(,node)] 
            [else '()] )))))

(define (generate-library-node list-instance library-node virtual-file-node)
  (if (null? list-instance)
    (begin
      (library-node-file-nodes-set! library-node (append (library-node-file-nodes library-node) `(,virtual-file-node)))
      library-node)
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [child (find 
              (lambda (child-node) (equal? head (library-node-name child-node))) 
              (library-node-children library-node))])
      (generate-library-node
        rest 
        (if child
          child
          (let ([child (make-library-node head library-node '() '())])
            (library-node-children-set! library-node 
              (append (library-node-children library-node) `(,child)))
            child))
        virtual-file-node))))
)