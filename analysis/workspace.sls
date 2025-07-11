(library (scheme-langserver analysis workspace)
  (export 
    init-workspace
    init-virtual-file-system
    init-library-node
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
    workspace-facet
    workspace-type-inference?

    update-file-node-with-tail

    attach-new-file
    remove-new-file

    pick
    generate-library-node)
  (import 
    (ufo-match)
    (ufo-threaded-function)

    (chezscheme) 
    (only (srfi :13 strings) string-suffix? string-prefix?)

    (scheme-langserver util path)
    (ufo-try)
    (scheme-langserver util io)
    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util sub-list)

    (scheme-langserver analysis identifier meta)
    (scheme-langserver analysis type substitutions generator)

    (scheme-langserver analysis abstract-interpreter)
    (scheme-langserver analysis util)
    (scheme-langserver analysis tokenizer)
    
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter)

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
    ;only for identifer catching and type inference
    (immutable threaded?)
    (immutable type-inference?)))

(define (refresh-workspace workspace-instance)
  (let* ([path (file-node-path (workspace-file-node workspace-instance))]
      [root-file-node (init-virtual-file-system path '() (generate-akku-acceptable-file-filter (string-append path "/.akku/list")))]
      [root-library-node (init-library-node root-file-node)]
      [file-linkage (init-file-linkage root-file-node root-library-node)]
      [batches (get-init-reference-batches file-linkage)])
    (init-references workspace-instance batches)
    (workspace-file-node-set! workspace-instance root-file-node)
    (workspace-library-node-set! workspace-instance root-library-node)
    (workspace-file-linkage-set! workspace-instance file-linkage)
    workspace-instance))

(define init-workspace
  (case-lambda 
    [(path) (init-workspace path 'akku 'r6rs #f #f)]
    [(path threaded?) (init-workspace path 'akku 'r6rs threaded? #f)]
    [(path threaded? type-inference?) (init-workspace path 'akku 'r6rs threaded? type-inference?)]
    [(path identifier threaded? type-inference?) (init-workspace path identifier 'r6rs threaded? type-inference?)]
    [(path identifier top-environment threaded? type-inference?)
    ;; (pretty-print `(DEBUG: function: init-workspace))
      (let* ([facet 
            (case identifier
              [txt (generate-txt-file-filter)]
              [akku (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))]
              [else (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))])]
          [root-file-node (init-virtual-file-system path '() facet)]
          [root-library-node (init-library-node root-file-node)]
          [file-linkage (init-file-linkage root-file-node root-library-node)]
          [batches (get-init-reference-batches file-linkage)])
        (init-references root-file-node root-library-node file-linkage threaded? batches type-inference?)
        (make-workspace root-file-node root-library-node file-linkage facet threaded? type-inference?))]))

;; head -[linkage]->files
;; for single file
;; import 
;; init define let ...
;; export
(define init-references 
  (case-lambda 
    [(workspace-instance target-paths) 
      (init-references 
        (workspace-file-node workspace-instance)
        (workspace-library-node workspace-instance)
        (workspace-file-linkage workspace-instance)
        (workspace-threaded? workspace-instance)
        target-paths
        (workspace-type-inference? workspace-instance))]
    [(root-file-node root-library-node file-linkage threaded? target-paths type-inference?)
      (let loop ([paths target-paths])
        (if (not (null? paths))
          (let ([batch (car paths)])
            ((if threaded? threaded-map map)
              (lambda (path)
                (if (string? path)
                  (private-init-references root-file-node root-library-node file-linkage path type-inference?)))
              batch)
            (loop (cdr paths)))))]))

(define (private-init-references root-file-node root-library-node file-linkage target-path type-inference?)
  (let* ([current-file-node (walk-file root-file-node target-path)]
      [document (file-node-document current-file-node)]
      [index-node-list (document-index-node-list document)])
    ; (pretty-print 'test0)
    ; (pretty-print target-path)
    (step root-file-node root-library-node file-linkage document)
    (process-library-identifier-excluded-references document)
    ; (pretty-print 'test1)
    (if type-inference?
      (try 
        (construct-substitution-list-for document)
        (except c 
          [(or (string? c) (symbol? c))
            (pretty-print target-path)
            (pretty-print 'workspace-error)
            (pretty-print c)]
          [else 
            (display-condition c)
            (pretty-print target-path)
            (pretty-print 'workspace-error)
            (pretty-print `(format ,(condition-message c) ,@(condition-irritants c)))
            '()])))
    (document-refreshable?-set! document #f)))

(define (update-file-node-with-tail workspace-instance target-file-node text)
  (let* ([root-file-node (workspace-file-node workspace-instance)]
      [linkage (workspace-file-linkage workspace-instance)]
      [target-document (file-node-document target-file-node)]
      [root-library-node (workspace-library-node workspace-instance)]

      [old-library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node))]
      [old-library-node-list 
        (filter (lambda (item) (not (null? item)))
          (map (lambda (old-library-identifiers) (walk-library old-library-identifiers root-library-node))
            old-library-identifiers-list))]
      [new-index-nodes 
        (map 
          (lambda (item) (init-index-node '() item)) 
          (source-file->annotations text (uri->path (document-uri target-document))))])
;;For old dependency
    (map (lambda (document) (document-refreshable?-set! document #t))
      (map (lambda (path) (file-node-document (walk-file root-file-node path))) (dedupe (get-reference-path-to linkage (file-node-path target-file-node)))))

    (document-text-set! target-document text)
    (document-line-length-vector-set! target-document (text->line-length-vector text))
    (document-index-node-list-set! target-document new-index-nodes)

    (let ([new-library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node))])
      (if (not (equal? new-library-identifiers-list old-library-identifiers-list))
        (begin 
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
          (map 
            (lambda (library-identifiers)
              (if (walk-library library-identifiers root-library-node)
                (generate-library-node library-identifiers root-library-node target-file-node)))
            new-library-identifiers-list)
          (workspace-file-linkage-set! workspace-instance (init-file-linkage root-file-node root-library-node))
;;For new dependency
          (map (lambda (document) (document-refreshable?-set! document #t))
            (map (lambda (path) (file-node-document (walk-file root-file-node path))) 
              (dedupe (get-reference-path-to (workspace-file-linkage workspace-instance) (file-node-path target-file-node))))))))))

;; target-file-node<-[linkage]-other-file-nodes
(define (refresh-workspace-for workspace-instance target-file-node)
  (if (document-refreshable? (file-node-document target-file-node))
    (let* ([linkage (workspace-file-linkage workspace-instance)]
        [root-file-node (workspace-file-node workspace-instance)]
        [root-library-node (workspace-library-node workspace-instance)]
        [library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node))])
      (if (null? library-identifiers-list)
        (init-references workspace-instance `((,(file-node-path target-file-node))))
        (let* ([path (refresh-file-linkage&get-refresh-path linkage root-library-node target-file-node (document-index-node-list (file-node-document target-file-node)) library-identifiers-list)]
            [path-aheadof `(,@(list-ahead-of path (file-node-path target-file-node)) ,(file-node-path target-file-node))]
            [refreshable-path (filter (lambda (single) (document-refreshable? (file-node-document (walk-file root-file-node single)))) path-aheadof)]
            ;target-file-node may don't have library-identifiers-list
            [refreshable-batches (shrink-paths linkage refreshable-path)])
          (init-references workspace-instance refreshable-batches))))))

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
              (lambda (p) 
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


(define (remove-new-file path parent my-filter)
  (let ([f (walk-file parent path)])
    (cond 
      [(not (null? f)) '()]
      [else 
        (file-node-children-set!
          (file-node-parent f)
          (filter 
            (lambda (x) (not (equal? x f)))
            (file-node-children (file-node-parent f))))])))

(define (attach-new-file path parent my-filter)
  (let ([f (walk-file parent path)])
    (cond 
      [(not (file-exists? path)) '()]
      [(not (null? f)) f]
      [(null? (file-node-children parent)) 
        (init-virtual-file-system path parent my-filter)
        (walk-file parent path)]
      [else 
        (let ([maybe-parent 
              (find 
                (lambda (child)
                  (string-prefix? path (file-node-path child)))
                (file-node-children parent))])
          (if maybe-parent
            (attach-new-file path maybe-parent my-filter)
            '()))])))

(define (init-document path)
  (let ([uri (path->uri path)]
      [s (read-string path)])
    (try
      (cond 
        [(string? s) 
          (make-document 
            uri 
            s
            (map (lambda (item) (init-index-node '() item)) (source-file->annotations path))
            (find-meta '(chezscheme)))]
        [(eof-object? s) 
          (make-document 
            uri 
            ""
            '()
            (find-meta '(chezscheme)))]
        [else '()])
      (except c
        [(equal? c 'can-not-tolerant) '()]
        [else '()]))))

(define init-library-node
  (case-lambda 
    [(file-node) (init-library-node file-node (make-library-node '() '() '() '())) ]
    [(file-node root-library-node) 
      (if (file-node-folder? file-node)
        (map 
          (lambda (child-node) (init-library-node child-node root-library-node))
          (file-node-children file-node))
        (map 
          (lambda (library-identifiers) (generate-library-node library-identifiers root-library-node file-node))
          (get-library-identifiers-list (file-node-document file-node))))
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