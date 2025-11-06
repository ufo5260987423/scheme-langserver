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
    workspace-mutex
    workspace-library-node
    workspace-library-node-set!
    workspace-file-linkage
    workspace-facet
    workspace-type-inference?
    workspace-top-environment
    workspace-undiagnosed-paths
    workspace-undiagnosed-paths-set!

    update-file-node-with-tail

    attach-new-file
    remove-new-file)
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

    (immutable mutex)

    (immutable facet)
    ;only for identifer catching and type inference
    (immutable threaded?)
    (immutable type-inference?)
    (immutable top-environment)

    (mutable undiagnosed-paths))
  (protocol 
    (lambda (new)
      (case-lambda 
        [(file-node library-node file-linkage facet threaded? type-inference? top-environment)
          (new file-node library-node file-linkage (if threaded? (make-mutex) '()) facet threaded? type-inference? top-environment '())]
        [(file-node library-node file-linkage facet threaded? type-inference? top-environment undiagnosed-paths)
          (new file-node library-node file-linkage (if threaded? (make-mutex) '()) facet threaded? type-inference? top-environment undiagnosed-paths)]))))

(define (refresh-workspace workspace-instance)
  (let* ([path (file-node-path (workspace-file-node workspace-instance))]
      [root-file-node (init-virtual-file-system path '() (workspace-facet workspace-instance) (workspace-top-environment workspace-instance))]
      [root-library-node (init-library-node root-file-node (workspace-top-environment workspace-instance))]
      [file-linkage (init-file-linkage root-file-node root-library-node (workspace-top-environment workspace-instance))]
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
          [root-file-node (init-virtual-file-system path '() facet top-environment)]
          [root-library-node (init-library-node root-file-node top-environment)]
          [file-linkage (init-file-linkage root-file-node root-library-node top-environment)]
          [batches (get-init-reference-batches file-linkage)]
          [workspace-instance (make-workspace root-file-node root-library-node file-linkage facet threaded? type-inference? top-environment (apply append batches))])
        (init-references workspace-instance batches)
        workspace-instance)]))

;; head -[linkage]->files
;; for single file
;; import 
;; init define let ...
;; export
(define (init-references workspace-instance target-paths) 
  (fold-left 
    (lambda (left batch)
      (if (workspace-threaded? workspace-instance)
        (with-mutex (workspace-mutex workspace-instance)
          (threaded-map 
            (lambda (path) (private-init-references workspace-instance path))
            (filter string? batch)))
        (map 
          (lambda (path) (private-init-references workspace-instance path))
          (filter string? batch))))
    '()
    target-paths))

(define (private-init-references workspace-instance target-path)
  (let* ([current-file-node (walk-file (workspace-file-node workspace-instance) target-path)]
      [document (file-node-document current-file-node)]
      [index-node-list (document-index-node-list document)])
    (document-diagnoses-set! document '())
    ; (pretty-print 'test0)
    ; (pretty-print target-path)
    (step (workspace-file-node workspace-instance) (workspace-library-node workspace-instance) (workspace-file-linkage workspace-instance) document)
    (process-library-identifier-excluded-references document)
    ; (pretty-print 'test1)
    (if (workspace-type-inference? workspace-instance)
      (try 
        (construct-substitutions-for document)
        (except c 
          [(or (string? c) (symbol? c))
            (warning 'init-warning0 target-path '(,c))]
          [(condition? c)
            (warning 'init-warning1 target-path `(,(condition-who c) ,(condition-message c) ,(condition-irritants c)))]
          [else 
            (error 'init-error target-path '())])))
    (document-refreshable?-set! document #f)))

(define (update-file-node-with-tail workspace-instance target-file-node text)
  (let* ([root-file-node (workspace-file-node workspace-instance)]
      [linkage (workspace-file-linkage workspace-instance)]
      [target-document (file-node-document target-file-node)]
      [root-library-node (workspace-library-node workspace-instance)]
      [old-library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node) (workspace-top-environment workspace-instance))]
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

    (let ([new-library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node) (workspace-top-environment workspace-instance))])
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
                (make-library-node library-identifiers root-library-node target-file-node)))
            new-library-identifiers-list)
          (workspace-file-linkage-set! workspace-instance (init-file-linkage root-file-node root-library-node (workspace-top-environment workspace-instance)))
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
        [library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node) (workspace-top-environment workspace-instance))])
      (if (null? library-identifiers-list)
        (init-references workspace-instance `((,(file-node-path target-file-node))))
        (let* ([path 
            (refresh-file-linkage&get-refresh-path 
              linkage root-library-node target-file-node 
              (document-index-node-list (file-node-document target-file-node)) 
              library-identifiers-list 
              (workspace-top-environment workspace-instance))]
            [path-aheadof `(,@(list-ahead-of path (file-node-path target-file-node)) ,(file-node-path target-file-node))]
            [refreshable-path (filter (lambda (single) (document-refreshable? (file-node-document (walk-file root-file-node single)))) path-aheadof)]
            ;target-file-node may don't have library-identifiers-list
            [refreshable-batches (shrink-paths linkage refreshable-path)])
          (workspace-undiagnosed-paths-set! workspace-instance (ordered-dedupe (merge string<? (workspace-undiagnosed-paths workspace-instance) (sort string<? path)) string=?))
          (init-references workspace-instance refreshable-batches))))))

(define init-virtual-file-system
  (case-lambda
    [(path parent my-filter) (init-virtual-file-system path parent my-filter 'r6rs)]
    [(path parent my-filter top-environment)
      (if (my-filter path)
      (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [document 
            (if folder? 
              '() 
              (init-document path top-environment))]
          [node (make-file-node path name parent folder? '() document)]
          [children (if folder?
              (map 
                (lambda (p) 
                  (init-virtual-file-system 
                    (string-append path 
                      (if (string-suffix? (string (directory-separator)) path)
                        ""
                        (string (directory-separator)))
                      p) 
                    node 
                    my-filter
                    top-environment)) 
                (directory-list path))
              '())])
        (file-node-children-set! node (filter (lambda (p) (not (null? p))) children)) 
        node)
      '())]))

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

(define attach-new-file
  (case-lambda
    [(path parent my-filter) (attach-new-file path parent my-filter 'r6rs)]
    [(path parent my-filter top-environment)
      (let ([f (walk-file parent path)])
        (cond 
          [(not (my-filter path)) '()]
          [(not (file-exists? path)) '()]
          [(not (null? f)) f]
          [(file-node-folder? parent)
            (let ([maybe-parent 
                  (find (lambda (child) (string-prefix? (file-node-path child) path))
                    (file-node-children parent))])
              (if maybe-parent
                (attach-new-file path maybe-parent my-filter top-environment)
                (let ([new-node
                      (init-virtual-file-system 
                        (find (lambda (p) (string-prefix? p path))
                          (map 
                            (lambda (p) (string-append (file-node-path parent) (string (directory-separator)) p))
                            (directory-list (file-node-path parent))))
                        parent my-filter top-environment)])
                  (file-node-children-set! parent `(,@(file-node-children parent) ,new-node))
                  new-node)))]
          [else 
            (let* ([name (path->name path)] 
                [document (init-document path top-environment)]
                [node (make-file-node path name parent #f '() document)])
              (file-node-children-set! parent `(,@(file-node-children parent) ,node))
              node)]))]))

(define (init-document path top-environment)
  (let ([uri (path->uri path)]
      [s (read-string path)]
      [meta-lib (case top-environment
                  ['r7rs '(scheme base)]
                  ['s7 '(s7)]
                  ['goldfish '(s7)]
                  [else '(chezscheme)])])
    (try
      (cond 
        [(string? s) 
          (let ([d (make-document uri s (find-meta meta-lib top-environment))])
            (document-index-node-list-set! d (map (lambda (item) (init-index-node '() item)) (source-file->annotations path))) 
            d)]
        [(eof-object? s) 
          (make-document uri "" (find-meta meta-lib top-environment))]
        [else '()])
      (except c
        [(equal? c 'can-not-tolerant) '()]
        [else '()]))))

(define init-library-node
  (case-lambda 
    [(file-node) (init-library-node file-node 'r6rs (make-library-node '() '() '() '()) )]
    [(file-node top-environment) (init-library-node file-node top-environment (make-library-node '() '() '() '()))]
    [(file-node top-environment root-library-node)
      (if (file-node-folder? file-node)
        (map 
          (lambda (child-node) (init-library-node child-node top-environment root-library-node))
          (file-node-children file-node))
        (map 
          (lambda (library-identifiers) (make-library-node library-identifiers root-library-node file-node))
          (get-library-identifiers-list (file-node-document file-node) top-environment)))
      root-library-node]))
)
