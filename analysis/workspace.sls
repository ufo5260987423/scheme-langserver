(library (scheme-langserver analysis workspace)
  (export 
    init-workspace
    init-virtual-file-system
    init-library-node
    init-document
    init-references
    make-workspace

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
    workspace-threaded?
    workspace-type-inference?
    workspace-top-environment
    workspace-undiagnosed-paths
    workspace-undiagnosed-paths-set!

    update-file-node-with-tail

    attach-new-file

    save-workspace-cache-for!)
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
    (scheme-langserver analysis identifier util)
    (scheme-langserver analysis identifier rules library-import)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis workspace-cache))

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
    (workspace-undiagnosed-paths-set! workspace-instance (apply append batches))
    workspace-instance))

(define (private:generate-facet identifier path)
  (case identifier
    [txt (generate-txt-file-filter)]
    [akku (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))]
    [else (generate-akku-acceptable-file-filter (string-append path "/.akku/list"))]))

(define (private:init-workspace-from-scratch path identifier top-environment threaded? type-inference? facet)
  (let* ([root-file-node (init-virtual-file-system path '() facet top-environment)]
      [root-library-node (init-library-node root-file-node top-environment)]
      [file-linkage (init-file-linkage root-file-node root-library-node top-environment)]
      [batches (get-init-reference-batches file-linkage)]
      [workspace-instance (make-workspace root-file-node root-library-node file-linkage facet threaded? type-inference? top-environment (apply append batches))])
    (init-references workspace-instance batches)
    workspace-instance))

(define (private:clear-file-node-diagnoses file-node)
  (when (not (file-node-folder? file-node))
    (document-diagnoses-set! (file-node-document file-node) '()))
  (for-each private:clear-file-node-diagnoses (file-node-children file-node)))

(define (private:prepare-workspace-payload workspace)
  ;; 1. Clear document diagnoses (runtime state, must not be persisted)
  (private:clear-file-node-diagnoses (workspace-file-node workspace))
  ;; 2. Clear workspace undiagnosed-paths
  (workspace-undiagnosed-paths-set! workspace '())
  ;; 3. Clear file-linkage path->id-map (equal-hashtable cannot be decoded)
  (let ([linkage (workspace-file-linkage workspace)])
    (when linkage
      (file-linkage-path->id-map-set! linkage (make-eq-hashtable))))
  ;; Return serializable alist payload
  `((file-node . ,(workspace-file-node workspace))
    (library-node . ,(workspace-library-node workspace))
    (file-linkage . ,(workspace-file-linkage workspace))
    (threaded? . ,(workspace-threaded? workspace))
    (type-inference? . ,(workspace-type-inference? workspace))
    (top-environment . ,(workspace-top-environment workspace))
    (undiagnosed-paths . ,(workspace-undiagnosed-paths workspace))))

(define (rebuild-workspace-from-payload payload facet threaded?)
  (let ([file-node (cdr (assq 'file-node payload))]
        [library-node (cdr (assq 'library-node payload))]
        [file-linkage (cdr (assq 'file-linkage payload))]
        [type-inference? (cdr (assq 'type-inference? payload))]
        [top-environment (cdr (assq 'top-environment payload))]
        [undiagnosed-paths (cdr (assq 'undiagnosed-paths payload))])
    (make-workspace file-node library-node file-linkage facet threaded? type-inference? top-environment undiagnosed-paths)))

(define (private:file-content-changed? path cached-text)
  (or (not (file-exists? path))
      (guard (e [else #t])
        (let ([disk-text (call-with-input-file path get-string-all)])
          (not (string=? cached-text disk-text))))))

(define (private:cache-consistency-check workspace-instance)
  (let loop ([node (workspace-file-node workspace-instance)])
    (let ([doc (file-node-document node)])
      (or (and (document? doc)
               (private:file-content-changed? (file-node-path node) (document-text doc)))
          (ormap loop (file-node-children node))))))

(define (private:try-load-workspace-cache cache-path path identifier top-environment threaded? type-inference?)
  (and cache-path
       (workspace-cache-available? cache-path)
       (guard (ex (else #f))
         (let ([payload (load-workspace-cache cache-path identifier top-environment)])
           (let ([workspace-instance
                   (rebuild-workspace-from-payload
                     payload
                     (private:generate-facet identifier path)
                     threaded?)])
             ;; If any file on disk differs from the cached text, fall back to
             ;; a full refresh.  This is conservative but correct; incremental
             ;; refresh can be added later.
             (if (private:cache-consistency-check workspace-instance)
               (refresh-workspace workspace-instance)
               workspace-instance))))))

(define (save-workspace-cache-for! workspace cache-path identifier top-environment)
  (save-workspace-cache! (private:prepare-workspace-payload workspace) cache-path identifier top-environment))

(define init-workspace
  (case-lambda 
    [(path) (init-workspace path 'akku 'r6rs #f #f #f)]
    [(path threaded?) (init-workspace path 'akku 'r6rs threaded? #f #f)]
    [(path threaded? type-inference?) (init-workspace path 'akku 'r6rs threaded? type-inference? #f)]
    [(path identifier threaded? type-inference?) (init-workspace path identifier 'r6rs threaded? type-inference? #f)]
    [(path identifier top-environment threaded? type-inference?) 
      (init-workspace path identifier top-environment threaded? type-inference? #f)]
    [(path identifier top-environment threaded? type-inference? cache-path)
      (init-workspace path identifier top-environment threaded? type-inference? (private:generate-facet identifier path) cache-path)]
    [(path identifier top-environment threaded? type-inference? facet cache-path)
      (init-workspace-cache-registry!)
      (or (private:try-load-workspace-cache cache-path path identifier top-environment threaded? type-inference?)
          (private:init-workspace-from-scratch path identifier top-environment threaded? type-inference? facet))]))

;; head -[linkage]->files
;; for single file
;; import 
;; init define let ...
;; export
(define (init-references workspace-instance target-paths) 
  (for-each 
    (lambda (batch)
      (let ([paths (filter string? batch)])
        (if (workspace-threaded? workspace-instance)
          ;; Cancel-barrier: did-change may mark completion/hover/definition
          ;; tasks as stop?=#t in the tickal-task-list. Their expire callbacks
          ;; will try to acquire workspace-mutex. Holding it for the entire
          ;; batch ensures expire cannot interrupt the engine mid-analysis,
          ;; which would leave document-* fields in an inconsistent state.
          (with-mutex (workspace-mutex workspace-instance)
            (let ([path+syntax-pairs
                (map
                  (lambda (path)
                    (let* ([current-file-node (walk-file (workspace-file-node workspace-instance) path)]
                        [document (file-node-document current-file-node)]
                        [index-node-list (document-index-node-list document)]
                        [syntax-diagnoses 
                          (filter (lambda (d) (string-prefix? "Syntax error:" (cadddr d))) 
                            (document-diagnoses document))])
                      (document-diagnoses-set! document '())
                      (clear-references-for (car index-node-list))
                      (cons path syntax-diagnoses)))
                  paths)])
              (threaded-map 
                (lambda (pair) 
                  (let* ([target-path (car pair)]
                      [syntax-diagnoses (cdr pair)]
                      [current-file-node (walk-file (workspace-file-node workspace-instance) target-path)]
                      [document (file-node-document current-file-node)])
                    (try 
                      (private-init-references workspace-instance target-path syntax-diagnoses)
                      (except c
                        [(condition? c)
                          (append-new-diagnoses document 
                            `(0 0 1 ,(string-append "Analysis error: " 
                                (with-output-to-string (lambda () (pretty-print c)))) 
                                "analysis" "analysis-error"))
                          '()]
                        [else 
                          (append-new-diagnoses document 
                            `(0 0 1 ,(string-append "Analysis error: " 
                                (with-output-to-string (lambda () (pretty-print c)))) 
                                "analysis" "analysis-error"))
                          '()]))))
                path+syntax-pairs)))
          (begin
            (for-each
              (lambda (path)
                (let* ([current-file-node (walk-file (workspace-file-node workspace-instance) path)]
                    [document (file-node-document current-file-node)]
                    [index-node-list (document-index-node-list document)]
                    [syntax-diagnoses 
                      (filter (lambda (d) (string-prefix? "Syntax error:" (cadddr d))) 
                        (document-diagnoses document))])
                  (document-diagnoses-set! document '())
                  (clear-references-for (car index-node-list))
                  (private-init-references workspace-instance path syntax-diagnoses)))
              paths)))))
    target-paths))

(define (private-init-references workspace-instance target-path . maybe-syntax-diagnoses)
  (let* ([current-file-node (walk-file (workspace-file-node workspace-instance) target-path)]
      [document (file-node-document current-file-node)]
      [index-node-list (document-index-node-list document)]
      [syntax-diagnoses 
        (if (null? maybe-syntax-diagnoses)
          (filter (lambda (d) (string-prefix? "Syntax error:" (cadddr d))) 
            (document-diagnoses document))
          (car maybe-syntax-diagnoses))])
    ; (pretty-print 'test0)
    ; (pretty-print target-path)
    (step (workspace-file-node workspace-instance) (workspace-library-node workspace-instance) (workspace-file-linkage workspace-instance) document)
    (process-library-identifier-excluded-references document)
    (private:check-unused-imports document)
    ; (pretty-print 'test1)
    (if (workspace-type-inference? workspace-instance)
      (try 
        (construct-substitutions-for document)
        (except c 
          [(or (string? c) (symbol? c))
            (append-new-diagnoses document `(0 0 2 ,(string-append "Type inference warning: " (if (string? c) c (symbol->string c))) "type" "type-inference-warning"))
            (warning 'init-warning0 target-path '(,c))]
          [(condition? c)
            (append-new-diagnoses document `(0 0 2 ,(string-append "Type inference warning: " (condition-message c)) "type" "type-inference-warning"))
            (warning 'init-warning1 target-path `(,(condition-who c) ,(condition-message c) ,(condition-irritants c)))]
          [else 
            (error 'init-error target-path '())])))
    (document-diagnoses-set! document (append syntax-diagnoses (document-diagnoses document)))
    (document-refreshable?-set! document #f)))

(define (private:mark-used-imports document)
  (let ([used-ht (make-eq-hashtable)])
    (let loop ([nodes (document-index-node-list document)] [in-import? #f])
      (for-each
        (lambda (node)
          (let ([expression (annotation-stripped (index-node-datum/annotations node))])
            (cond
              [(and (pair? expression) (eq? 'import (car expression)))
                (loop (index-node-children node) #t)]
              [in-import?
                (loop (index-node-children node) #t)]
              [else
                (if (and (null? (index-node-children node)) (symbol? expression))
                  (for-each
                    (lambda (ref)
                      (if (not (eq? (identifier-reference-document ref) document))
                        (eq-hashtable-set! used-ht ref #t)))
                    (find-available-references-for document node expression)))
                (loop (index-node-children node) #f)])))
        nodes))
    used-ht))

(define (private:check-unused-imports document)
  (let* ([used-ht (private:mark-used-imports document)]
      [seen (make-eq-hashtable)])
    (let loop ([nodes (document-index-node-list document)])
      (for-each
        (lambda (node)
          (let ([expression (annotation-stripped (index-node-datum/annotations node))])
            (if (and (pair? expression) (eq? 'import (car expression)))
              (for-each (lambda (child) (private:check-import-clause document child used-ht seen)) (cdr (index-node-children node))))
            (loop (index-node-children node))))
        nodes))))

(define (private:check-import-clause document index-node used-ht seen)
  (let ([expression (annotation-stripped (index-node-datum/annotations index-node))])
    (match expression
      [('only (library-identifier **1) (? symbol? identifier) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [idents identifier])
          (if (not (null? nodes))
            (let* ([current-node (car nodes)]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car idents) seen))
              (loop (cdr nodes) (cdr idents)))))]
      [('except (library-identifier **1) (? symbol? identifier) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [idents identifier])
          (if (not (null? nodes))
            (let* ([current-node (car nodes)]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car idents) seen))
              (loop (cdr nodes) (cdr idents)))))]
      [('rename (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [internal-names internal-name])
          (if (not (null? nodes))
            (let* ([current-node (cadr (index-node-children (car nodes)))]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car internal-names) seen))
              (loop (cdr nodes) (cdr internal-names)))))]
      [('alias (library-identifier **1) ((? symbol? external-name) (? symbol? internal-name)) **1)
        (let loop ([nodes (cddr (index-node-children index-node))] [internal-names internal-name])
          (if (not (null? nodes))
            (let* ([current-node (cadr (index-node-children (car nodes)))]
                [refs (index-node-references-import-in-this-node current-node)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document current-node (car internal-names) seen))
              (loop (cdr nodes) (cdr internal-names)))))]
      [('prefix (library-identifier **1) (? symbol? prefix-id))
        '()]
      [('for _ ...)
        '()]
      [(library-identifier **1)
        (let ([parent (index-node-parent index-node)])
          (if (index-node? parent)
            (let ([refs (index-node-references-import-in-this-node parent)])
              (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
                (private:append-unused-import-diagnose document index-node (library-identifier->string expression) seen)
                '()))
            '()))]
      [else '()])))

(define (private:append-unused-import-diagnose document index-node identifier seen)
  (if (not (eq-hashtable-contains? seen index-node))
    (begin
      (eq-hashtable-set! seen index-node #t)
      (append-new-diagnoses document
        `(,(index-node-start index-node) ,(index-node-end index-node) 2
          ,(string-append "Unused import: " (if (symbol? identifier) (symbol->string identifier) identifier))
          "import" "unused-import")))))

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
        (begin
          (document-diagnoses-set! target-document '())
          (map 
            (lambda (item) (init-index-node '() item)) 
            (source-file->annotations text (uri->path (document-uri target-document)) (consume-sps-auxiliary text) #t target-document)))])
;;For old dependency
    (map (lambda (document) (document-refreshable?-set! document #t))
      (map (lambda (path) (file-node-document (walk-file root-file-node path))) (dedupe (get-reference-path-to linkage (file-node-path target-file-node)))))

    (document-text-set! target-document text)
    (document-line-length-vector-set! target-document (text->line-length-vector text))
    (document-index-node-list-set! target-document new-index-nodes)
    (document-refreshable?-set! target-document #t)

    (let ([new-library-identifiers-list (get-library-identifiers-list (file-node-document target-file-node) (workspace-top-environment workspace-instance))])
      (if (not (equal? new-library-identifiers-list old-library-identifiers-list))
        (begin 
;; BEGIN: some file may change their library-identifier or even do not have library identifier, their should be process carefully.
          (for-each 
            (lambda (old-library-node)
              (library-node-file-nodes-set! 
                old-library-node 
                (filter 
                  (lambda (file-node)
                    (not (equal? (file-node-path target-file-node) (file-node-path file-node))))
                  (library-node-file-nodes old-library-node)))
              (if (and (null? (library-node-file-nodes old-library-node)) 
                  (null? (library-node-children old-library-node))
                  (not (null? (library-node-parent old-library-node))))
                (delete-library-node-from-tree old-library-node)))
            old-library-node-list)
;; END
          (for-each 
            (lambda (library-identifiers)
              (make-library-node library-identifiers root-library-node target-file-node))
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
        (let ([target-path (file-node-path target-file-node)])
          (workspace-undiagnosed-paths-set! workspace-instance 
            (ordered-dedupe (merge string<? (workspace-undiagnosed-paths workspace-instance) (sort string<? `(,target-path))) string=?))
          (init-references workspace-instance `((,target-path))))
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
      (if (and (not (file-directory? path)) (file-symbolic-link? path))
        '()
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
          node))
      '())]))

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
                (let ([prefix-path
                      (find (lambda (p) (string-prefix? p path))
                        (map 
                          (lambda (p) (string-append (file-node-path parent) (string (directory-separator)) p))
                          (directory-list (file-node-path parent))))])
                  (if prefix-path
                    (let ([new-node (init-virtual-file-system prefix-path parent my-filter top-environment)])
                      (file-node-children-set! parent `(,@(file-node-children parent) ,new-node))
                      (attach-new-file path new-node my-filter top-environment))
                    '()))))]
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
    (cond 
      [(string? s) 
        (let ([d (make-document uri s (find-meta meta-lib top-environment))])
          (document-diagnoses-set! d '())
          (document-index-node-list-set! d (map (lambda (item) (init-index-node '() item)) (source-file->annotations s path (consume-sps-auxiliary s) #t d))) 
          d)]
      [(eof-object? s) 
        (make-document uri "" (find-meta meta-lib top-environment))]
      [else '()])))

(define init-library-node
  (case-lambda 
    [(file-node) (init-library-node file-node 'r6rs (make-library-node '() '() '() '()) )]
    [(file-node top-environment) (init-library-node file-node top-environment (make-library-node '() '() '() '()))]
    [(file-node top-environment root-library-node)
      (if (file-node-folder? file-node)
        (for-each 
          (lambda (child-node) (init-library-node child-node top-environment root-library-node))
          (file-node-children file-node))
        (let ([library-identifiers-list (get-library-identifiers-list (file-node-document file-node) top-environment)])
          (if (null? library-identifiers-list)
            (make-library-node '() root-library-node file-node)
            (map 
              (lambda (library-identifiers) (make-library-node library-identifiers root-library-node file-node))
              library-identifiers-list))))
      root-library-node]))
)
