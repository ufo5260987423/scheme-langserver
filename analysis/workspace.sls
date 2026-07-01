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
    workspace-path->mtime-cache
    workspace-path->mtime-cache-set!

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

    (mutable undiagnosed-paths)
    (mutable path->mtime-cache))
  (protocol 
    (lambda (new)
      (case-lambda 
        [(file-node library-node file-linkage facet threaded? type-inference? top-environment)
          (new file-node library-node file-linkage (if threaded? (make-mutex) '()) facet threaded? type-inference? top-environment '() (make-hashtable string-hash equal?))]
        [(file-node library-node file-linkage facet threaded? type-inference? top-environment undiagnosed-paths)
          (new file-node library-node file-linkage (if threaded? (make-mutex) '()) facet threaded? type-inference? top-environment undiagnosed-paths (make-hashtable string-hash equal?))]
        [(file-node library-node file-linkage facet threaded? type-inference? top-environment undiagnosed-paths path->mtime-cache)
          (new file-node library-node file-linkage (if threaded? (make-mutex) '()) facet threaded? type-inference? top-environment undiagnosed-paths path->mtime-cache)]))))

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

(define (private:clear-non-serializable-fields! workspace)
  ;; Walk the reachable object graph and clear fields that contain procedures,
  ;; which Chez fasl-write cannot serialize.  Currently this covers:
  ;;   - index-node expansion-generator (procedure)
  ;;   - identifier-reference syntax-expander (procedure)
  (let ([seen (make-eq-hashtable)])
    (define (visit obj)
      (when (and (or (pair? obj) (vector? obj) (box? obj) (hashtable? obj) (record? obj))
                 (not (hashtable-ref seen obj #f)))
        (hashtable-set! seen obj #t)
        (cond
          [(index-node? obj)
           (index-node-expansion-generator-set! obj '())
           (visit (index-node-children obj))
           (visit (index-node-excluded-references obj))
           (visit (index-node-references-import-in-this-node obj))
           (visit (index-node-references-export-to-other-node obj))
           (visit (index-node-substitution-list obj))
           (visit (index-node-import-file-nodes obj))
           (visit (index-node-datum/annotations obj))]
          [(identifier-reference? obj)
           (identifier-reference-syntax-expander-set! obj #f)
           (visit (identifier-reference-parents obj))
           (visit (identifier-reference-type-expressions obj))
           (visit (identifier-reference-index-node obj))
           (visit (identifier-reference-initialization-index-node obj))]
          [(document? obj)
           (visit (document-index-node-list obj))
           (visit (document-ordered-reference-list obj))]
          [(file-node? obj)
           (unless (file-node-folder? obj)
             (visit (file-node-document obj)))
           (visit (file-node-children obj))]
          [(pair? obj)
           (visit (car obj))
           (visit (cdr obj))]
          [(vector? obj)
           (do ([i 0 (+ i 1)]) [(= i (vector-length obj))]
             (visit (vector-ref obj i)))]
          [(box? obj)
           (visit (unbox obj))]
          [(hashtable? obj)
           (let-values ([(keys vals) (hashtable-entries obj)])
             (visit keys)
             (visit vals))]
          [(record? obj)
           (let* ([rtd (record-rtd obj)]
                  [names (record-type-field-names rtd)])
             (do ([i 0 (+ i 1)]) [(= i (vector-length names))]
               (visit ((record-accessor rtd i) obj))))])))
    (visit (workspace-file-node workspace))
    (visit (workspace-library-node workspace))
    (visit (workspace-file-linkage workspace))))

(define (private:path->id-map->alist path->id-map)
  (let-values ([(keys values) (hashtable-entries path->id-map)])
    (let loop ([i 0] [result '()])
      (if (>= i (vector-length keys))
        result
        (loop (+ i 1) (cons (cons (vector-ref keys i) (vector-ref values i)) result))))))

(define (private:alist->path->id-map alist)
  (let ([ht (make-hashtable string-hash equal?)])
    (for-each
      (lambda (pair)
        (hashtable-set! ht (car pair) (cdr pair)))
      alist)
    ht))

(define (private:collect-file-mtimes root-file-node)
  (let ([result '()])
    (let loop ([node root-file-node])
      (if (file-node-folder? node)
        (for-each loop (file-node-children node))
        (guard (e [else #f])
          (let* ([path (file-node-path node)]
                 [mtime (file-modification-time path)])
            (set! result (cons (cons path (cons (time-second mtime) (time-nanosecond mtime))) result))))))
    result))

(define (private:mtime-matches? cached-mtime disk-mtime)
  (and cached-mtime
       (let ([cached-second (if (pair? cached-mtime) (car cached-mtime) (time-second cached-mtime))]
             [cached-nsec (if (pair? cached-mtime) (cdr cached-mtime) (time-nanosecond cached-mtime))])
         (and (= cached-second (time-second disk-mtime))
              (= cached-nsec (time-nanosecond disk-mtime))))))

(define (private:prepare-workspace-payload workspace)
  ;; 1. Clear document diagnoses (runtime state, must not be persisted)
  (private:clear-file-node-diagnoses (workspace-file-node workspace))
  ;; 2. Clear procedure-valued fields that Chez fasl-write cannot serialize
  (private:clear-non-serializable-fields! workspace)
  ;; 3. Clear workspace undiagnosed-paths
  (workspace-undiagnosed-paths-set! workspace '())
  ;; 4. Convert file-linkage path->id-map (equal-hashtable, not FASL-serializable)
  ;;    to an alist and store it separately in the payload.
  (let* ([linkage (workspace-file-linkage workspace)]
      [path->id-alist (private:path->id-map->alist (file-linkage-path->id-map linkage))]
      [path->mtime-alist (private:collect-file-mtimes (workspace-file-node workspace))])
    (file-linkage-path->id-map-set! linkage (make-eq-hashtable))
    ;; Return serializable alist payload
    `((file-node . ,(workspace-file-node workspace))
      (library-node . ,(workspace-library-node workspace))
      (file-linkage . ,linkage)
      (path->id-alist . ,path->id-alist)
      (path->mtime-alist . ,path->mtime-alist)
      (threaded? . ,(workspace-threaded? workspace))
      (type-inference? . ,(workspace-type-inference? workspace))
      (top-environment . ,(workspace-top-environment workspace))
      (undiagnosed-paths . ,(workspace-undiagnosed-paths workspace)))))

(define (rebuild-workspace-from-payload payload facet threaded?)
  (let ([file-node (cdr (assq 'file-node payload))]
      [library-node (cdr (assq 'library-node payload))]
      [file-linkage (cdr (assq 'file-linkage payload))]
      [path->id-alist (cdr (assq 'path->id-alist payload))]
      [path->mtime-alist (cdr (assq 'path->mtime-alist payload))]
      [type-inference? (cdr (assq 'type-inference? payload))]
      [top-environment (cdr (assq 'top-environment payload))]
      [undiagnosed-paths (cdr (assq 'undiagnosed-paths payload))])
    (file-linkage-path->id-map-set! file-linkage (private:alist->path->id-map path->id-alist))
    (let* ([path->mtime-cache (make-hashtable string-hash equal?)]
           [workspace-instance (make-workspace file-node library-node file-linkage facet threaded? type-inference? top-environment undiagnosed-paths path->mtime-cache)])
      (for-each
        (lambda (pair)
          (when (pair? pair)
            (hashtable-set! path->mtime-cache (car pair) (cdr pair))))
        (if (list? path->mtime-alist) path->mtime-alist '()))
      workspace-instance)))

(define (private:file-content-changed? path cached-text cached-mtime)
  ;; Fast path: if we have a cached mtime and it matches the disk, the file
  ;; has not changed.  Avoid the expensive read-string + string=? fallback.
  (if (guard (e [else #f])
        (private:mtime-matches? cached-mtime (file-modification-time path)))
    #f
    (or (not (file-exists? path))
        (guard (e [else #t])
          (let ([disk-text (read-string path)])
            (not (string=? cached-text disk-text)))))))

(define (private:collect-cached-file-paths workspace-instance)
  (let ([paths '()])
    (let loop ([node (workspace-file-node workspace-instance)])
      (if (file-node-folder? node)
        (for-each loop (file-node-children node))
        (set! paths (cons (file-node-path node) paths))))
    paths))

(define (private:collect-disk-file-paths root-path facet)
  (let ([paths '()]
        [sep (string (directory-separator))])
    (let collect ([dir root-path])
      (for-each
        (lambda (name)
          (unless (or (equal? name ".") (equal? name ".."))
            (let ([path (string-append dir sep name)])
              (if (file-directory? path)
                (collect path)
                (when (facet path)
                  (set! paths (cons path paths)))))))
        (directory-list dir)))
    paths))

(define (private:delete-library-node-if-empty! library-node)
  (when (and (null? (library-node-file-nodes library-node))
             (null? (library-node-children library-node))
             (not (null? (library-node-parent library-node))))
    (delete-library-node-from-tree library-node)))

(define (private:delete-file-node workspace-instance target-path)
  (let* ([root-file-node (workspace-file-node workspace-instance)]
         [root-library-node (workspace-library-node workspace-instance)]
         [linkage (workspace-file-linkage workspace-instance)]
         [file-node (walk-file root-file-node target-path)])
    (when (file-node? file-node)
      ;; 1. Remove from file-linkage
      (shrink-file-linkage! linkage target-path)
      ;; 2. Remove from library-node tree
      (let ([library-identifiers-list
              (get-library-identifiers-list (file-node-document file-node) (workspace-top-environment workspace-instance))])
        (for-each
          (lambda (library-identifiers)
            (let ([library-node (walk-library library-identifiers root-library-node)])
              (when (library-node? library-node)
                (library-node-file-nodes-set!
                  library-node
                  (filter
                    (lambda (fn) (not (equal? target-path (file-node-path fn))))
                    (library-node-file-nodes library-node)))
                (private:delete-library-node-if-empty! library-node))))
          library-identifiers-list))
      ;; 3. Remove from parent file-node children
      (let ([parent (file-node-parent file-node)])
        (file-node-children-set!
          parent
          (filter
            (lambda (child) (not (eq? child file-node)))
            (file-node-children parent)))))))

(define (private:build-path->file-node-ht root-file-node)
  (let ([ht (make-hashtable string-hash equal?)])
    (let walk ([node root-file-node])
      (if (file-node-folder? node)
        (for-each walk (file-node-children node))
        (hashtable-set! ht (file-node-path node) node)))
    ht))

(define (private:cache-consistency-check workspace-instance)
  ;; Returns three values: (changed-paths deleted-paths new-paths)
  (let* ([facet (workspace-facet workspace-instance)]
         [root-path (file-node-path (workspace-file-node workspace-instance))]
         [cached-paths (private:collect-cached-file-paths workspace-instance)]
         [disk-paths (private:collect-disk-file-paths root-path facet)]
         [cached-ht (make-hashtable string-hash equal?)]
         [disk-ht (make-hashtable string-hash equal?)]
         [path->mtime-cache (workspace-path->mtime-cache workspace-instance)]
         [path->file-node-ht (private:build-path->file-node-ht (workspace-file-node workspace-instance))]
         [changed '()]
         [deleted '()]
         [new '()])
    (for-each (lambda (p) (hashtable-set! cached-ht p #t)) cached-paths)
    (for-each (lambda (p) (hashtable-set! disk-ht p #t)) disk-paths)
    ;; Check cached paths: changed or deleted
    (for-each
      (lambda (cached-path)
        (if (hashtable-ref disk-ht cached-path #f)
          (let ([file-node (hashtable-ref path->file-node-ht cached-path #f)]
                [cached-mtime (hashtable-ref path->mtime-cache cached-path #f)])
            (when (and (file-node? file-node)
                       (private:file-content-changed?
                         cached-path
                         (document-text (file-node-document file-node))
                         cached-mtime))
              (set! changed (cons cached-path changed))))
          (set! deleted (cons cached-path deleted))))
      cached-paths)
    ;; Check disk paths: new files
    (for-each
      (lambda (disk-path)
        (unless (hashtable-ref cached-ht disk-path #f)
          (set! new (cons disk-path new))))
      disk-paths)
    (values changed deleted new)))

(define (private:apply-cache-incremental-refresh! workspace-instance changed-paths new-paths)
  (let* ([root-file-node (workspace-file-node workspace-instance)]
         [facet (workspace-facet workspace-instance)]
         [top-environment (workspace-top-environment workspace-instance)])
    ;; 1. Attach new files
    (let ([new-file-nodes
            (filter
              file-node?
              (map
                (lambda (path) (attach-new-file path root-file-node facet top-environment))
                new-paths))])
      ;; 2. Update changed files
      (for-each
        (lambda (path)
          (let ([file-node (walk-file root-file-node path)]
                [text (read-string path)])
            (when (and (file-node? file-node) (string? text))
              (update-file-node-with-tail workspace-instance file-node text))))
        changed-paths)
      ;; 3. Refresh changed and new files
      (for-each
        (lambda (file-node) (refresh-workspace-for workspace-instance file-node))
        (append
          new-file-nodes
          (filter
            file-node?
            (map (lambda (path) (walk-file root-file-node path)) changed-paths)))))))

(define (private:try-load-workspace-cache cache-path path identifier top-environment threaded? type-inference?)
  (and cache-path
       (workspace-cache-available? cache-path)
       (guard (ex (else #f))
         (let ([payload (load-workspace-cache cache-path identifier top-environment type-inference? threaded?)])
           (let ([workspace-instance
                   (rebuild-workspace-from-payload
                     payload
                     (private:generate-facet identifier path)
                     threaded?)])
             (let-values ([(changed deleted new) (private:cache-consistency-check workspace-instance)])
               (cond
                 [(and (null? changed) (null? deleted) (null? new))
                  workspace-instance]
                 [else
                   ;; Phase 3: true incremental refresh
                   (for-each
                     (lambda (target-path) (private:delete-file-node workspace-instance target-path))
                     deleted)
                   (private:apply-cache-incremental-refresh! workspace-instance changed new)
                   workspace-instance])))))))

(define (save-workspace-cache-for! workspace cache-path identifier top-environment type-inference? threaded?)
  (save-workspace-cache! (private:prepare-workspace-payload workspace) cache-path identifier top-environment type-inference? threaded?))

(define init-workspace
  (case-lambda 
    [(path) (init-workspace path 'akku 'r6rs #f #f #f)]
    [(path threaded?) (init-workspace path 'akku 'r6rs threaded? #f #f)]
    [(path threaded? type-inference?) (init-workspace path 'akku 'r6rs threaded? type-inference? #f)]
    [(path identifier threaded? type-inference?) (init-workspace path identifier 'r6rs threaded? type-inference? #f)]
    [(path identifier top-environment threaded? type-inference?) 
      (init-workspace path identifier top-environment threaded? type-inference? #f)]
    ;; The 6th argument is overloaded for backward compatibility:
    ;; - procedure => a custom facet filter (old behavior)
    ;; - string or #f => cache-path (new behavior), facet is generated from identifier
    [(path identifier top-environment threaded? type-inference? sixth)
      (if (procedure? sixth)
        (init-workspace path identifier top-environment threaded? type-inference? sixth #f)
        (init-workspace path identifier top-environment threaded? type-inference? (private:generate-facet identifier path) sixth))]
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
                      (when (not (null? index-node-list))
                        (clear-references-for (car index-node-list)))
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
                                (with-output-to-string (lambda () (display-condition c)))) 
                                "analysis" "analysis-error"))
                          '()]
                        [else 
                          (append-new-diagnoses document 
                            `(0 0 1 ,(string-append "Analysis error: " 
                                (with-output-to-string (lambda () (write c)))) 
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
                  (when (not (null? index-node-list))
                    (clear-references-for (car index-node-list))
                    (private-init-references workspace-instance path syntax-diagnoses))))
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
    (private:check-unused-local-variables document)
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

(define (private:collect-import-usages document)
  (let ([used-ht (make-eq-hashtable)]
      [import-clauses '()]
      [duplicate-seen (make-hashtable equal-hash equal?)])
    (let loop ([nodes (document-index-node-list document)] [in-import? #f])
      (for-each
        (lambda (node)
          (let ([expression (annotation-stripped (index-node-datum/annotations node))])
            (cond
              [(and (pair? expression) (eq? 'import (car expression)))
                (for-each 
                  (lambda (child) (private:check-duplicate-import-clause document child duplicate-seen))
                  (cdr (index-node-children node)))
                (set! import-clauses 
                  (append (cdr (index-node-children node)) import-clauses))
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
    (values used-ht (reverse import-clauses))))

(define (private:check-unused-imports document)
  (let-values ([(used-ht import-clauses) (private:collect-import-usages document)])
    (let ([seen (make-eq-hashtable)])
      (for-each 
        (lambda (clause-node) (private:check-import-clause document clause-node used-ht seen))
        import-clauses))))

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
        ; Plain imports now also attach their references to the library-identifier
        ; node itself (see library-import.sls), so checking this node tells us
        ; whether any binding introduced by this specific import was used.
        (let ([refs (index-node-references-import-in-this-node index-node)])
          (if (and (not (null? refs)) (not (find (lambda (r) (eq-hashtable-contains? used-ht r)) refs)))
            (private:append-unused-import-diagnose document index-node (library-identifier->string expression) seen)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate import detection (merged into collect-import-usages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:check-duplicate-import-clause document index-node seen)
  (let* ([expression (annotation-stripped (index-node-datum/annotations index-node))]
      [library-identifier (resolve-import-library-identifier expression)])
    (when (and (pair? library-identifier) (not (null? library-identifier)))
      (if (hashtable-contains? seen library-identifier)
        (private:append-duplicate-import-diagnose document index-node library-identifier)
        (hashtable-set! seen library-identifier #t)))))

(define (private:append-duplicate-import-diagnose document index-node library-identifier)
  (append-new-diagnoses document
    `(,(index-node-start index-node) ,(index-node-end index-node) 2
      ,(string-append "Duplicate import: " (library-identifier->string library-identifier))
      "import" "duplicate-import")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused local variable detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collect symbols that are explicitly exported from the document's
; top-level (library ...) / (define-library ...) form.  Local bindings
; whose identifiers appear here must not be reported as unused.
(define (private:collect-exported-identifiers document)
  (let ([exported (make-eq-hashtable)])
    (for-each
      (lambda (top-node)
        (let ([expr (annotation-stripped (index-node-datum/annotations top-node))])
          (when (and (pair? expr) (or (eq? 'library (car expr)) (eq? 'define-library (car expr))))
            (for-each
              (lambda (body-node)
                (let ([body-expr (annotation-stripped (index-node-datum/annotations body-node))])
                  (when (and (pair? body-expr) (eq? 'export (car body-expr)))
                    (for-each
                      (lambda (export-item-node)
                        (private:collect-export-item-identifiers exported export-item-node))
                      (cdr (index-node-children body-node))))))
              (cddr (index-node-children top-node))))))
      (document-index-node-list document))
    exported))

(define (private:collect-export-item-identifiers exported export-item-node)
  (let ([export-expr (annotation-stripped (index-node-datum/annotations export-item-node))])
    (cond
      [(symbol? export-expr)
        (eq-hashtable-set! exported export-expr #t)]
      [(and (pair? export-expr) (eq? 'rename (car export-expr)))
        (for-each
          (lambda (pair-node)
            (let ([pair-expr (annotation-stripped (index-node-datum/annotations pair-node))])
              (when (and (pair? pair-expr) (symbol? (car pair-expr)))
                (eq-hashtable-set! exported (car pair-expr) #t))))
          (cdr (index-node-children export-item-node)))]
      [else (void)])))

(define (private:collect-local-binding-references document)
  ; Local bindings may live either in document-ordered-reference-list (e.g.
  ; with-syntax syntax-parameters) or in index-node-references-export-to-other-node
  ; of the identifier leaf node (e.g. define/lambda/let).  Collect from both
  ; places and dedupe by the binding's index-node to avoid duplicate diagnostics.
  (let ([result '()] [seen (make-eq-hashtable)])
    (define (add! ref)
      (let ([index-node (identifier-reference-index-node ref)])
        (when (and (eq? (identifier-reference-document ref) document)
                (null? (identifier-reference-library-identifier ref))
                (index-node? index-node)
                (not (eq-hashtable-contains? seen index-node)))
          (eq-hashtable-set! seen index-node #t)
          (set! result (cons ref result)))))
    (for-each add! (document-ordered-reference-list document))
    (let walk ([node (document-index-node-list document)])
      (cond
        [(null? node) (void)]
        [(pair? node)
          (walk (car node))
          (walk (cdr node))]
        [(index-node? node)
          (for-each add! (index-node-references-import-in-this-node node))
          (for-each add! (index-node-references-export-to-other-node node))
          (for-each add! (index-node-excluded-references node))
          (walk (index-node-children node))]
        [else (void)]))
    result))

; Only parameters (lambda/case-lambda/define parameter-list formals) are
; reported.  Top-level define names and let-bound variables are intentionally
; skipped to avoid forward-reference and import-rename false positives.
(define (private:check-unused-local-variables document)
  (let* ([exported-ht (private:collect-exported-identifiers document)]
      [seen (make-eq-hashtable)])
    (for-each
      (lambda (ref)
        (when (and (not (eq-hashtable-contains? seen ref))
                (eq? (identifier-reference-type ref) 'parameter)
                (zero? (identifier-reference-usage-count ref)))
          (let ([id (identifier-reference-identifier ref)])
            (when (and (symbol? id) (not (eq-hashtable-contains? exported-ht id)))
              (eq-hashtable-set! seen ref #t)
              (let ([index-node (identifier-reference-index-node ref)])
                (when (index-node? index-node)
                  (append-new-diagnoses document
                    `(,(index-node-start index-node) ,(index-node-end index-node) 2
                      ,(string-append "Unused local variable: " (symbol->string id))
                      "identifier" "unused-local-variable"))))))))
      (private:collect-local-binding-references document))))

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
            (source-file->annotations text (uri->path (document-uri target-document)) (consume-sps-auxiliary text) #t target-document (workspace-top-environment workspace-instance))))])
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
          (document-index-node-list-set! d (map (lambda (item) (init-index-node '() item)) (source-file->annotations s path (consume-sps-auxiliary s) #t d top-environment))) 
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
