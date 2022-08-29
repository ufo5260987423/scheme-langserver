(library (scheme-langserver analysis workspace)
  (export 
    init-workspace
    init-virtual-file-system
    init-library-node
    init-index-node
    init-document

    source-file->annotation
    pick
    walk-find
    walk-library
    generate
    folder-or-scheme-file?)
  (import 
    (ufo-match)
    (chezscheme) 
    (only (srfi :13 strings) string-prefix? string-suffix?)

    (scheme-langserver util path)
    (scheme-langserver util try)
    (scheme-langserver util io)

    ; (scheme-langserver analysis identifier reference)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

(define-record-type workspace
  (fields
    (immutable file-node)
    (mutable source-node)))

(define (init-workspace path)
  (let* ([root-file-node (init-virtual-file-system path '() folder-or-scheme-file?)]
        [workspace-instance (make-workspace root-file-node (init-library-node root-file-node))])
    workspace-instance
  ))

(define (init-virtual-file-system path parent my-filter)
  (if (my-filter path)
    (let* ([name (path->name path)] 
          [folder? (file-directory? path)]
          [document (if folder? '() 
              (try
                (init-document (path->uri path))
                ;;todo diagnostic
                (except e
                  [else '()])))]
          [node (make-file-node path name parent folder? '() document)]
          [children (if folder?
              (map 
                (lambda(p) 
                  (init-virtual-file-system 
                    (string-append path (list->string (list (directory-separator))) p) 
                    node 
                    my-filter)) 
                (directory-list path))
              '())])
      (file-node-children-set! node (filter (lambda(p) (not (null? p))) children))
      node)
    '()))

(define (init-document uri)
  (let ([path (uri->path uri)])
    (make-document 
      uri 
      (read-string path) 
      (init-index-node '() (source-file->annotation path)))))

(define init-library-node
  (case-lambda 
    [(file-node) (init-library-node file-node (make-library-node '() '() '() '())) ]
    [(file-node root-library-node) 
      (if (file-node-folder? file-node)
        (map 
          (lambda (child-node) (init-library-node child-node root-library-node))
          (file-node-children file-node))
        (let ([document-instance (file-node-document file-node)])
          (if (not (null? document-instance))
            (let* ([index-node-instance (document-index-node document-instance)]
                  [expression (annotation-stripped (index-node-datum/annotations index-node-instance))])
            ;;rule
              (match expression 
                [('library (name **1) rest ... ) (generate name root-library-node file-node)]
                [else '()])))))
      root-library-node]))

(define (init-index-node parent datum/annotations)
  (let* ([source (annotation-source datum/annotations)]
        [node (make-index-node parent (source-object-bfp source) (source-object-efp source) datum/annotations '() '() '())]
        [expression (annotation-expression datum/annotations)])
    (index-node-children-set! 
      node 
      (if (list? expression)
        (map 
          (lambda(e) 
            (if (annotation? e)
              (init-index-node node e)
              '()))
          expression)
        '()))
    node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define source-file->annotation
  (case-lambda
    ([path] (source-file->annotation (read-string path) path))
    ([source path]
      (let-values 
        ([(ann end-pos)
          (get-datum/annotations 
            (open-string-input-port source) 
            (make-source-file-descriptor path (open-file-input-port path)) 0)])
        ann))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (folder-or-scheme-file? path)
  (if (file-directory? path) 
    #t
    (find (lambda(t) (or t #f))
      (map (lambda (suffix) (string-suffix? suffix path)) 
      '(".sps" ".sls" ".scm" ".ss")))))

(define (walk-find node path)
  (if (equal? (file-node-path node) path)
    `(,node)
    (if (string-prefix? (file-node-path node) path)
      (apply append 
        (map 
          (lambda (new-node) 
            (walk-find new-node path)) (file-node-children node) ))
      '())))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (walk-library list-instance node)
  (if (null? list-instance)
    node
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [children (find 
              (lambda(child-node) (equal? head (library-node-name child-node))) 
              (library-node-children node))])
      (if children
        (walk-library rest (car children))
        '()))))

(define (generate list-instance library-node virtual-file-node)
  (if (null? list-instance)
    (begin
      (library-node-file-nodes-set! library-node (append (library-node-file-nodes library-node) `(,virtual-file-node)))
      library-node)
    (let* ([head (car list-instance)]
          [rest (cdr list-instance)]
          [child (find 
              (lambda (child-node) (equal? head (library-node-name child-node))) 
              (library-node-children library-node))])
      (generate 
        rest 
        (if child
          child
          (let ([child (make-library-node head library-node '() '())])
            (library-node-children-set!  library-node 
              (append (library-node-children library-node) `(,child)))
            child))
        virtual-file-node))))
)