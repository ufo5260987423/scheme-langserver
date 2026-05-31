(library (scheme-langserver analysis dependency file-linkage)
  (export 
    init-file-linkage

    file-linkage-path->id-map
    file-linkage?
    file-linkage-id->path-map
    file-linkage-matrix
    file-linkage-take
    file-linkage-set!
    file-linkage-head
    file-linkage-from
    file-linkage-to

    get-reference-path-to
    get-reference-path-from

    get-imported-libraries-from-index-node
    
    refresh-file-linkage&get-refresh-path
    get-init-reference-batches
    shrink-paths
    shrink-file-linkage!)
  (import 
    (chezscheme)
    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency rules library-import)
    (scheme-langserver analysis dependency rules library-import-r7rs)
    (scheme-langserver analysis dependency rules load)

    (scheme-langserver util dedupe)
    (scheme-langserver util contain)
    (scheme-langserver util matrix)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system file-node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type file-linkage
  (fields
    (mutable path->id-map)
    (mutable id->path-map)
    (mutable matrix)))

(define init-file-linkage
  (case-lambda
    [(root-file-node root-library-node) (init-file-linkage root-file-node root-library-node 'r6rs)]
    [(root-file-node root-library-node top-environment)
      (let ([id->path-map (make-eq-hashtable)]
            [path->id-map (make-hashtable string-hash equal?)])
        (init-maps root-library-node id->path-map path->id-map)
        (let ([matrix (make-vector (* (hashtable-size id->path-map) (hashtable-size id->path-map)))])
          (init-matrix root-library-node root-file-node root-library-node path->id-map matrix top-environment)
          (make-file-linkage path->id-map id->path-map matrix)))]))

(define (init-maps current-library-node id->path-map path->id-map)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (pair? file-nodes)
      (begin
        (hashtable-set! path->id-map (file-node-path (car file-nodes)) (hashtable-size path->id-map))
        (hashtable-set! id->path-map (hashtable-size id->path-map) (file-node-path (car file-nodes)))
        (loop (cdr file-nodes)))))
  (map (lambda (node) (init-maps node id->path-map path->id-map)) (library-node-children current-library-node)))

(define refresh-file-linkage&get-refresh-path
  (case-lambda
    [(linkage root-library-node file-node new-index-node-list new-library-identifier-list)
      (refresh-file-linkage&get-refresh-path linkage root-library-node file-node new-index-node-list new-library-identifier-list 'r6rs)]
    [(linkage root-library-node file-node new-index-node-list new-library-identifier-list top-environment)
      (let* ([path (file-node-path file-node)]
          [path->id-map (file-linkage-path->id-map linkage)]
          [id->path-map (file-linkage-id->path-map linkage)]
          [old-node-count (sqrt (vector-length (file-linkage-matrix linkage)))]
          [id (if (hashtable-ref path->id-map path #f)
            (hashtable-ref path->id-map path #f)
            (if (null? new-library-identifier-list)
              '()
              (begin
                (hashtable-set! path->id-map path old-node-count)
                (hashtable-set! id->path-map old-node-count path)
                (file-linkage-matrix-set! linkage (matrix-expand (file-linkage-matrix linkage)))
                old-node-count)))]
          [reference-id-to (if (null? id) '() (filter (lambda (inner-id) (not (= inner-id id))) (linkage-matrix-to-recursive (file-linkage-matrix linkage) id)))]
          [reference-id-from (if (null? id) '() (filter (lambda (inner-id) (not (= inner-id id))) (linkage-matrix-from-recursive (file-linkage-matrix linkage) id)))]
          [matrix (file-linkage-matrix linkage)]
          [old-imported-file-ids
            (map 
              (lambda(p) (hashtable-ref path->id-map p #f)) 
              (get-reference-path-from linkage path))]
          [new-imported-file-ids
            (map 
              (lambda(p) (hashtable-ref path->id-map p #f)) 
              (apply append 
                (map 
                  (lambda (index-node) (get-imported-libraries-from-index-node root-library-node index-node top-environment))
                  new-index-node-list)))])
        (if (null? id)
          (begin
            ;; File is not in the linkage (script file or already removed)
            '())
          (begin 
            (for-each (lambda(row-id) (matrix-set! matrix row-id id 0)) old-imported-file-ids)
            (for-each (lambda(column-id) (matrix-set! matrix id column-id 1)) (dedupe new-imported-file-ids))
            (map (lambda(current-id) (hashtable-ref id->path-map current-id #f)) `(,@reference-id-from ,id ,@reference-id-to)))))]))

(define (shrink-file-linkage! linkage removed-path)
  (let* ([path->id-map (file-linkage-path->id-map linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [removed-id (hashtable-ref path->id-map removed-path #f)])
    (if removed-id
      (let* ([old-node-count (sqrt (vector-length (file-linkage-matrix linkage)))]
          [new-path->id-map (make-hashtable string-hash equal?)]
          [new-id->path-map (make-eq-hashtable)]
          [next-id 0])
        (let loop ([id 0])
          (if (< id old-node-count)
            (begin
              (when (not (= id removed-id))
                (let ([path (hashtable-ref id->path-map id #f)])
                  (when path
                    (hashtable-set! new-path->id-map path next-id)
                    (hashtable-set! new-id->path-map next-id path)
                    (set! next-id (+ 1 next-id)))))
              (loop (+ 1 id)))
            (void)))
        (file-linkage-path->id-map-set! linkage new-path->id-map)
        (file-linkage-id->path-map-set! linkage new-id->path-map)
        (file-linkage-matrix-set! linkage (matrix-shrink (file-linkage-matrix linkage) removed-id)))
      (void))))

(define (get-reference-path-to linkage to-path)
  (let* ([matrix (file-linkage-matrix linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [path->id-map (file-linkage-path->id-map linkage)]
      [to-node-id (hashtable-ref path->id-map to-path #f)])
    (if to-node-id
      (map (lambda (id) (hashtable-ref id->path-map id #f)) (linkage-matrix-to-recursive matrix to-node-id))
      '())))

(define (get-reference-path-from linkage from-path)
  (let* ([matrix (file-linkage-matrix linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [path->id-map (file-linkage-path->id-map linkage)]
      [from-node-id (hashtable-ref path->id-map from-path #f)])
    (if from-node-id
      (map (lambda (id) (hashtable-ref id->path-map id #f)) (linkage-matrix-from-recursive matrix from-node-id))
      '())))

(define (get-init-reference-batches linkage)
  (let* ([node-count (sqrt (vector-length (file-linkage-matrix linkage)))]
      [ids (iota node-count)]
      [matrix (file-linkage-matrix linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [batches (shrink-ids matrix ids)])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      batches)))

(define (shrink-paths linkage paths)
  (let* ([path->id-map (file-linkage-path->id-map linkage)]
      [id->path-map (file-linkage-id->path-map linkage)]
      [ids (map (lambda (current-path) (hashtable-ref path->id-map current-path #f)) paths)]
      [shrinked-ids (shrink-ids (file-linkage-matrix linkage) ids)])
    (map 
      (lambda (ids) 
        (map (lambda (id) (hashtable-ref id->path-map id #f)) ids))
      shrinked-ids)))

(define (shrink-ids matrix ids)
  (if (null? ids)
      '()
      (let* ([id-set (make-eq-hashtable)]
          [out-degrees (make-eq-hashtable)]
          [in-adj (make-eq-hashtable)])
        ; Build id-set for O(1) membership test
        (for-each (lambda (id) (hashtable-set! id-set id #t)) ids)
        
        ; Build adjacency lists and out-degrees for the subgraph induced by ids
        (for-each 
          (lambda (from)
            (let ([neighbors '()])
              (for-each 
                (lambda (to)
                  (when (and (hashtable-ref id-set to #f) (not (zero? (matrix-take matrix from to))))
                    (set! neighbors (cons to neighbors))))
                ids)
              (hashtable-set! out-degrees from (length neighbors))
              (for-each 
                (lambda (to)
                  (hashtable-set! in-adj to 
                    (cons from (hashtable-ref in-adj to '()))))
                neighbors)))
          ids)
        
        ; Kahn's algorithm: peel layers of nodes with zero out-degree.
        ; Each layer can be analysed in parallel because its members no
        ; longer depend on any remaining node in the current set.
        (let loop ([queue (filter (lambda (id) (zero? (hashtable-ref out-degrees id 0))) ids)]
            [remaining (length ids)]
            [batches '()])
          (cond
            [(null? queue)
              (if (zero? remaining)
                (reverse batches)
                ; Remaining nodes form one or more SCCs.  Rather than
                ; serialising them one-by-one (which destroys parallelism),
                ; pack the whole SCC as a single batch so that all its
                ; members are processed concurrently.
                (let ([scc (filter (lambda (id) (> (hashtable-ref out-degrees id 0) 0)) ids)])
                  (reverse (cons scc batches))))]
            [else
              (let peel ([q queue] [next-q '()] [batch '()] [rem remaining])
                (if (null? q)
                    (loop next-q rem (cons batch batches))
                    (let* ([node (car q)]
                            [in-neighbors (hashtable-ref in-adj node '())])
                      ; Decrement out-degree of every node that has an edge to node.
                      ; When a node's out-degree drops to zero it becomes ready for
                      ; the next batch.
                      (for-each 
                        (lambda (neighbor)
                          (let ([deg (hashtable-ref out-degrees neighbor 0)])
                            (when (> deg 0)
                              (let ([new-deg (- deg 1)])
                                (hashtable-set! out-degrees neighbor new-deg)
                                (when (zero? new-deg)
                                  (set! next-q (cons neighbor next-q)))))))
                        in-neighbors)
                      (peel (cdr q) next-q (cons node batch) (- rem 1)))))])))))

(define (file-linkage-head linkage)
  (let* ([matrix (file-linkage-matrix linkage)]
      [rows-count (sqrt (vector-length matrix))]
      [id->path-map (file-linkage-id->path-map linkage)])
    (map 
      (lambda (id) (hashtable-ref id->path-map id #f))
      (let loop ([n 0][m 0][middle 0])
        (if (< n rows-count)
          (if (< m rows-count)
            (loop n (+ 1 m) (+ middle (matrix-take matrix n m)))
            (if (zero? middle) 
              `(,n . ,(loop (+ 1 n) 0 0)) 
              (loop (+ 1 n) 0 0)))
          '())))))

(define (linkage-matrix-from-recursive matrix from-id)
  (let loop ([result `(,from-id)] [iterator `(,from-id)])
    (let ([children 
          (apply append (map 
            (lambda (current-from)
              (filter 
                (lambda (id) (not (contain? result id)))
                (matrix-from matrix current-from)))
            iterator))])
      (if (null? children)
        result
        (loop (append result children) children)))))

(define (linkage-matrix-to-recursive matrix to-id)
  (let loop ([result `(,to-id)] [iterator `(,to-id)])
    (let ([children 
          (apply append (map 
            (lambda (current-to)
              (filter 
                (lambda (id) (not (contain? result id)))
                (matrix-to matrix current-to)))
            iterator))])
      (if (null? children)
        result
        (loop (append result children) children)))))
        
; from-path
; row-id
(define (file-linkage-from linkage from-path)
  (let* ([matrix (file-linkage-matrix linkage)]
      [rows-count (sqrt (vector-length matrix))]
      [row-id (hashtable-ref (file-linkage-path->id-map linkage) from-path #f)])
    (let loop ([column-id 0])
      (if (< column-id rows-count)
        (if (zero? (matrix-take matrix row-id column-id))
          (loop (+ 1 column-id))
          `(,(hashtable-ref (file-linkage-id->path-map linkage) column-id #f) . ,(loop (+ 1 column-id))))
        '()))))

; to-path
; column-id
(define (file-linkage-to linkage to-path)
  (let* ([matrix (file-linkage-matrix linkage)]
      [rows-count (sqrt (vector-length matrix))]
      [column-id (hashtable-ref (file-linkage-path->id-map linkage) to-path #f)])
    (let loop ([row-id 0])
      (if (< row-id rows-count)
        (if (zero? (matrix-take matrix row-id column-id))
          (loop (+ 1 row-id))
          `(,(hashtable-ref (file-linkage-id->path-map linkage) row-id #f) .  ,(loop (+ 1 row-id))))
        '()))))

(define (file-linkage-take linkage from to)
  (matrix-take 
    (file-linkage-matrix linkage) 
    (hashtable-ref (file-linkage-path->id-map linkage) from #f) 
    (hashtable-ref (file-linkage-path->id-map linkage) to #f)))

(define (file-linkage-set! linkage from to)
  (matrix-set!
    (file-linkage-matrix linkage) 
    (hashtable-ref (file-linkage-path->id-map linkage) from #f) 
    (hashtable-ref (file-linkage-path->id-map linkage) to #f)))

(define get-imported-libraries-from-index-node
  (case-lambda
    [(root-library-node index-node) (get-imported-libraries-from-index-node root-library-node index-node 'r6rs)]
    [(root-library-node index-node top-environment)
      (let ([func (case top-environment
                    ['r6rs library-import-process]
                    ['r7rs library-import-process-r7rs]
                    ['s7 library-import-process-r7rs])])
        (apply append 
          (map 
            (lambda (l) (map file-node-path (library-node-file-nodes l)))
            (filter (lambda (l) 
                (if (null? l) #f (not (null? (library-node-file-nodes l)))))
              (map 
                (lambda (id) (walk-library id root-library-node))
                (func index-node))))))]))

(define init-matrix
  (case-lambda
    [(current-library-node root-file-node root-library-node path->id-map matrix) (init-matrix current-library-node root-file-node root-library-node path->id-map matrix 'r6rs)]
    [(current-library-node root-file-node root-library-node path->id-map matrix top-environment)
      (let loop ([file-nodes (library-node-file-nodes current-library-node)])
        (if (pair? file-nodes)
          (let* ([file-node (car file-nodes)]
              [path (file-node-path file-node)]
              [imported-libraries 
                (dedupe (apply append 
                  (map (lambda (index-node) (get-imported-libraries-from-index-node root-library-node index-node top-environment))
                    (document-index-node-list (file-node-document file-node)))))]
              [loaded-files 
                (dedupe (apply append 
                  (map (lambda (index-node) (load-process root-file-node (file-node-document file-node) index-node))
                    (document-index-node-list (file-node-document file-node)))))])
    
            (map (lambda (imported-library-path) 
                    (if (not (null? imported-library-path))
                      (matrix-set! matrix 
                        (hashtable-ref path->id-map path #f) 
                        (hashtable-ref path->id-map imported-library-path #f))))
              imported-libraries)
    
            (map (lambda (file-node) 
                    (matrix-set! matrix 
                      (hashtable-ref path->id-map path #f) 
                      (hashtable-ref path->id-map (file-node-path file-node) #f)))
              loaded-files)
            
            (loop (cdr file-nodes)))))  
        
      (map  (lambda (node) 
              (init-matrix node root-file-node root-library-node path->id-map matrix top-environment)) 
        (library-node-children current-library-node))  
        ]))
)