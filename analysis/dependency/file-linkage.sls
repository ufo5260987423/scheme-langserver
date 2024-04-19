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
    get-init-reference-path)
  (import 
    (chezscheme)
    (scheme-langserver analysis util)

    (scheme-langserver analysis dependency rules library-import)
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

(define (init-file-linkage root-library-node)
  (let ([id->path-map (make-eq-hashtable)]
        [path->id-map (make-hashtable string-hash equal?)])
    (init-maps root-library-node id->path-map path->id-map)
    (let ([matrix (make-vector (* (hashtable-size id->path-map) (hashtable-size id->path-map)))])
      (init-matrix root-library-node root-library-node path->id-map matrix)
      (make-file-linkage path->id-map id->path-map matrix))))

(define (init-maps current-library-node id->path-map path->id-map)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (pair? file-nodes)
      (begin
        (hashtable-set! path->id-map (file-node-path (car file-nodes)) (hashtable-size path->id-map))
        (hashtable-set! id->path-map (hashtable-size id->path-map) (file-node-path (car file-nodes)))
        (loop (cdr file-nodes)))))
  (map (lambda (node) (init-maps node id->path-map path->id-map)) (library-node-children current-library-node)))

(define (refresh-file-linkage&get-refresh-path linkage root-library-node file-node new-index-node-list new-library-identifier-list)
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
      [target-document (file-node-document file-node)]
      [old-imported-file-ids
        (map 
          (lambda(p) (hashtable-ref path->id-map p #f)) 
          (get-reference-path-from linkage path))]
      [new-imported-file-ids
        (map 
          (lambda(p) (hashtable-ref path->id-map p #f)) 
          (apply append 
            (map 
              (lambda (index-node) (get-imported-libraries-from-index-node root-library-node index-node))
              new-index-node-list)))])
    (if (null? id)
      ;;todo shrink matrix
      '()
      (begin 
        (map (lambda(row-id) (matrix-set! matrix row-id id 0)) old-imported-file-ids)
        (map (lambda(column-id) (matrix-set! matrix id column-id 1)) (dedupe new-imported-file-ids))
        (map (lambda(current-id) (hashtable-ref id->path-map current-id #f)) `(,@reference-id-from ,id ,@reference-id-to))))))

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

(define (get-init-reference-path linkage)
  (let* ([node-count (sqrt (vector-length (file-linkage-matrix linkage)))]
      [visited-ids (make-vector node-count)]
      [matrix (file-linkage-matrix linkage)]
      [id->path-map (file-linkage-id->path-map linkage)])
    (let loop ([from-id 0] 
        [path '()]
        [last-path-length 0])
      (cond 
        [(or (= (length path) node-count) 
          (and (= node-count from-id)
            (= last-path-length (length path))))
          (map (lambda (id) (hashtable-ref id->path-map id #f)) path)]
        [(= node-count from-id)
          (loop 0 path (length path))]
        [(not (zero? (vector-ref visited-ids from-id)))
          (loop (+ 1 from-id) path last-path-length)]
        [else 
          (let ([to-ids (matrix-from matrix from-id)])
            (if (= (length to-ids) (apply + (map (lambda (to-id) (vector-ref visited-ids to-id)) to-ids)))
              (begin
                (vector-set! visited-ids from-id 1)
                (loop (+ from-id 1) (append path `(,from-id)) last-path-length))
              (loop (+ from-id 1) path last-path-length)))]))))

(define (file-linkage-head linkage)
  (let* ([matrix (file-linkage-matrix linkage)]
      [rows-count (sqrt (vector-length matrix))]
      [id->path-map (file-linkage-id->path-map linkage)])
    (let loop ([n 0][m 0][middle 0][result '()])
      (if (< n rows-count)
        (if (< m rows-count)
          (loop n (+ 1 m) (+ middle (matrix-take matrix n m)) result)
          (loop (+ 1 n) 0 0 (append result (if (zero? middle) `(,n) '()))))
        (map 
          (lambda (id) (hashtable-ref id->path-map id #f))
          result)))))

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
    (let loop ([column-id 0][result '()])
      (if (< column-id rows-count)
        (loop 
          (+ 1 column-id)
          (if (zero? (matrix-take matrix row-id column-id))
            result
            (append result `(,(hashtable-ref (file-linkage-id->path-map linkage) column-id #f)))))
        result))))

; to-path
; column-id
(define (file-linkage-to linkage to-path)
  (let* ([matrix (file-linkage-matrix linkage)]
      [rows-count (sqrt (vector-length matrix))]
      [column-id (hashtable-ref (file-linkage-path->id-map linkage) to-path #f)])
    (let loop ([row-id 0][result '()])
      (if (< row-id rows-count)
        (loop 
          (+ 1 row-id)
          (if (zero? (matrix-take matrix row-id column-id))
            result
            (append result `(,(hashtable-ref (file-linkage-id->path-map linkage) row-id #f)))))
        result))))

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

(define (get-imported-libraries-from-index-node root-library-node index-node)
  (apply append 
    (map 
      (lambda (l) (map file-node-path (library-node-file-nodes l)))
      (filter (lambda (l) 
          (if (null? l) #f (not (null? (library-node-file-nodes l)))))
        (map 
          (lambda (id) (walk-library id root-library-node))
          (library-import-process index-node))))))

(define (init-matrix current-library-node root-library-node path->id-map matrix)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (pair? file-nodes)
      (let* ([file-node (car file-nodes)]
          [path (file-node-path file-node)]
          [imported-libraries 
            (dedupe (apply append 
              (map (lambda (index-node) (get-imported-libraries-from-index-node root-library-node index-node))
                (document-index-node-list (file-node-document file-node)))))]
          [loaded-files 
            (dedupe (apply append 
              (map (lambda (index-node) (load-process root-library-node (file-node-document file-node) index-node))
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
          (init-matrix node root-library-node path->id-map matrix)) 
    (library-node-children current-library-node)))
)