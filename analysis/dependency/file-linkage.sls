(library (scheme-langserver analysis dependency file-linkage)
  (export 
    init-file-linkage

    file-linkage-path->id-map
    file-linkage-id->path-map
    file-linkage-matrix
    file-linkage-take
    file-linkage-set!
    file-linkage-head)
  (import 
    (rnrs)
    (scheme-langserver analysis dependency rules library-import)

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
      (let ([cycle (find-cycle matrix)])
        (if (not (null? cycle))
          (display (map (lambda (id) (hashtable-ref id->path-map id #f)) cycle))))
      (make-file-linkage path->id-map id->path-map matrix))))

(define (init-maps current-library-node id->path-map path->id-map)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (pair? file-nodes)
      (begin
        (hashtable-set! path->id-map (file-node-path (car file-nodes)) (hashtable-size path->id-map))
        (hashtable-set! id->path-map (hashtable-size id->path-map) (file-node-path (car file-nodes)))
        (loop (cdr file-nodes)))))
  (map (lambda (node) (init-maps node id->path-map path->id-map)) (library-node-children current-library-node)))

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

(define file-linkage-take 
  (case-lambda 
    [(linkage from to) 
      (matrix-take 
        (file-linkage-matrix linkage) 
        (hashtable-ref (file-linkage-path->id-map linkage) from #f) 
        (hashtable-ref (file-linkage-path->id-map linkage) to #f))]
    [(linkage from)
      (let* ([matrix (file-linkage-matrix linkage)]
          [rows-count (sqrt (vector-length matrix))]
          [row-id (hashtable-ref (file-linkage-path->id-map linkage) from #f)])
        (let loop ([result '()])
          (if (< (length result) rows-count)
            (loop 
              (if (zero? (matrix-take matrix row-id (length result)))
                result
                (append result `(,(hashtable-ref (file-linkage-id->path-map linkage) (length result) #f)))))
            result)))]))

(define (file-linkage-set! linkage from to)
  (matrix-set!
    (file-linkage-matrix linkage) 
    (hashtable-ref (file-linkage-path->id-map linkage) from #f) 
    (hashtable-ref (file-linkage-path->id-map linkage) to #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (encode rows-number n m)
  (+ (* n rows-number) m))

(define (decode rows-number i)
  (let* ([n (floor (/ i rows-number))]
      [m (- i (encode rows-number n 0))])
    `(,n ,m)))

(define (matrix-take matrix n m)
  (if (and n m)
    (vector-ref matrix (encode (sqrt (vector-length matrix)) n m))))

(define (matrix-set! matrix n m)
  (if (and n m)
    (vector-set! matrix (encode (sqrt (vector-length matrix)) n m) 1)))

(define (init-matrix current-library-node root-library-node path->id-map matrix)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (pair? file-nodes)
      (let* ([file-node (car file-nodes)]
            [path (file-node-path file-node)]
            [imported-libraries 
              (apply append 
                (map 
                  (lambda (l) (map file-node-path (library-node-file-nodes l)))
                  (filter (lambda (l) 
                    (if (null? l) #f (not (null? (library-node-file-nodes l)))))
                    (map 
                      (lambda (id) (walk-library id root-library-node))
                      (library-import-process (document-index-node (file-node-document file-node)))))))])
        (map (lambda (imported-library-path) 
                (if (not (null? imported-library-path))
                  (matrix-set! matrix 
                    (hashtable-ref path->id-map path #f) 
                    (hashtable-ref path->id-map imported-library-path #f))))
            imported-libraries)
        (loop (cdr file-nodes)))))
  (map  (lambda (node) 
          (init-matrix node root-library-node path->id-map matrix)) 
    (library-node-children current-library-node)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-cycle
  (case-lambda 
    [(matrix) (find-cycle matrix (sqrt (vector-length matrix)))]
    [(matrix node-count) 
      (let ([visited (make-vector node-count)])
        (let loop ([n 0]) 
          (if (< n node-count)
            (let ([result (find-cycle matrix visited n '())])
              (if (null? result)
                (loop (+ 1 n))
                result))
            '())))]
    [(matrix visited n path) 
      (if (zero? (vector-ref visited n))
        (begin
          (vector-set! visited n 1)
          (let loop ([m 0])
            (if (< m (vector-length visited))
              (if (zero? (matrix-take matrix n m))
                (loop (+ 1 m))
                (begin
                  (find-cycle matrix visited m (append path `(,n)))
                  (loop (+ 1 m))))
              '())))
        (if (not (find (lambda (t) (= n t)) path))
          '()
          (append path `(,n)))
        )]))

; (define merge 
;   (case-lambda 
;     [(not-merged-vector) 
;       (vector-map 
;         (lambda (i) (merge not-merged-vector i)) 
;         (generate-vector (vector-length not-merged-vector)))]
;     [(not-merged-vector i)
;       (if (= i (vector-ref not-merged-vector i))
;         i
;         (let ([pre-set (merge not-merged-vector (vector-ref not-merged-vector i))])
;           (vector-set! not-merged-vector i pre-set)
;           pre-set))]))

; (define (generate-vector max)
;   (let ([result (make-vector max)])
;     (let loop ([i 1])
;       (vector-set! result i i))
;     result))

; (define (eliminate-duplicates l)
;   (cond 
;     [(null? l) l]
;     [(member (car l) (cdr l)) (eliminate-duplicates (cdr l))]
;     [else (cons (car l) (eliminate-duplicates (cdr l)))]))
)