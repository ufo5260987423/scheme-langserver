(library (scheme-langserver analysis dependency file-linkage)
  (export 
    find-available-references-for

    make-identifier-reference
    identifier-reference-document
    identifier-reference-index-node)
  (import 
    (rnrs)
    (scheme-langserver analysis dependency rules library-import)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system file-node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-record-type linkage-matrix
  (fields
    (mutable path->id-map)
    (mutable id->path-map)
    (mutable matrix)))

(define (init-linkage-matrix root-library-node)
  (let ([id->path-map (make-eq-hashtable)]
        [path->id-map (make-hashtable string-hash equal?)])
    (init-maps root-library-node id->path-map path->id-map)
    (let ([matrix (* (hashtable-size id->path-map) (hashtable-size id->path-map))])
      (init-matrix root-library-node root-library-node path->id-map)
      (make-linkage-matrix path->id-map id->path-map matrix))))

(define (init-maps current-library-node id->path-map path->id-map)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (not (null? file-nodes))
      (begin
        (hashtable-set! path->id-map (file-node-path (car file-nodes)) (hashtable-size path->id-map))
        (hashtable-set! id->path-map (hashtable-size id->path-map) (file-node-path (car file-nodes)))
        (loop (cdr file-nodes)))))
  (map (lambda (node) (init-maps id->path-map path->id-map)) (library-node-children current-library-node)))

(define (matrix-take matrix n m)
  (let ([rows-number (sqrt (vector-length matrix))])
    (vector-ref matrix (+ (* n rows-number) m))))

(define (matrix-set! matrix n m value)
  (let ([rows-number (sqrt (vector-length matrix))])
    (vector-set! matrix (+ (* n rows-number) m) value)))

(define (init-matrix current-library-node root-library-node path->id-map matrix)
  (let loop ([file-nodes (library-node-file-nodes current-library-node)])
    (if (not (null? file-nodes))
      (let* ([file-node (car file-nodes)]
            [path (file-node-path file-node)]
            [imported-libraries (library-import-process (document-index-node (file-node-document file-node)))])
        (map (lambda (imported-library-path) 
                (matrix-set! matrix (hashtable-ref path->id-map path) (hashtable-ref path->id-map imported-library-path)))
                ;;todo diagnostic
              (map (lambda (imporeted-library) (walk-library imporeted-library root-library-node)) imported-libraries))
      )))
  (map (lambda (node) (init-matrix root-library-node path->id-map matrix)) (library-node-children current-library-node)))
)