(library (scheme-langserver util matrix)
    (export 
        matrix-expand
        matrix-take
        matrix-set!
        matrix-to
        matrix-from
        find-cycle
        encode 
        decode)
    (import (rnrs))

(define (matrix-from matrix from-id)
  (let ([rows-count (sqrt (vector-length matrix))]
        [row-id from-id])
    (let loop ([column-id 0][result '()])
      (if (< column-id rows-count)
        (loop 
          (+ 1 column-id)
          (if (zero? (matrix-take matrix row-id column-id))
            result
            (append result `(,column-id))))
        result))))

(define (matrix-to matrix to-id)
  (let ([rows-count (sqrt (vector-length matrix))]
        [column-id to-id])
    (let loop ([row-id 0][result '()])
      (if (< row-id rows-count)
        (loop 
          (+ 1 row-id)
          (if (zero? (matrix-take matrix row-id column-id))
            result
            (append result `(,row-id))))
        result))))

(define (matrix-expand target-matrix)
  (let* ([node-count (sqrt (vector-length target-matrix))]
      [result (make-vector (* (+ 1 node-count) (+ 1 node-count)))]
      [current-length (vector-length result)])
    (let loop ([index 0])
      (if (< index current-length)
        (let* ([indexes (decode node-count index)]
            [row-id (car indexes)]
            [column-id (cadr indexes)])
          (vector-set! result index (if (or (= row-id node-count) (= column-id node-count)) (matrix-take target-matrix row-id column-id) 0))
          (loop (+ index 1)))
        result))))

(define (encode columns-number n m)
  (+ (* n columns-number) m))

(define (decode columns-number i)
  (let* ([n (floor (/ i columns-number))]
      [m (- i (encode columns-number n 0))])
    `(,n ,m)))

(define matrix-take 
  (case-lambda 
    [(matrix n m) (matrix-take matrix (sqrt (vector-length matrix)) n m)]
    [(matrix columns-number n m) (if (and n m) (vector-ref matrix (encode columns-number n m)))]))

(define matrix-set! 
  (case-lambda
    [(matrix n m) (matrix-set! matrix n m 1)]
    [(matrix n m value) (matrix-set! matrix (sqrt (vector-length matrix)) n m value)]
    [(matrix columns-number n m value) (if (and n m) (vector-set! matrix (encode columns-number n m) value))]))


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
                (let ([result (find-cycle matrix visited m (append path `(,n)))])
                  (if (null? result)
                    (loop (+ 1 m))
                    (append path result))))
              '())))
        (if (find (lambda (t) (= n t)) path)
          (append path `(,n))
          '()))]))
)