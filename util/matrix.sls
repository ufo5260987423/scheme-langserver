(library (scheme-langserver util matrix)
    (export 
        matrix-expand
        matrix-take
        matrix-set!
        matrix-to
        matrix-from
        find-cycle
        encode 
        decode
        matrix-shrink)
    (import (rnrs))

(define (matrix-from matrix from-id)
  (let ([rows-count (sqrt (vector-length matrix))]
        [row-id from-id])
    (let loop ([column-id 0][result '()])
      (if (< column-id rows-count)
        (loop 
          (+ 1 column-id)
          (if (zero? (vector-ref matrix (encode rows-count row-id column-id)))
            result
            (cons column-id result)))
        (reverse result)))))

(define (matrix-to matrix to-id)
  (let ([rows-count (sqrt (vector-length matrix))]
        [column-id to-id])
    (let loop ([row-id 0][result '()])
      (if (< row-id rows-count)
        (loop 
          (+ 1 row-id)
          (if (zero? (vector-ref matrix (encode rows-count row-id column-id)))
            result
            (cons row-id result)))
        (reverse result)))))

(define (matrix-expand target-matrix)
  (let* ([node-count (sqrt (vector-length target-matrix))]
      [new-count (+ 1 node-count)]
      [result (make-vector (* new-count new-count))]
      [current-length (vector-length result)])
    (let loop ([index 0])
      (if (< index current-length)
        (let ([row-id (div index new-count)]
              [column-id (mod index new-count)])
          (vector-set! result index (if (or (= row-id node-count) (= column-id node-count)) 0 (vector-ref target-matrix (encode node-count row-id column-id))))
          (loop (+ index 1)))
        result))))

(define (matrix-shrink target-matrix removed-id)
  (let* ([node-count (sqrt (vector-length target-matrix))]
      [new-count (- node-count 1)])
    (if (<= new-count 0)
      (make-vector 0)
      (let ([result (make-vector (* new-count new-count) 0)])
        (let loop ([new-row 0])
          (if (< new-row new-count)
            (let ([old-row (if (< new-row removed-id) new-row (+ new-row 1))])
              (let loop2 ([new-col 0])
                (if (< new-col new-count)
                  (let ([old-col (if (< new-col removed-id) new-col (+ new-col 1))])
                    (vector-set! result (encode new-count new-row new-col) (vector-ref target-matrix (encode node-count old-row old-col)))
                    (loop2 (+ 1 new-col)))
                  (loop (+ 1 new-row)))))
            result))))))

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
            (let ([result (find-cycle matrix node-count visited n '())])
              (if (null? result)
                (loop (+ 1 n))
                result))
            '())))]
    [(matrix node-count visited n path) 
      (if (zero? (vector-ref visited n))
        (begin
          (vector-set! visited n 1)
          (let loop ([m 0])
            (if (< m (vector-length visited))
              (if (zero? (vector-ref matrix (encode node-count n m)))
                (loop (+ 1 m))
                (let ([result (find-cycle matrix node-count visited m (cons n path))])
                  (if (null? result)
                    (loop (+ 1 m))
                    (cons n result))))
              '())))
        (if (find (lambda (t) (= n t)) path)
          (cons n path)
          '()))]))
)