(library (scheme-langserver analysis type domain-specific-language syntax-candy)
  (export 
    candy:segmentable?
    candy:matchable?
    candy:match
    candy:match-left
    candy:match-right
    
    segment?
    segment-type
    segment-tail)
  (import 
    (chezscheme)
    (scheme-langserver util matrix)
    (scheme-langserver util contain)
    (scheme-langserver util try))

(define-record-type segment
  (fields
    (immutable type)
    (mutable tail)))

(define (candy:segmentable? target)
  (try
    (private-segment target)
    #t
    (except c 
      (else #f))))

(define (candy:matchable? parameter-template argument-list)
  (equal? 'skipped 
    (vector-ref (private-segments->match-matrix (private-segment parameter-template) (private-segment argument-list)) 0)))

(define (candy:match-right parameter-template argument-list)
  (map (lambda (match-segment-pair) `(,(segment-type (car match-segment-pair)) . ,(segment-type (cdr match-segment-pair)))) (candy:match parameter-template argument-list)))

(define (candy:match-left parameter-template argument-list)
  (fold-left 
    (lambda (result match-segment-pair)
      (cond 
        [(and 
          (null? result)
          (or 
            (private-is-... (car match-segment-pair))
            (private-is-**1 (car match-segment-pair))))
          `((,(segment-type (car match-segment-pair)) . (,(segment-type (cdr match-segment-pair)))))]
        [(null? result)
          `((,(segment-type (car match-segment-pair)) . ,(segment-type (cdr match-segment-pair))))]
        [(or 
          (private-is-... (car match-segment-pair))
          (private-is-**1 (car match-segment-pair)))
          (let ([last-left (car (car (reverse result)))]
              [last-right (cdr (car (reverse result)))]
              [ahead (reverse (cdr (reverse result)))])
            (if (equal? last-left (segment-type (car match-segment-pair)))
              (append ahead `((,last-left . ,(append last-right (list (segment-type (cdr match-segment-pair)))))))
              (append result `((,(segment-type (car match-segment-pair)) . (,(segment-type (cdr match-segment-pair))))))))]
        [else (append result `((,(segment-type (car match-segment-pair)) . ,(segment-type (cdr match-segment-pair)))))]))
    '()
    (candy:match parameter-template argument-list)))

;NOTE: a complecated case is like regexes abc+ and ab...c
(define candy:match 
  (case-lambda
    [(parameter-template argument-list)
      (let* ([rest-segment (private-segment parameter-template)]
          [ready-segment (private-segment argument-list)]
          [matrix (private-segments->match-matrix rest-segment ready-segment)])
        (candy:match matrix rest-segment ready-segment 0 0))]
    [(matrix rest-segments ready-segments row-id column-id)
      (if (or (> row-id (vector-length rest-segments))
          (> column-id (vector-length ready-segments)))
        '()
        (let* ([current-value (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id)])
          (cond 
            [(equal? current-value 'matched)
              `(,`(,(vector-ref rest-segments (- row-id 1)) . 
                    ,(vector-ref ready-segments (- column-id 1)))
                ,@(candy:match matrix rest-segments ready-segments (+ row-id 1) column-id)
                ,@(candy:match matrix rest-segments ready-segments row-id (+ column-id 1)))]
            [(equal? current-value 'skipped)
              `(,@(candy:match matrix rest-segments ready-segments (+ row-id 1) column-id)
                ,@(candy:match matrix rest-segments ready-segments row-id (+ column-id 1)))]
            [(equal? current-value 'unused) '()])))]))

(define private-segments->match-matrix
  (case-lambda 
    [(rest-segments ready-segments)
      (private-segments->match-matrix
        ;this matrix has 3 status: matched skipped and unused and it only supports stepping right→ and down↓.
        ;you can't step this matrix diagonally↘.
        (make-vector (* (+ 1 (vector-length rest-segments)) (+ 1 (vector-length ready-segments))) 'unused)
        rest-segments 
        ready-segments
        0 
        0)]
    [(matrix rest-segments ready-segments row-id column-id)
      (cond
        ;In end zone
        [(and 
            (= row-id (vector-length rest-segments)) 
            (= column-id (vector-length ready-segments)))
          (cond
            [(or 
                (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
                (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1))))
              (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id 'matched)]
            [(or
                (and 
                  (equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
                  (private-is-**1 (vector-ref ready-segments (- column-id 1))))
                (and 
                  (equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
                  (private-is-**1 (vector-ref rest-segments (- row-id 1)))))
              (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id 'matched)]
            [(or 
                (private-is-... (vector-ref rest-segments (- row-id 1)))
                (private-is-... (vector-ref ready-segments (- column-id 1))))
              (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id 'matched)]
            [else '()])]
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;In start zone
        [(and (zero? row-id) (zero? column-id)) 
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped)]

        ;skipped and continue at two directions
        [(and (zero? row-id) (< 0 column-id))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))
            (if (private-is-... (vector-ref ready-segments (- column-id 1)))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right)))]
        [(and (zero? column-id) (< 0 row-id))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))
            (if (private-is-... (vector-ref rest-segments (- row-id 1)))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down)))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;In the core, a most basic idea is to recure the only one comming path,
        ;'matched means both of previous ready-segment and rest-segment are matched, not only one of them.
        ;each clause should first check itself is possible

        ;'matched
        [(and 
            (equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
            ;in this case, ... and **1 are the same
            (or 
              (private-is-**1 (vector-ref ready-segments (- column-id 1)))
              (private-is-... (vector-ref ready-segments (- column-id 1)))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref rest-segments (- row-id 1)))
                    (private-is-... (vector-ref rest-segments (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))

          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id))
            ;here can't go down for which might leading misunderstanding semantic
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))]
        [(equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right)]

        [(and 
            (equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
            ;in this case, ... and **1 are the same
            (or 
              (private-is-**1 (vector-ref rest-segments (- row-id 1)))
              (private-is-... (vector-ref rest-segments (- row-id 1)))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref ready-segments (- column-id 1)))
                    (private-is-... (vector-ref ready-segments (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))

          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id))
            ;here can't go down for which might leading misunderstanding semantic
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))]
        [(equal? 'matched (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down)]

        ;'skipped
        ;'skipped means both of previous ready-segment and rest-segment are skipped, not only one of them,
        ;and current segments are innocent. Whicn means that ... and **1 won't skip ... items.
        ;So, **1 , ... and trivial segments are supposed to get matched here and seperately go to different directions.
        [(and 
            (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
            (private-is-**1 (vector-ref ready-segments (- column-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref rest-segments (- row-id 1)))
                    (private-is-... (vector-ref rest-segments (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))
          ;can't skipped!
          ; (if (equal? 'unused (matrix-take row-id column-id))
          ;   (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))
        ]
        [(and 
            (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
            (private-is-... (vector-ref ready-segments (- column-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref rest-segments (- row-id 1)))
                    (private-is-... (vector-ref rest-segments (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))
          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id))
            ;here won't involve misunderstandings
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))]
        [(equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)]

        [(and 
            (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
            (private-is-**1 (vector-ref rest-segments (- row-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref ready-segments (- column-id 1)))
                    (private-is-... (vector-ref ready-segments (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))
          ;can't be skipped!
          ; (if (equal? 'unused (matrix-take row-id column-id))
          ;   (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))
        ]
        [(and 
            (equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
            (private-is-... (vector-ref rest-segments (- row-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref ready-segments (- column-id 1)))
                    (private-is-... (vector-ref ready-segments (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))
          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id))
            ;here won't involve misunderstanding
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))]
        [(equal? 'skipped (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)]

        ;debug
        ; [else 
        ;   (pretty-print '??)
        ;   (pretty-print (vector-ref ready-segments (- column-id 1)))
        ;   (pretty-print (vector-ref rest-segments (- row-id 1)))
        ;   (pretty-print (matrix-take matrix (+ 1 (vector-length ready-segments)) (- row-id 1) column-id))
        ;   (pretty-print (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (- column-id 1)))
        ; ]
      )
      matrix]))

(define private-next-step-ok? 
  (case-lambda 
    [(matrix rest-segments ready-segments row-id column-id status) 
      (if (private-next-step-ok? matrix rest-segments ready-segments row-id column-id status 'right)
        #t
        (private-next-step-ok? matrix rest-segments ready-segments row-id column-id status 'down))]
    [(matrix rest-segments ready-segments row-id column-id status direction)
      (cond 
        [(and (equal? 'right direction) (< column-id (vector-length ready-segments)))
          (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id status)
          (private-segments->match-matrix matrix rest-segments ready-segments row-id (+ column-id 1))
          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id (+ column-id 1)))
            (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id 'unused))
          (not (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id)))]
        [(and (equal? 'down direction) (< row-id (vector-length rest-segments)))
          (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id status)
          (private-segments->match-matrix matrix rest-segments ready-segments (+ row-id 1) column-id)
          (if (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) (+ row-id 1) column-id))
            (matrix-set! matrix (+ 1 (vector-length ready-segments)) row-id column-id 'unused))
          (not (equal? 'unused (matrix-take matrix (+ 1 (vector-length ready-segments)) row-id column-id)))]
        [else #f])]))

(define (private-is-... segment) 
  (equal? '... (segment-tail segment)))

(define (private-is-**1 segment) 
  (equal? '**1 (segment-tail segment)))


(define (private-segment rule-list)
  (list->vector 
    (fold-left
      (lambda (result current-rule)
        (if (or 
            (equal? current-rule '...) 
            (equal? current-rule '**1))
          (if (null? result)
            (raise "wrong rule")
            (let ([current-segment (car (reverse result))])
              (if (null? (segment-tail current-segment))
                (begin
                  (segment-tail-set! current-segment current-rule)
                  result)
                (raise "wrong rule"))))
          (append result `(,(make-segment current-rule '())))))
      '()
      rule-list)))
)