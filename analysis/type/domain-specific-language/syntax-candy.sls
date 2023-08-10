(library (scheme-langserver analysis type domain-specific-language syntax-candy)
  (export 
    candy:segmentable?
    candy:matchable?
    candy:match)
  (import 
    (chezscheme)
    (scheme-langserver util matrix)
    (scheme-langserver util contain)
    (scheme-langserver util try))

(define (candy:segmentable? target)
  (try
    (private-segment target)
    #t
    (except c 
      (else #f))))

(define (candy:matchable? parameter-template argument-list)
  (equal? 'skipped 
    (vector-ref (private-segments->match-matrix (private-segment parameter-template) (private-segment argument-list)) 0)))

;NOTE: a complecated case is like regexes abc+ and ab...c
(define candy:match 
  (case-lambda
    [(parameter-template argument-list)
      (let* ([rest-segment (private-segment parameter-template)]
          [ready-segment (private-segment argument-list)]
          [matrix (private-segments->match-matrix rest-segment ready-segment)])
        (if (equal? 'skipped (vector-ref matrix 0))
          '()
          (candy:match matrix rest-segment ready-segment 0 0)))]
    [(matrix rest-segments ready-segments row-id column-id)
      (if (or (> row-id (length rest-segments))
          (> column-id (length ready-segments)))
        '()
        (let* ([current-value (matrix-take matrix row-id column-id)]
            [rest-segment (vector-ref (list->vector rest-segments) row-id)]
            [ready-segment (vector-ref (list->vector ready-segments) column-id)])
          (cond 
            [(equal? current-value 'matched)
              `(,`(,rest-segment . ,ready-segment) 
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
        (make-vector (* (+ 1 (length rest-segments)) (+ 1 (length rest-segments))) 'unused)
        rest-segments 
        ready-segments
        0 
        0)]
    [(matrix rest-segments ready-segments row-id column-id)
      (cond
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;In start zone
        [(and (zero? row-id) (zero? column-id)) 
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped)]

        ;skipped and continue at two directions
        [(and (zero? row-id) (< 0 column-id))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))
            (if (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1)))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right)))]
        [(and (zero? column-id) (< 0 row-id))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))
            (if (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1)))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down)))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;In the core, a most basic idea is to recure the only one comming path,
        ;'matched means both of previous ready-segment and rest-segment are matched, not only one of them.
        ;each clause should first check itself is possible

        ;'matched
        [(and 
            (equal? 'matched (matrix-take matrix (- row-id 1) column-id))
            ;in this case, ... and **1 are the same
            (or 
              (private-is-**1 (vector-ref (list->vector ready-segments) (- column-id 1)))
              (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1)))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref (list->vector rest-segments) (- row-id 1)))
                    (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))

          (if (equal? 'unused (matrix-take matrix row-id column-id))
            ;here can't go down for which might leading misunderstanding semantic
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))]
        [(equal? 'matched (matrix-take matrix (- row-id 1) column-id))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right)]

        [(and 
            (equal? 'matched (matrix-take matrix row-id (- column-id 1)))
            ;in this case, ... and **1 are the same
            (or 
              (private-is-**1 (vector-ref (list->vector rest-segments) (- row-id 1)))
              (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1)))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref (list->vector ready-segments) (- column-id 1)))
                    (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))

          (if (equal? 'unused (matrix-take matrix row-id column-id))
            ;here can't go down for which might leading misunderstanding semantic
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))]
        [(equal? 'matched (matrix-take matrix row-id (- column-id 1)))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down)]

        ;'skipped
        ;'skipped means both of previous ready-segment and rest-segment are skipped, not only one of them,
        ;and current segments are innocent. Whicn means that ... and **1 won't skip ... items.
        ;So, **1 , ... and trivial segments are supposed to get matched here and seperately go to different directions.
        [(and 
            (equal? 'skipped (matrix-take matrix (- row-id 1) column-id))
            (private-is-**1 (vector-ref (list->vector ready-segments) (- column-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref (list->vector rest-segments) (- row-id 1)))
                    (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))
          ;can't skipped!
          ; (if (equal? 'unused (matrix-take row-id column-id))
          ;   (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))
        ]
        [(and 
            (equal? 'skipped (matrix-take matrix (- row-id 1) column-id))
            (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down))
            (if (or (private-is-**1 (vector-ref (list->vector rest-segments) (- row-id 1)))
                    (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)))
          (if (equal? 'unused (matrix-take matrix row-id column-id))
            ;here won't involve misunderstandings
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'right))]
        [(equal? 'skipped (matrix-take matrix (- row-id 1) column-id))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right)]

        [(and 
            (equal? 'skipped (matrix-take matrix row-id (- column-id 1)))
            (private-is-**1 (vector-ref (list->vector rest-segments) (- row-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref (list->vector ready-segments) (- column-id 1)))
                    (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))
          ;can't be skipped!
          ; (if (equal? 'unused (matrix-take row-id column-id))
          ;   (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))
        ]
        [(and 
            (equal? 'skipped (matrix-take matrix row-id (- column-id 1)))
            (private-is-... (vector-ref (list->vector rest-segments) (- row-id 1))))
          (if (not (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'right))
            (if (or (private-is-**1 (vector-ref (list->vector ready-segments) (- column-id 1)))
                    (private-is-... (vector-ref (list->vector ready-segments) (- column-id 1))))
              (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)))
          (if (equal? 'unused (matrix-take matrix row-id column-id))
            ;here won't involve misunderstanding
            (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'skipped 'down))]
        [(equal? 'skipped (matrix-take matrix row-id (- column-id 1)))
          (private-next-step-ok? matrix rest-segments ready-segments row-id column-id 'matched 'down)]

        ;debug
        ; [else 
        ;   (pretty-print '??)
        ;   (pretty-print (vector-ref (list->vector ready-segments) (- column-id 1)))
        ;   (pretty-print (vector-ref (list->vector rest-segments) (- row-id 1)))
        ;   (pretty-print (matrix-take matrix (- row-id 1) column-id))
        ;   (pretty-print (matrix-take matrix row-id (- column-id 1)))
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
        [(and (equal? 'right direction) (< column-id (length ready-segments)))
          (matrix-set! matrix row-id column-id status)
          (if (< column-id (length ready-segments))
            (begin 
              (private-segments->match-matrix matrix rest-segments ready-segments row-id (+ column-id 1)))
              (if (equal? 'unused (matrix-take matrix row-id (+ column-id 1)))
                (matrix-set! matrix row-id column-id 'unused)))
          (not (equal? 'unused (matrix-take matrix row-id column-id)))]
        [(and (equal? 'down direction) (< row-id (length rest-segments)))
          (matrix-set! matrix row-id column-id status)
          (if (< row-id (length rest-segments))
            (begin
              (private-segments->match-matrix matrix rest-segments ready-segments (+ row-id 1) column-id)
              (if (equal? 'unused (matrix-take matrix (+ row-id 1) column-id))
                (matrix-set! matrix row-id column-id 'unused))))
          (not (equal? 'unused (matrix-take matrix row-id column-id)))]
        [else #f])]))

(define (private-is-... segment) 
  (if (list? segment)
    (equal? '... (car (reverse segment))))
    #f)

(define (private-is-**1 segment) 
  (if (list? segment)
    (equal? '**1 (car (reverse segment))))
    #f)

(define (private-type-of segment) 
  (if (list? segment)
    (car segment)
    segment))

(define (private-segment rule-list)
  (let loop ([loop-body rule-list] [result '()])
    (if (null? loop-body)
      result
      (cond
        [(equal? (car loop-body) '...) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (equal? (car (reverse result)) '...)
                  (equal? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,(list (car (reverse result)) '...)))))))]
        [(equal? (car loop-body) '**1) 
          (if (null? result)
            (raise "wrong rule")
            (begin
              (if (or 
                  (equal? (car (reverse result)) '...)
                  (equal? (car (reverse result)) '**1))
                (raise "wrong rule")
                (loop 
                  (cdr loop-body) 
                  (append 
                    (reverse (cdr (reverse result))) 
                    `(,(list (car (reverse result)) '**1)))))))]
        [else (loop (cdr loop-body) (append result `(,(car loop-body))))]))))
)