;; "queue.scm"  Queues/Stacks for Scheme
;; Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;;
;; This code is in the public domain.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib queue)
  (export make-queue
          queue?
          queue-empty?
          queue-push!
          enqueue!
          dequeue!
          queue-pop!
          dequeue-all!
          queue-front
          queue-rear)
  (import (scheme base))

  (begin

    ;;@code{(require 'queue)}
    ;;@ftindex queue
    ;;
    ;;A @dfn{queue} is a list where elements can be added to both the front
    ;;and rear, and removed from the front (i.e., they are what are often
    ;;called @dfn{dequeues}).  A queue may also be used like a stack.

    ;; Elements in a queue are stored in a list.  The last pair in the list
    ;; is stored in the queue type so that datums can be added in constant
    ;; time.

    (define-record-type <queue>
                        (queue first-pair last-pair)
                        queue?
                        (first-pair queue-first queue-first-set!)
                        (last-pair queue-last queue-last-set!))

    ;;@args
    ;;Returns a new, empty queue.
    (define (make-queue)
      (queue '() '()))

    ;;@body
    ;;Returns @code{#t} if the queue @var{q} is empty.
    (define (queue-empty? q)
      (null? (queue-first q)))

    ;;@body
    ;;Adds @var{datum} to the front of queue @var{q}.
    (define (queue-push! q datum)
      (let* ((old-first-pair (queue-first q))
             (new-first-pair (cons datum old-first-pair)))
        (queue-first-set! q new-first-pair)
        (when (null? old-first-pair)
          (queue-last-set! q new-first-pair)))
      q)

    ;;@body
    ;;Adds @var{datum} to the rear of queue @var{q}.
    (define (enqueue! q datum)
      (let ((new-pair (cons datum '())))
        (if (null? (queue-first q))
          (queue-first-set! q new-pair)
          (set-cdr! (queue-last q) new-pair))
        (queue-last-set! q new-pair))
      q)

    ;;@body
    ;;@deffnx {Procedure} queue-pop! q
    ;;Both of these procedures remove and return the datum at the front of
    ;;the queue.  @code{queue-pop!} is used to suggest that the queue is
    ;;being used like a stack.
    (define (dequeue! q)
      (let ((first-pair (queue-first q)))
        (if (null? first-pair)
          (error "queue is empty" q)
          (let ((first-cdr (cdr first-pair)))
            (queue-first-set! q first-cdr)
            (when (null? first-cdr)
              (queue-last-set! q '()))
            (car first-pair)))))
    (define queue-pop! dequeue!)

    ;;@ All of the following functions raise an error if the queue @var{q}
    ;;is empty.

    ;;@body
    ;;Removes and returns (the list) of all contents of queue @var{q}.
    (define (dequeue-all! q)
      (let ((lst (queue-first q)))
        (queue-first-set! q '())
        (queue-last-set!  q '())
        lst))

    ;;@body
    ;;Returns the datum at the front of the queue @var{q}.
    (define (queue-front q)
      (let ((first-pair (queue-first q)))
        (if (null? first-pair)
          (error "queue is empty" q)
          (car first-pair))))

    ;;@body
    ;;Returns the datum at the rear of the queue @var{q}.
    (define (queue-rear q)
      (let ((last-pair (queue-last q)))
        (if (null? last-pair)
          (error "queue is empty" q)
          (car last-pair))))

    ))

