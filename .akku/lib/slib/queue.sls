#!r6rs
;; Akku.scm wrote this file based on "slib-queue-SLIB-3b5-r7rs/slib/queue.sld"
;; "queue.scm"  Queues/Stacks for Scheme
;; Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;;
;; This code is in the public domain.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(library
  (slib queue)
  (export
    make-queue
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
  (define-record-type
    <queue>
    (queue first-pair last-pair)
    queue?
    (first-pair queue-first queue-first-set!)
    (last-pair queue-last queue-last-set!))
  (define (make-queue) (queue '() '()))
  (define (queue-empty? q) (null? (queue-first q)))
  (define (queue-push! q datum)
    (let* ((old-first-pair (queue-first q))
           (new-first-pair (cons datum old-first-pair)))
      (queue-first-set! q new-first-pair)
      (when (null? old-first-pair)
            (queue-last-set! q new-first-pair)))
    q)
  (define (enqueue! q datum)
    (let ((new-pair (cons datum '())))
      (if (null? (queue-first q))
        (queue-first-set! q new-pair)
        (set-cdr! (queue-last q) new-pair))
      (queue-last-set! q new-pair))
    q)
  (define (dequeue! q)
    (let ((first-pair (queue-first q)))
      (if (null? first-pair)
        (error "queue is empty" q)
        (let ((first-cdr (cdr first-pair)))
          (queue-first-set! q first-cdr)
          (when (null? first-cdr) (queue-last-set! q '()))
          (car first-pair)))))
  (define queue-pop! dequeue!)
  (define (dequeue-all! q)
    (let ((lst (queue-first q)))
      (queue-first-set! q '())
      (queue-last-set! q '())
      lst))
  (define (queue-front q)
    (let ((first-pair (queue-first q)))
      (if (null? first-pair)
        (error "queue is empty" q)
        (car first-pair))))
  (define (queue-rear q)
    (let ((last-pair (queue-last q)))
      (if (null? last-pair)
        (error "queue is empty" q)
        (car last-pair)))))
