#!r6rs

(library (ufo-thread-pool util blocking-queue)
	(export 
		init-blocking-queue
		blocking-queue-pop
		blocking-queue-push)
  (import (chezscheme) (slib queue))

(define-record-type blocking-queue 
  	(fields 
		(immutable mutex)
	  	(immutable condition)
	  	(immutable queue)))
   
(define (init-blocking-queue)
  	(make-blocking-queue (make-mutex) (make-condition) (make-queue)))

(define (blocking-queue-pop queue)
    (with-mutex (blocking-queue-mutex queue)
        (let loop ()
          	(if (queue-empty? (blocking-queue-queue queue))
              	(begin
                	(condition-wait (blocking-queue-condition queue) (blocking-queue-mutex queue))
                	(loop))
	      		(dequeue! (blocking-queue-queue queue))))))

(define (blocking-queue-push queue item)
    (with-mutex (blocking-queue-mutex queue)
    	(enqueue! (blocking-queue-queue queue) item))
  	(condition-signal (blocking-queue-condition queue)))
)
