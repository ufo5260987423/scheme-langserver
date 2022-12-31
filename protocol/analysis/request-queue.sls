(library (scheme-langserver protocol analysis request-queue)
  (export 
    init-request-queue
    request-queue-pop
    request-queue-push
    request-queue-peek)
  (import 
    (chezscheme)
    (slib queue)

    (scheme-langserver protocol analysis rules document-sync))

(define-record-type request-queue 
  	(fields 
		  (immutable mutex)
	  	(immutable condition)
	  	(immutable queue)))
   
(define (init-request-queue)
  	(make-request-queue (make-mutex) (make-condition) (make-queue)))

(define (request-queue-pop queue)
    (with-mutex (request-queue-mutex queue)
        (let loop ()
          	(if (queue-empty? (request-queue-queue queue))
              	(begin
                	(condition-wait (request-queue-condition queue) (request-queue-mutex queue))
                	(loop))
                (private-dequeue! (request-queue-queue queue))))))

(define (request-queue-push queue item)
    (with-mutex (request-queue-mutex queue)
    	(enqueue! (request-queue-queue queue) item))
  	(condition-signal (request-queue-condition queue)))

(define (request-queue-peek queue)
    (with-mutex (request-queue-mutex queue)
      (if (queue-empty? (request-queue-queue queue))
        '()
        (queue-front (request-queue-queue queue)))))

(define (private-dequeue! queue)
  (let loop ([request (dequeue! (request-queue-queue queue))])
    (if (queue-empty? (request-queue-queue queue))
      request
      (let ([result
          (filter
            (lambda (returned-request) (not (equal? returned-request request)))
            (map 
              (lambda (func) (func request queue))
              (list process-document-sync)))])
        (if (null? result)
          request
          (loop result))))))
)