(library (scheme-langserver protocol analysis request-queue)
  (export 
    request-queue-peek)
  (import 
    (chezscheme)
    (slib queue)

    (scheme-langserver protocol analysis rules document-sync))

(define-record-type request-request-queue 
  	(fields 
		  (immutable mutex)
	  	(immutable condition)
	  	(immutable queue)
      (mutable timeout-second)))
   
(define (init-request-queue)
  	(make-request-queue (make-mutex) (make-condition) (make-queue) 1))

(define (request-queue-pop queue)
    (with-mutex (request-queue-mutex queue)
        (let loop ()
          	(if (queue-empty? (request-queue-queue queue))
              	(begin
                	(condition-wait (request-queue-condition queue) (queue-mutex queue))
                	(loop))
                (prevate-dequeue! queue)))))

(define (request-queue-push queue item)
    (with-mutex (request-queue-mutex queue)
    	(enqueue! (request-queue-queue queue) item))
  	(condition-signal (request-queue-condition queue)))

(define (request-queue-peek queue)
    (with-mutex (request-queue-mutex queue)
      (if (queue-empty? (request-queue-queue queue))
        '()
        (queue-front (request-queue-queue queue)))))

(define (prevate-dequeue! queue)
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