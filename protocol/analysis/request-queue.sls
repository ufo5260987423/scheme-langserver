(library (scheme-langserver protocol analysis request-queue)
  (export 
    init-request-queue
    request-queue-pop
    request-queue-push)
  (import 
    (chezscheme)
    (slib queue)

    (scheme-langserver util association)
    (scheme-langserver protocol request)
    (scheme-langserver protocol analysis util))

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
        (dequeue! (request-queue-queue queue))))))

(define (request-queue-push queue request)
  (with-mutex (request-queue-mutex queue)
    (cond 
      [(equal? (request-method request) "$/cancelRequest")
        (let* ([pure-queue (request-queue-queue queue)]
            [cancel-target-id (request-id request)]
            [predicator (lambda (request-in-queue) (equal? cancel-target-id (request-id request)))]
            [replace-maker (lambda (request)
              (make-request 
                cancel-target-id
                "$/cancelRequest"
                (make-alist 'method (request-method request))))])
          (scan-queue&replace pure-queue predicator replace-maker))]
      [else (enqueue! (request-queue-queue queue) request)]))
  (condition-signal (request-queue-condition queue)))
)