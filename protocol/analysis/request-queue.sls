(library (scheme-langserver protocol analysis request-queue)
  (export 
    make-request-queue
    request-queue-pop
    request-queue-push)
  (import 
    (chezscheme)
    (slib queue)

    (scheme-langserver util association)
    (scheme-langserver protocol request))

(define-record-type request-queue 
  (fields 
    (immutable mutex)
    (immutable condition)
    (immutable queue)
    (mutable cancelable-task-list))
  (protocol
    (lambda (new)
      (lambda ()
        (new (make-mutex) (make-condition) (make-queue) '())))))

(define-record-type cancelable-task 
  (fields 
    (immutable request)
    (immutable mutex)
    (mutable canceled?))
  (protocol
    (lambda (new)
      (lambda (request)
        (new request (make-mutex) #f)))))

(define (cancel task)
  (with-mutex (cancelable-task-mutex task)
    (cancelable-task-canceled?-set! task #t)))

(define (request-queue-pop queue request-processor)
  (with-mutex (request-queue-mutex queue)
    (let loop ()
      (if (queue-empty? (request-queue-queue queue))
        (begin
          (condition-wait (request-queue-condition queue) (request-queue-mutex queue))
          (loop))
        (letrec* ([task (dequeue! (request-queue-queue queue))]
            [request (cancelable-task-request task)]
            [ticks 100000]
            [job (lambda () (request-processor request))]
            ;will be in another thread
            [complete 
              (lambda (ticks value) 
                (cancel task)
                (remove:from-request-cancelable-task-list queue task)
                value)]
            ;will be in another thread
            [expire 
              (lambda (remains) 
                (if (cancelable-task-canceled? task)
                  (begin 
                    (remove:from-request-cancelable-task-list queue task)
                    (request-processor (make-request (request-id request) "$/cancelRequest" (make-alist 'method (request-method request)))))
                  (remains ticks complete expire)))])
          ;will be in another thread
          (lambda () ((make-engine job) ticks complete expire)))))))

(define (remove:from-request-cancelable-task-list queue task)
  (with-mutex (request-queue-mutex queue)
    (request-queue-cancelable-task-list-set! 
      queue
      (remove task (request-queue-cancelable-task-list queue)))))

(define (request-queue-push queue request)
  (with-mutex (request-queue-mutex queue)
    (cond 
      [(equal? (request-method request) "$/cancelRequest")
        (let* ([id (assq-ref (request-params request) 'id)]
            [pure-queue (request-queue-queue queue)]
            ;here, id is cancel target id
            [predicator (lambda (task) (equal? id (request-id (cancelable-task-request task))))]
            [target-task (find predicator (request-queue-cancelable-task-list queue))])
          ;must cancel in local thread.
          (when target-task (cancel target-task)))]
      [else 
        (let ([target-task (make-cancelable-task request)])
          (enqueue! (request-queue-queue queue) target-task)
          (request-queue-cancelable-task-list-set! 
            queue
            `(,@(request-queue-cancelable-task-list queue) ,target-task)))]))
  (condition-signal (request-queue-condition queue)))
)