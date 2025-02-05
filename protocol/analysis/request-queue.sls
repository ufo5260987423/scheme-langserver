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
        (let ([task (dequeue! (request-queue-queue queue))]
            ; Each tick corresponds roughly to one nonleaf procedure call
            [ticks 100])
          (request-queue-cancelable-task-list-set! 
            queue
            (append 
              (filter 
                (lambda (t) 
                  ;here, canceled? check occured in local thread. It may be inacuracy because conflict with another thread. But this is enough.
                  (not (cancelable-task-canceled? t))) 
                (request-queue-cancelable-task-list queue))
              `(,task)))
          ;will be in another thread
          (lambda ()
            ((call/1cc 
              (lambda (return)
                (timer-interrupt-handler
                  (lambda ()
                    (with-mutex (cancelable-task-mutex task)
                      (if (cancelable-task-canceled? task)
                        (return '())))
                    (set-timer ticks)))
                ;check `canceled?` immidiately
                (set-timer 1)
                (request-processor (cancelable-task-request task))
                ;this may cause conflict with local thread. But it's ok
                (cancel task))))))))))

(define (request-queue-push queue request)
  (let ([id (request-id request)])
    (with-mutex (request-queue-mutex queue)
      (cond 
        [(equal? (request-method request) "$/cancelRequest")
          (let* ([pure-queue (request-queue-queue queue)]
              ;here, id is cancel target id
              [predicator (lambda (task) (equal? id (request-id (cancelable-task-request task))))]
              [target-task (find predicator (request-queue-cancelable-task-list queue))])
            ;must cancel in local thread.
            (when target-task (cancel target-task)))]
        [else 
          (enqueue! (request-queue-queue queue) (make-cancelable-task request))])))
  (condition-signal (request-queue-condition queue)))
)