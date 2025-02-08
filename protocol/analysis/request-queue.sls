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
    (mutable tickal-task-list))
  (protocol
    (lambda (new)
      (lambda ()
        (new (make-mutex) (make-condition) (make-queue) '())))))

(define ticks 10000)

(define-record-type tickal-task 
  (fields 
    (immutable request)
    (immutable request-queue)
    (immutable mutex)
    (mutable expire)
    (mutable complete))
  (protocol
    ;must have request-queue-mutex
    (lambda (new)
      (lambda (request request-queue)
        (letrec* ([new-task (new request request-queue (make-mutex) '() '())]
            [complete 
              (lambda (ticks value) 
                (remove:from-request-tickal-task-list request-queue new-task)
                value)]
            [expire 
              (lambda (remains) 
                (let ([new-pair
                    (with-mutex (tickal-task-mutex new-task)
                      (cons 
                        (tickal-task-complete new-task)
                        (tickal-task-expire new-task)))])
                  (remains ticks (car new-pair) (cdr new-pair))))])
          (enqueue! (request-queue-queue request-queue) new-task)
          (request-queue-tickal-task-list-set! 
            request-queue
            `(,@(request-queue-tickal-task-list request-queue) ,new-task))

          (tickal-task-expire-set! new-task expire)
          (tickal-task-complete-set! new-task complete)

          new-task)))))

(define (request-queue-pop queue request-processor)
  (with-mutex (request-queue-mutex queue)
    (let loop ()
      (if (queue-empty? (request-queue-queue queue))
        (begin
          (condition-wait (request-queue-condition queue) (request-queue-mutex queue))
          (loop))
        (letrec* ([task (dequeue! (request-queue-queue queue))]
            [request (tickal-task-request task)]
            [job (lambda () (request-processor request))])
          ;will be in another thread
          (lambda () ((make-engine job) ticks (tickal-task-complete task) (tickal-task-expire task))))))))

(define (remove:from-request-tickal-task-list queue task)
  (with-mutex (request-queue-mutex queue)
    (request-queue-tickal-task-list-set! 
      queue
      (remove task (request-queue-tickal-task-list queue)))))

(define (request-queue-push queue request potential-request-processor)
  (with-mutex (request-queue-mutex queue)
    (cond 
      [(equal? (request-method request) "$/cancelRequest")
        (let* ([id (assq-ref (request-params request) 'id)]
            [pure-queue (request-queue-queue queue)]
            ;here, id is cancel target id
            [predicator (lambda (task) (equal? id (request-id (tickal-task-request task))))]
            [tickal-task (find predicator (request-queue-tickal-task-list queue))])
          ;must cancel in local thread.
          (when tickal-task 
            (with-mutex (tickal-task-mutex tickal-task)
              (tickal-task-expire-set! tickal-task
                (lambda (remains)
                  (remove:from-request-tickal-task-list queue tickal-task)
                  (potential-request-processor (make-request id "$/cancelRequest" (make-alist 'method (request-method (tickal-task-request tickal-task))))))))))]
      [else (make-tickal-task request queue)]))
  (condition-signal (request-queue-condition queue)))
)