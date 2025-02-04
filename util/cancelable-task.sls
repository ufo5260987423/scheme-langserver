(library (scheme-langserver cancelable-task)
  (export 
    add&return:cancelable-task
    cancel)
  (import 
    (chezscheme)
    (ufo-thread-pool))

(define-record-type cancelable-task 
  (fields 
    (immutable mutex)
    (mutable canceled?))
  (protocol
    (lambda (new)
      (lambda ()
        (new (make-mutex) #f)))))

(define (add&return:cancelable-task thread-pool job-thunk ticks)
  (let ([task (make-cancelable-task)])
    (thread-pool-add-job thread-pool 
      (lambda ()
        ((call/1cc 
          (lambda (return)
            (timer-interrupt-handler
              (lambda ()
                (with-mutex (cancelable-task-mutex task)
                  (if (cancelable-task-canceled? task)
                    (return '())))
                (set-timer ticks)))
            (set-timer ticks)
            (return (job-thunk)))))))
    task))
(define (cancel task)
  (with-mutex (cancelable-task-mutex task)
    (cancelable-task-canceled?-set! task #f)))
)