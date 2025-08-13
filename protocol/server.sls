(library (scheme-langserver protocol server)
  (export 

    do-log
    do-log-timestamp

    make-server
    server?
    server-mutex
    server-shutdown?
    server-shutdown?-set!
    server-workspace
    server-workspace-set!
    server-thread-pool
    server-input-port
    server-output-port
    server-type-inference?
    ;close
    server-condition
    server-request-queue
    server-top-environment

    server-work-done-progress?
    server-work-done-progress?-set!)
  (import (chezscheme))

(define-record-type server
  (fields 
    (immutable input-port)
    (immutable output-port)
    (immutable log-port)
    ; only have 1 thread, in order to asynchronize request processing
    (immutable thread-pool)
    ;;for output-port
    (immutable mutex)
    (immutable request-queue)
    (immutable type-inference?)
    (mutable workspace)
    (mutable shutdown?)
    (mutable condition)
    (mutable work-done-progress?)
    (immutable top-environment)
    )
  (protocol
    (lambda (new)
      (case-lambda
        [(input-port output-port log-port thread-pool request-queue workspace type-inference?)
          (new 
            input-port 
            output-port 
            log-port 
            thread-pool
            (if (null? thread-pool) '() (make-mutex))
            request-queue
            type-inference?
            workspace
            #f
            (make-condition)
            #f
            'r6rs)]
        [(input-port output-port log-port thread-pool request-queue workspace type-inference? top-environment)
          (new 
            input-port 
            output-port 
            log-port 
            thread-pool
            (if (null? thread-pool) '() (make-mutex))
            request-queue
            type-inference?
            workspace
            #f
            (make-condition)
            #f
            top-environment)]))))

(define (do-log message server-instance)
  (if (not (null? (server-log-port server-instance)))
    (begin 
      (put-string (server-log-port server-instance) message)
      (put-string (server-log-port server-instance) "\n")
      (flush-output-port (server-log-port server-instance)))))

(define (do-log-timestamp server-instance)
  (let* ([date (current-date)]
      [current-date-string 
        (fold-left 
          (lambda (h t) (string-append h " " t )) 
          (number->string (date-year date))
          (map 
            number->string 
            (map 
              (lambda (f) (f date))
              (list date-month date-day date-hour date-minute date-second date-nanosecond))))])
    (if (not (null? (server-log-port server-instance)))
      (begin 
        (put-string (server-log-port server-instance) current-date-string)
        (put-string (server-log-port server-instance) "\n")
        (flush-output-port (server-log-port server-instance))))))
)
