(library (scheme-langserver protocol server)
  (export 

    do-log

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
    ;close
    server-condition
    server-request-queue)
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
        (mutable workspace)
        (mutable shutdown?)
        (mutable condition))
  (protocol
    (lambda (new)
      (lambda (input-port output-port log-port thread-pool request-queue workspace)
        (new 
          input-port 
          output-port 
          log-port 
          thread-pool
          (if (null? thread-pool) '() (make-mutex))
          request-queue
          workspace
          #f
          (make-condition))))))

(define (do-log message server-instance)
    (if (not (null? (server-log-port server-instance)))
        (begin 
            (put-string (server-log-port server-instance) message)
            (put-string (server-log-port server-instance) "\n")
            (flush-output-port (server-log-port server-instance)))))
)