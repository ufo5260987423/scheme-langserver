(library (scheme-langserver protocol analysis request-queue)
  (export 
    make-request-queue
    request-queue-pop
    request-queue-push
    request-queue-empty?)
  (import 
    (chezscheme)
    (slib queue)

    (scheme-langserver util association)
    (scheme-langserver protocol request)
    (scheme-langserver analysis workspace))

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

;ticks is an empirical constant for Chez Scheme's engine time-slicing.
;It bounds how many abstract instructions a single task may execute before
;entering the expire callback.  The value 100000 was chosen to let typical
;LSP requests finish in one slice while forcing long-running analysis
;(e.g. type inference) to yield periodically so that cancellation can be checked.
(define ticks 100000)

(define-record-type tickal-task 
  (fields 
    (immutable request)
    (mutable stop?)
    (immutable expire)
    (immutable complete))
  (protocol
    ; Must have request-queue-mutex
    (lambda (new)
      (lambda (request request-queue workspace)
        (let ([new-task #f])
          (letrec ([complete 
              (lambda (ticks value) 
                (remove:from-request-tickal-task-list request-queue new-task)
                value)]
            ; This expire mainly aims to interrupt type inference, so that acquires workspace mutex.
            ; It shouldn't be supposed that it interrupt the workspace refreshing procedure.
            [expire 
              (lambda (remains) 
                (cond 
                  [(or 
                    (string=? "textDocument/didChange" (request-method request))
                    (string=? "textDocument/didOpen" (request-method request))
                    (string=? "textDocument/didClose" (request-method request)))
                    (remains ticks complete expire)]
                  [(tickal-task-stop? new-task)
                    (with-mutex (workspace-mutex workspace)
                      (remove:from-request-tickal-task-list request-queue new-task))]
                  [else (remains ticks complete expire)]))])
            (set! new-task (new request #f expire complete))
            (enqueue! (request-queue-queue request-queue) new-task)
            (request-queue-tickal-task-list-set! 
              request-queue
              (cons new-task (request-queue-tickal-task-list request-queue)))
            new-task))))))

(define (request-queue-empty? queue)
  (with-mutex (request-queue-mutex queue)
    (queue-empty? (request-queue-queue queue))))

(define (request-queue-pop queue request-processor)
  (with-mutex (request-queue-mutex queue)
    (let loop ()
      (when (queue-empty? (request-queue-queue queue))
        ; By default, this will release request-queue-mutex 
        ; and re-enter when request-queue-condition is signed.
        (condition-wait (request-queue-condition queue) (request-queue-mutex queue))
        (loop)))
    (let* ([task (dequeue! (request-queue-queue queue))]
          [request (tickal-task-request task)]
          [job (lambda () 
              (if (tickal-task-stop? task)
                (remove:from-request-tickal-task-list queue task)
                (request-processor request)))])
      ; May be called in the consumer thread or directly
      (lambda () ((make-engine job) ticks (tickal-task-complete task) (tickal-task-expire task))))))

(define (remove:from-request-tickal-task-list queue task)
  (with-mutex (request-queue-mutex queue)
    (request-queue-tickal-task-list-set! 
      queue
      (remq task (request-queue-tickal-task-list queue)))))

(define (request-queue-push queue request potential-request-processor workspace)
  (with-mutex (request-queue-mutex queue)
    (case (request-method request)
      ["private:publish-diagnoses"
        (let* ([predicator (lambda (task) (string=? "private:publish-diagnoses" (request-method (tickal-task-request task))))]
            [tickal-task (find predicator (request-queue-tickal-task-list queue))])
          (when (not tickal-task)
            (make-tickal-task request queue workspace)))]
      ["$/cancelRequest"
        (let ([id (assq-ref (request-params request) 'id)])
          (when id
            (let* ([predicator (lambda (task) (equal? id (request-id (tickal-task-request task))))]
                [tickal-task (find predicator (request-queue-tickal-task-list queue))])
              ;must cancel in local thread.
              (when tickal-task 
                (tickal-task-stop?-set! tickal-task #t)
                (potential-request-processor 
                  (make-request id "$/cancelRequest" (make-alist 'method (request-method (tickal-task-request tickal-task)))))))))]
      ["textDocument/didChange"
        (for-each
          (lambda (task)
            (let ([method (request-method (tickal-task-request task))])
              (when (or
                (string=? method "private:publish-diagnoses")
                (string=? method "textDocument/hover")
                (string=? method "textDocument/completion")
                (string=? method "textDocument/references")
                (string=? method "textDocument/definition")
                (string=? method "textDocument/documentSymbol")
                (string=? method "textDocument/diagnostic")
                (string=? method "textDocument/documentHighlight")
                (string=? method "textDocument/signatureHelp")
                (string=? method "textDocument/formatting")
                (string=? method "textDocument/prepareRename")
                (string=? method "textDocument/rangeFormatting")
                (string=? method "textDocument/onTypeFormatting"))
                (tickal-task-stop?-set! task #t))))
          (request-queue-tickal-task-list queue))
        (make-tickal-task request queue workspace)]
      [else (make-tickal-task request queue workspace)])
      ; Because the pool is limited to have only one thread.
      (condition-signal (request-queue-condition queue))))
)
