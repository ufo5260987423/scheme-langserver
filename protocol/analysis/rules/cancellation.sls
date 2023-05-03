(library (scheme-langserver protocol analysis rules cancellation)
  (export 
    process-cancellation)
  (import 
    (chezscheme)
    (slib queue)
    (scheme-langserver util path)
    (scheme-langserver util association)    

    (scheme-langserver protocol request)
    (scheme-langserver protocol analysis util)
    (scheme-langserver protocol alist-access-object)

    (scheme-langserver util association))

(define (process-cancellation head-request queue)
  (if (queue-empty? queue)
    head-request
    (let* ([head-id (request-id head-request)]
        [head-method (request-method head-request)]
        [predicator 
          (lambda (request) 
            (and (equal? head-id (request-id request))) (equal? "$/cancelRequest" (request-method request)))]
        [result (scan-queue&pick-out queue predicator)])
      (if (null? result)
        head-request
        (private-request->canceled (car (reverse head-request)) head-method)))))

(define (private-request->canceled request method)
  (make-request 
    (request-id request)
    (request-method request)
    (make-alist 'method method)))
)