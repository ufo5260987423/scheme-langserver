(library (scheme-langserver protocol analysis util)
  (export 
    scan-queue&pick-out
    scan-queue&replace)
  (import 
    (chezscheme)
    (slib queue))

(define (scan-queue&pick-out pure-queue predicator)
  (if (queue-empty? pure-queue)
    '()
    (let* ([head-request (dequeue! pure-queue)]
        [tail (scan-queue&pick-out pure-queue predicator)])
      (if (predicator head-request)
        `(,head-request ,@tail)
        (begin
          (enqueue! pure-queue head-request)
          tail)))))

(define (scan-queue&replace pure-queue predicator replace-maker)
  (if (queue-empty? pure-queue)
    '()
    (let* ([head-request (dequeue! pure-queue)]
        [tail (scan-queue&pick-out pure-queue predicator)])
      (if (predicator head-request)
        `(,(replace-maker head-request) ,@tail)
        (begin
          (enqueue! pure-queue head-request)
          tail)))))
)