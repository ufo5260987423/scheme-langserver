#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver protocol analysis request-queue)
    (scheme-langserver protocol request)
    (scheme-langserver analysis workspace))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/empty-project"))

(define workspace (init-workspace fixture 'txt 'r6rs #t #f))

;; ------------------------------------------------------------------
;; 1. basic creation and emptiness
;; ------------------------------------------------------------------
(test-begin "request-queue-basic")
(let ([queue (make-request-queue)])
  (test-equal "empty after creation" #t (request-queue-empty? queue)))
(test-end)

;; ------------------------------------------------------------------
;; 2. basic push / pop / processor invocation
;; ------------------------------------------------------------------
(test-begin "request-queue-push-pop")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (cons (request-method r) calls)))]
    [req (make-request 1 "textDocument/hover" '())])
  (request-queue-push queue req processor workspace)
  (test-equal "not empty after push" #f (request-queue-empty? queue))
  
  (let ([thunk (request-queue-pop queue processor)])
    (test-equal "pop returns procedure" #t (procedure? thunk))
    (thunk))
  
  (test-equal "processor called once" 1 (length calls))
  (test-equal "processor got hover" "textDocument/hover" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 3. private:publish-diagnoses deduplication
;; ------------------------------------------------------------------
(test-begin "request-queue-dedup-publish-diagnoses")
(let* ([queue (make-request-queue)]
    [processor (lambda (r) 'ok)]
    [req1 (make-request '() "private:publish-diagnoses" '())]
    [req2 (make-request '() "private:publish-diagnoses" '())])
  (request-queue-push queue req1 processor workspace)
  (request-queue-push queue req2 processor workspace)
  
  (let ([thunk (request-queue-pop queue processor)])
    (thunk))
  (test-equal "queue empty after single pop (dedup worked)" #t (request-queue-empty? queue)))
(test-end)

;; ------------------------------------------------------------------
;; 4. $/cancelRequest sets stop? on pending task
;; ------------------------------------------------------------------
(test-begin "request-queue-cancel-request")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (cons (request-method r) calls)))]
    [req (make-request 42 "textDocument/completion" '())]
    [cancel (make-request '() "$/cancelRequest" (list (cons 'id 42)))])
  (request-queue-push queue req processor workspace)
  (request-queue-push queue cancel processor workspace)
  
  (let ([thunk (request-queue-pop queue processor)])
    (thunk))
  
  ; Note: current implementation invokes potential-request-processor for the
  ; cancelRequest itself inside the push.  The original req is skipped because
  ; stop? = #t, so the only call we see is the cancelRequest notification.
  (test-equal "original request was cancelled" 1 (length calls))
  (test-equal "cancelRequest invoked processor" "$/cancelRequest" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 5. malformed $/cancelRequest (missing id) does not cancel notifications
;; ------------------------------------------------------------------
(test-begin "request-queue-bad-cancel")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (cons (request-method r) calls)))]
    [notif (make-request '() "private:publish-diagnoses" '())]
    [bad-cancel (make-request '() "$/cancelRequest" '())])
  (request-queue-push queue notif processor workspace)
  (request-queue-push queue bad-cancel processor workspace)
  
  (test-equal "notification still in queue" #f (request-queue-empty? queue))
  (let ([thunk (request-queue-pop queue processor)])
    (thunk))
  (test-equal "queue empty after pop" #t (request-queue-empty? queue))
  (test-equal "notification ran" 1 (length calls))
  (test-equal "notification method" "private:publish-diagnoses" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 6. textDocument/didChange cancels stale document-state requests
;; ------------------------------------------------------------------
(test-begin "request-queue-didChange-cancels-stale")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (cons (request-method r) calls)))]
    [hover (make-request 10 "textDocument/hover" '())]
    [pub (make-request '() "private:publish-diagnoses" '())]
    [change (make-request '() "textDocument/didChange" '())])
  (request-queue-push queue hover processor workspace)
  (request-queue-push queue pub processor workspace)
  (request-queue-push queue change processor workspace)
  
  ; Pop order is FIFO: hover, pub, change.
  ; didChange push marked both hover and pub as stop?.
  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))
  (let ([th3 (request-queue-pop queue processor)])
    (th3))
  
  (test-equal "queue empty after three pops" #t (request-queue-empty? queue))
  ; Only didChange should have invoked the processor.
  (test-equal "only didChange ran" 1 (length calls))
  (test-equal "didChange method" "textDocument/didChange" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 7. FIFO ordering for multiple generic requests
;; ------------------------------------------------------------------
(test-begin "request-queue-fifo-order")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [req1 (make-request 1 "textDocument/hover" '())]
    [req2 (make-request 2 "textDocument/completion" '())]
    [req3 (make-request 3 "textDocument/definition" '())])
  (request-queue-push queue req1 processor workspace)
  (request-queue-push queue req2 processor workspace)
  (request-queue-push queue req3 processor workspace)

  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))
  (let ([th3 (request-queue-pop queue processor)])
    (th3))

  (test-equal "queue empty after three pops" #t (request-queue-empty? queue))
  (test-equal "processor called three times" 3 (length calls))
  (test-equal "fifo order preserved"
    '("textDocument/hover" "textDocument/completion" "textDocument/definition")
    calls))
(test-end)

;; ------------------------------------------------------------------
;; 8. cancelRequest only affects the matching id
;; ------------------------------------------------------------------
(test-begin "request-queue-cancel-specific-id")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [req-a (make-request 7 "textDocument/hover" '())]
    [req-b (make-request 8 "textDocument/completion" '())]
    [cancel (make-request '() "$/cancelRequest" (list (cons 'id 7)))])
  (request-queue-push queue req-a processor workspace)
  (request-queue-push queue req-b processor workspace)
  (request-queue-push queue cancel processor workspace)

  ; cancelRequest itself is processed synchronously inside push.
  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))

  (test-equal "queue empty after two pops" #t (request-queue-empty? queue))
  (test-equal "two calls total (cancel + req-b)" 2 (length calls))
  (test-equal "cancelRequest processed first" "$/cancelRequest" (car calls))
  (test-equal "req-b ran after cancel" "textDocument/completion" (cadr calls)))
(test-end)

;; ------------------------------------------------------------------
;; 9. didChange does not cancel arbitrary / non-document-state methods
;; ------------------------------------------------------------------
(test-begin "request-queue-didChange-not-cancel-arbitrary")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [code-action (make-request 20 "textDocument/codeAction" '())]
    [change (make-request '() "textDocument/didChange" '())])
  (request-queue-push queue code-action processor workspace)
  (request-queue-push queue change processor workspace)

  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))

  (test-equal "queue empty after two pops" #t (request-queue-empty? queue))
  (test-equal "both requests ran" 2 (length calls))
  (test-equal "codeAction preserved" "textDocument/codeAction" (car calls))
  (test-equal "didChange ran" "textDocument/didChange" (cadr calls)))
(test-end)

;; ------------------------------------------------------------------
;; 10. didChange does not cancel document sync requests (didOpen/didClose)
;; ------------------------------------------------------------------
(test-begin "request-queue-didChange-not-cancel-sync")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [open-req (make-request '() "textDocument/didOpen" '())]
    [change (make-request '() "textDocument/didChange" '())]
    [close-req (make-request '() "textDocument/didClose" '())])
  (request-queue-push queue open-req processor workspace)
  (request-queue-push queue change processor workspace)
  (request-queue-push queue close-req processor workspace)

  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))
  (let ([th3 (request-queue-pop queue processor)])
    (th3))

  (test-equal "queue empty after three pops" #t (request-queue-empty? queue))
  (test-equal "all three sync requests ran" 3 (length calls))
  (test-equal "didOpen ran first" "textDocument/didOpen" (car calls))
  (test-equal "didChange ran second" "textDocument/didChange" (cadr calls))
  (test-equal "didClose ran third" "textDocument/didClose" (caddr calls)))
(test-end)

;; ------------------------------------------------------------------
;; 11. publish-diagnoses dedup survives interleaved requests
;; ------------------------------------------------------------------
(test-begin "request-queue-dedup-interleaved")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [pub1 (make-request '() "private:publish-diagnoses" '())]
    [hover (make-request 30 "textDocument/hover" '())]
    [pub2 (make-request '() "private:publish-diagnoses" '())])
  (request-queue-push queue pub1 processor workspace)
  (request-queue-push queue hover processor workspace)
  (request-queue-push queue pub2 processor workspace)

  (let ([th1 (request-queue-pop queue processor)])
    (th1))
  (let ([th2 (request-queue-pop queue processor)])
    (th2))

  (test-equal "queue empty after two pops" #t (request-queue-empty? queue))
  (test-equal "two tasks executed" 2 (length calls))
  (test-equal "first is publish-diagnoses" "private:publish-diagnoses" (car calls))
  (test-equal "second is hover" "textDocument/hover" (cadr calls)))
(test-end)

;; ------------------------------------------------------------------
;; 12. completed task is removed from tickal-task-list by complete callback
;; ------------------------------------------------------------------
(test-begin "request-queue-complete-cleanup")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [req (make-request 100 "textDocument/hover" '())]
    [cancel (make-request '() "$/cancelRequest" (list (cons 'id 100)))])
  (request-queue-push queue req processor workspace)
  ; Execute the request to completion.
  (let ([th (request-queue-pop queue processor)])
    (th))
  ; At this point the task should have been removed from tickal-task-list
  ; by the complete callback.  A subsequent cancel for the same id should
  ; find nothing and therefore not invoke the processor.
  (request-queue-push queue cancel processor workspace)

  (test-equal "queue empty after completion + cancel" #t (request-queue-empty? queue))
  (test-equal "only original request ran" 1 (length calls))
  (test-equal "original request method" "textDocument/hover" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 13. cancelled pending task is removed from tickal-task-list by job lambda
;; ------------------------------------------------------------------
(test-begin "request-queue-cancel-cleanup")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [req (make-request 200 "textDocument/completion" '())]
    [cancel1 (make-request '() "$/cancelRequest" (list (cons 'id 200)))]
    [cancel2 (make-request '() "$/cancelRequest" (list (cons 'id 200)))])
  (request-queue-push queue req processor workspace)
  ; First cancel sets stop? and invokes processor for the cancel itself.
  (request-queue-push queue cancel1 processor workspace)
  ; Pop the now-stopped task: job lambda sees stop?=#t and removes it.
  (let ([th (request-queue-pop queue processor)])
    (th))
  ; Second cancel should find nothing because the task was removed above.
  (request-queue-push queue cancel2 processor workspace)

  (test-equal "queue empty" #t (request-queue-empty? queue))
  ; Only cancel1 should have invoked the processor (once for itself).
  (test-equal "only one cancel processed" 1 (length calls))
  (test-equal "cancelRequest method" "$/cancelRequest" (car calls)))
(test-end)

;; ------------------------------------------------------------------
;; 14. cancelRequest for an already-finished task is harmless
;; ------------------------------------------------------------------
(test-begin "request-queue-cancel-after-finish")
(let* ([queue (make-request-queue)]
    [calls '()]
    [processor (lambda (r) (set! calls (append calls (list (request-method r)))))]
    [req-a (make-request 300 "textDocument/definition" '())]
    [cancel (make-request '() "$/cancelRequest" (list (cons 'id 300)))]
    [req-b (make-request 301 "textDocument/hover" '())])
  (request-queue-push queue req-a processor workspace)
  ; Finish req-a normally.
  (let ([th (request-queue-pop queue processor)])
    (th))
  ; Cancel the already-finished id: should be a no-op.
  (request-queue-push queue cancel processor workspace)
  ; Push another unrelated request to prove the queue is still healthy.
  (request-queue-push queue req-b processor workspace)
  (let ([th (request-queue-pop queue processor)])
    (th))

  (test-equal "queue empty" #t (request-queue-empty? queue))
  (test-equal "both real requests ran" 2 (length calls))
  (test-equal "req-a ran" "textDocument/definition" (car calls))
  (test-equal "req-b ran" "textDocument/hover" (cadr calls)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
