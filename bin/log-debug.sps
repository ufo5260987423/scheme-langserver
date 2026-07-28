#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs
;;to read log and reproduce similar action for debug
(import 
  (chezscheme)
  (srfi :64 testing) 
  (scheme-langserver) 
  (scheme-langserver protocol server) 
  (scheme-langserver util io) )

(define (process result target)
  `(,@result 
    ,(string-append 
      "Content-Length: "
      (number->string (bytevector-length (string->utf8 target)))
      "\r\n\r\n" target)))

(define (join-lines lines)
  (if (null? lines)
    ""
    (let loop ([lines (reverse lines)] [result ""])
      (if (null? (cdr lines))
        (string-append result (car lines))
        (loop (cdr lines) (string-append result (car lines) "\n"))))))

(test-begin "log-debug")
  (let loop ([lines (read-lines "~/ready-for-analyse.log")]
    [result '()]
    [state 'idle]
    [body-lines '()])
  (if (not (null? lines))
    (let ([current-line (car lines)])
      (case state
        [(idle)
          (cond
            [(equal? current-line "read-message") (loop (cdr lines) result 'read-timestamp '())]
            [(equal? current-line "send-message") (loop (cdr lines) result 'skip-timestamp '())]
            [else (loop (cdr lines) result 'idle '())])]
        [(read-timestamp)
          (loop (cdr lines) result 'read-body '())]
        [(read-body)
          (cond
            [(or (equal? current-line "read-message") (equal? current-line "send-message"))
              (let ([new-result (if (null? body-lines)
                                  result
                                  (process result (join-lines body-lines)))])
                (cond
                  [(equal? current-line "read-message") (loop (cdr lines) new-result 'read-timestamp '())]
                  [else (loop (cdr lines) new-result 'skip-timestamp '())]))]
            [else (loop (cdr lines) result 'read-body (cons current-line body-lines))])]
        [(skip-timestamp)
          (loop (cdr lines) result 'skip-body '())]
        [(skip-body)
          (cond
            [(or (equal? current-line "read-message") (equal? current-line "send-message"))
              (cond
                [(equal? current-line "read-message") (loop (cdr lines) result 'read-timestamp '())]
                [else (loop (cdr lines) result 'skip-timestamp '())])]
            [else (loop (cdr lines) result 'skip-body '())])]))
    (let* ([final-result (if (null? body-lines)
                            result
                            (process result (join-lines body-lines)))]
        [input-port (open-bytevector-input-port (string->utf8 (apply string-append final-result)))]
        [log-port (open-file-output-port "~/scheme-langserver.log" (file-options replace) 'block (make-transcoder (utf-8-codec)))]
        [output-port (open-file-output-port "~/scheme-langserver.out" (file-options replace) 'none)]
        [server-instance (init-server input-port output-port log-port #f #t 'r6rs)])
      (test-equal #t (server-shutdown? server-instance)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
