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
  (scheme-langserver util io) )

(define (process result target)
  `(,@result 
    ,(string-append 
      "Content-Length: "
      (number->string (bytevector-length (string->utf8 target)))
      "\r\n\r\n" target)))

(test-begin "log-debug")
(let loop ([lines (read-lines "~/ready-for-analyse.log")]
    [result '()]
    [read? #f])
  (if (not (null? lines))
    (let ([current-line (car lines)])
      (cond 
        [read?  (loop (cdr lines) (process result current-line) #f)]
        [(equal? current-line "read-message")
          (loop (cddr lines) result #t)]
        [else (loop (cdr lines) result #f)]))
    (let* ( [input-port (open-bytevector-input-port (string->utf8 (apply string-append result)))]
        [log-port (open-file-output-port "~/scheme-langserver.log" (file-options replace) 'block (make-transcoder (utf-8-codec)))]
        [output-port (open-file-output-port "~/scheme-langserver.out" (file-options replace) 'none)]
        [server-instance (init-server input-port output-port log-port #f #t 'r6rs)])
      (test-equal #f (server-shutdown? server-instance)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
