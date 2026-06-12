#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver protocol request)
    (scheme-langserver protocol server))

(test-begin "request-robustness")

;; Helper: build a mock server-instance with the given input string
(define (make-mock-server input-string)
  (let ([input-port (open-bytevector-input-port (string->utf8 input-string))]
        [output-port (open-output-string)]
        [log-port (open-output-string)])
    (make-server 
      input-port 
      output-port 
      log-port 
      #f   ; thread-pool
      #f   ; request-queue
      '()  ; workspace
      #f   ; type-inference?
      'r6rs ; top-environment
      #f   ; cache-path
      )))

;; 1. Normal JSON message
(let* ([input "Content-Length: 52\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"shutdown\"}"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-assert "normal JSON returns request" (request? result))
  (test-equal "normal JSON method" "shutdown" (request-method result)))

;; 2. Malformed JSON (unclosed object)
(let* ([input (string-append "Content-Length: 10\r\n\r\n" "{bad")]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "malformed JSON returns invalid" 'invalid result))

;; 3. Negative Content-Length
(let* ([input "Content-Length: -1\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"shutdown\"}"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "negative content-length treated as empty" #f result))

;; 4. Non-numeric Content-Length
(let* ([input "Content-Length: abc\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "non-numeric content-length treated as empty" #f result))

;; 5. Oversized Content-Length (> 10MB)
(let* ([input "Content-Length: 999999999\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"shutdown\"}"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "oversized content-length treated as empty" #f result))

;; 6. EOF before content
(let* ([input "Content-Length: 10\r\n\r\n"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "EOF before content returns false" #f result))

;; 7. Missing Content-Length header
(let* ([input "\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"shutdown\"}"]
       [server (make-mock-server input)]
       [result (read-message server)])
  (test-equal "missing content-length treated as empty" #f result))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
