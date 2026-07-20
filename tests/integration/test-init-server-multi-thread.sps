#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (chezscheme) (srfi :64 testing) (scheme-langserver) (scheme-langserver protocol server))

(define (make-lsp-request id method params)
  (let ([json (if params
                (string-append
                  "{\"id\":\"" id "\","
                  "\"method\":\"" method "\","
                  "\"params\":" params ","
                  "\"jsonrpc\":\"2.0\"}")
                (string-append
                  "{\"id\":\"" id "\","
                  "\"method\":\"" method "\","
                  "\"jsonrpc\":\"2.0\"}"))])
    (string-append
      "Content-Length: " (number->string (bytevector-length (string->utf8 json))) "\r\n\r\n"
      json)))

(define fixture-path
  (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib"))

(define init-params
  (string-append
    "{\"processId\":1,"
    "\"rootPath\":\"" fixture-path "\","
    "\"rootUri\":\"file://" fixture-path "\","
    "\"capabilities\":{}}"))

(test-begin "init-server multi-thread branch does not crash")
  (let* ([input-str (make-lsp-request "1" "initialize" init-params)]
         [input-port (open-bytevector-input-port (string->utf8 input-str))]
         [output-port (open-file-output-port "/tmp/scheme-langserver-mt-test.out"
                        (file-options replace) 'none)]
         [log-port (open-file-output-port "/tmp/scheme-langserver-mt-test.log"
                      (file-options replace) 'block (make-transcoder (utf-8-codec)))]
         [server-instance (init-server input-port output-port log-port #t #f)])
    (close-port output-port)
    (close-port log-port)
    ;; In multi-thread mode requests are processed asynchronously; the server
    ;; instance is enough to prove the threaded? check and thread-pool setup
    ;; succeeded.
    (test-assert (server? server-instance)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
