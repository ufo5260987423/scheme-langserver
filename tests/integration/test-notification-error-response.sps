#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

;; JSON-RPC 2.0: the server MUST NOT reply to a notification, even when
;; handling it fails. Requests that fail must still get an error response.

(import (rnrs (6)) (chezscheme) (srfi :64 testing) (srfi :13 strings) (scheme-langserver))

(define (frame json)
  (string-append
    "Content-Length: " (number->string (bytevector-length (string->utf8 json))) "\r\n\r\n"
    json))

(define fixture-path
  (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib"))

(define (run-server input-str out-path log-path)
  (let ([input-port (open-bytevector-input-port (string->utf8 input-str))]
        [output-port (open-file-output-port out-path (file-options no-fail) 'none)]
        [log-port (open-file-output-port log-path (file-options no-fail) 'block (make-transcoder (utf-8-codec)))])
    (init-server input-port output-port log-port #f #f)
    (close-port output-port)
    (close-port log-port)
    (bytevector->string
      (call-with-port (open-file-input-port out-path) get-bytevector-all)
      (make-transcoder (utf-8-codec)))))

(test-begin "no error response to a failing notification")
  (let ([output
          (run-server
            (string-append
              (frame (string-append
                "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                "{\"processId\":1,\"rootUri\":\"file://" fixture-path "\",\"capabilities\":{}}}"))
              (frame "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}")
              ;; Malformed notification: handling it throws.
              (frame "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didChange\",\"params\":{}}")
              ;; Malformed request: handling it throws, but it has an id.
              (frame "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/hover\",\"params\":{}}"))
            "/tmp/scheme-langserver-notification-error.out"
            "/tmp/scheme-langserver-notification-error.log")])
    (test-assert "initialize succeeded" (string-contains output "\"capabilities\""))
    (test-assert "no response with id false" (not (string-contains output "\"id\":false")))
    (test-assert "no error mentioning the notification" (not (string-contains output "textDocument\\/didChange")))
    (test-assert "failing request still gets an error response" (string-contains output "\"id\":3,\"error\"")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
