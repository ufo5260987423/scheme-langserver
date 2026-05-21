#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (chezscheme) (srfi :64 testing) (srfi :13 strings) (scheme-langserver) (scheme-langserver util io))

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

(define (make-lsp-notification method params)
  (let ([json (if params
                (string-append
                  "{\"method\":\"" method "\","
                  "\"params\":" params ","
                  "\"jsonrpc\":\"2.0\"}")
                (string-append
                  "{\"method\":\"" method "\","
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
    "\"capabilities\":{\"window\":{\"workDoneProgress\":true}}}"))

(test-begin "shutdown-exit-spec")
  ;; Test 1: shutdown returns null result and sets server-shutdown?
  (let* ([input-str (string-append
                      (make-lsp-request "1" "initialize" init-params)
                      (make-lsp-request "2" "shutdown" #f))]
         [input-port (open-bytevector-input-port (string->utf8 input-str))]
         [output-port (open-file-output-port "/tmp/scheme-langserver-test.out"
                        (file-options replace) 'none)]
         [log-port (open-file-output-port "/tmp/scheme-langserver-test.log"
                      (file-options replace) 'block (make-transcoder (utf-8-codec)))]
         [server-instance (init-server input-port output-port log-port #f #f)])
    (close-port output-port)
    (close-port log-port)
    (let ([output (bytevector->string
                  (call-with-port
                    (open-file-input-port "/tmp/scheme-langserver-test.out")
                    get-bytevector-all)
                  (make-transcoder (utf-8-codec)))])
      (test-assert (string-contains output "\"result\":null"))
      (test-assert (string-contains output "\"id\":\"2\""))))

  ;; Test 2: request after shutdown returns InvalidRequest
  (let* ([input-str (string-append
                      (make-lsp-request "1" "initialize" init-params)
                      (make-lsp-request "2" "shutdown" #f)
                      (make-lsp-request "3" "textDocument/hover"
                        "{\"textDocument\":{\"uri\":\"file:///x\"},\"position\":{\"line\":0,\"character\":0}}"))]
         [input-port (open-bytevector-input-port (string->utf8 input-str))]
         [output-port (open-file-output-port "/tmp/scheme-langserver-test2.out"
                        (file-options replace) 'none)]
         [log-port (open-file-output-port "/tmp/scheme-langserver-test2.log"
                      (file-options replace) 'block (make-transcoder (utf-8-codec)))]
         [server-instance (init-server input-port output-port log-port #f #f)])
    (close-port output-port)
    (close-port log-port)
    (let ([output (bytevector->string
                    (call-with-port
                      (open-file-input-port "/tmp/scheme-langserver-test2.out")
                      get-bytevector-all)
                    (make-transcoder (utf-8-codec)))])
      (test-assert (string-contains output "InvalidRequest"))
      (test-assert (string-contains output "\"id\":\"3\""))))

  ;; Test 3: initialize after shutdown is rejected
  (let* ([input-str (string-append
                      (make-lsp-request "1" "initialize" init-params)
                      (make-lsp-request "2" "shutdown" #f)
                      (make-lsp-request "3" "initialize" init-params))]
         [input-port (open-bytevector-input-port (string->utf8 input-str))]
         [output-port (open-file-output-port "/tmp/scheme-langserver-test3.out"
                        (file-options replace) 'none)]
         [log-port (open-file-output-port "/tmp/scheme-langserver-test3.log"
                      (file-options replace) 'block (make-transcoder (utf-8-codec)))]
         [server-instance (init-server input-port output-port log-port #f #f)])
    (close-port output-port)
    (close-port log-port)
    (let ([output (bytevector->string
                    (call-with-port
                      (open-file-input-port "/tmp/scheme-langserver-test3.out")
                      get-bytevector-all)
                    (make-transcoder (utf-8-codec)))])
      (test-assert (string-contains output "InvalidRequest"))
      (test-assert (string-contains output "\"id\":\"3\""))))

  ;; Test 4: exit without shutdown terminates with code 1
  (let ([exit-script
         (string-append
           "(import (rnrs (6)) (scheme-langserver) (scheme-langserver util io))"
           "(define exit-json \"{\\\"method\\\":\\\"exit\\\",\\\"jsonrpc\\\":\\\"2.0\\\"}\")"
           "(define exit-header (string-append \"Content-Length: \" (number->string (bytevector-length (string->utf8 exit-json))) \"\\r\\n\\r\\n\"))"
           "(let* ([input-port (open-bytevector-input-port (string->utf8 (string-append exit-header exit-json)))]"
           "       [output-port (open-file-output-port \"/tmp/exit-only.out\" (file-options replace) 'none)]"
           "       [log-port (open-file-output-port \"/tmp/exit-only.log\" (file-options replace) 'block (make-transcoder (utf-8-codec)))])"
           "  (init-server input-port output-port log-port #f #f))")])
    (call-with-port
      (open-file-output-port "/tmp/exit-only.sps" (file-options replace) 'block (make-transcoder (utf-8-codec)))
      (lambda (p) (display exit-script p) (newline p)))
    (test-equal 1 (system "source .akku/bin/activate && scheme --script /tmp/exit-only.sps >/dev/null 2>&1")))

  ;; Test 5: exit after shutdown terminates with code 0
  (let ([exit-script
         (string-append
           "(import (rnrs (6)) (scheme-langserver) (scheme-langserver util io))"
           "(define shutdown-json \"{\\\"id\\\":\\\"1\\\",\\\"method\\\":\\\"shutdown\\\",\\\"jsonrpc\\\":\\\"2.0\\\"}\")"
           "(define shutdown-header (string-append \"Content-Length: \" (number->string (bytevector-length (string->utf8 shutdown-json))) \"\\r\\n\\r\\n\"))"
           "(define exit-json \"{\\\"method\\\":\\\"exit\\\",\\\"jsonrpc\\\":\\\"2.0\\\"}\")"
           "(define exit-header (string-append \"Content-Length: \" (number->string (bytevector-length (string->utf8 exit-json))) \"\\r\\n\\r\\n\"))"
           "(define input-str (string-append shutdown-header shutdown-json exit-header exit-json))"
           "(let* ([input-port (open-bytevector-input-port (string->utf8 input-str))]"
           "       [output-port (open-file-output-port \"/tmp/shutdown-exit.out\" (file-options replace) 'none)]"
           "       [log-port (open-file-output-port \"/tmp/shutdown-exit.log\" (file-options replace) 'block (make-transcoder (utf-8-codec)))])"
           "  (init-server input-port output-port log-port #f #f))")])
    (call-with-port
      (open-file-output-port "/tmp/shutdown-exit.sps" (file-options replace) 'block (make-transcoder (utf-8-codec)))
      (lambda (p) (display exit-script p) (newline p)))
    (test-equal 0 (system "source .akku/bin/activate && scheme --script /tmp/shutdown-exit.sps >/dev/null 2>&1")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
