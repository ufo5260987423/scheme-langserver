#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver) (scheme-langserver util io) (ufo-thread-pool))

(test-begin "init test")
(let* ([thread-pool (init-thread-pool 1)]
        [initialization-json (string-append
    	"{\n" 
		"    \"id\": \"1\",\n" 
		"    \"method\": \"initialize\",\n" 
		; "    \"params\": {\n" 
		; "        \"textDocument\": {\n" 
		; "            \"uri\": \"${file}\"\n" 
		; "        },\n" 
		; "        \"position\": {\n" 
		; "            \"line\": ${line},\n" 
		; "            \"character\": ${char}\n" 
		; "        }\n" 
		; "    },\n" 
		"    \"jsonrpc\": \"2.0\"\n" 
		"}")]
        [header (string-append 
        "GET /example.http HTTP/1.1\r\n"
        "Content-Length: "
        (number->string (bytevector-length (string->utf8 initialization-json)))
        "\r\n\r\n")]

        [input-port (open-bytevector-input-port (string->utf8 (string-append header initialization-json)))]
        [log-port (open-file-output-port "~/scheme-langserver.log" (file-options append))]
        [output-port (standard-output-port)])

    ; (thread-pool-add-job thread-pool (lambda () 
    (init-server input-port output-port log-port)
    ; ))
    ; (call-with-port output-port
    ;     (lambda (p)
    ;         (get-u8 p)))

    (thread-pool-stop! thread-pool))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
