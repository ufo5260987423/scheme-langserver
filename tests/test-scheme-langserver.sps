#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver) (scheme-langserver util io) (ufo-thread-pool))

(let* ([thread-pool (init-thread-pool 1)]
        [utf8-transcoder (make-transcoder (utf-8-codec))]
        ; [input-port (standard-input-port)]
        ; [output-port (standard-output-port)]
        [input-port (standard-input-port)]
        [output-port (standard-output-port)]
        [initialization-json (string->bytevector (string-append
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
		"}") utf8-transcoder)]
        [header (string->bytevector (string-append 
        "GET /example.http HTTP/1.1\r\n"
        "Content-Length: "
        (number->string (bytevector-length initialization-json))
        "\r\n\r\n") utf8-transcoder)])

    (thread-pool-add-job thread-pool (lambda () (init-server input-port output-port)))

    (test-begin "init-stop test")
        (put-bytevector input-port header)
        (put-bytevector input-port initialization-json)
        ; (write-string header input-port)
        ; (write-string initialization-json input-port)
        ; (let ([result (read-line output-port)])
        ;     (display result)
        ; )

        ; (test-equal a-list (read-json result))
    (test-end)

    (thread-pool-stop! thread-pool)
)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
