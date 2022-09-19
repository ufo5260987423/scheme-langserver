#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver) (scheme-langserver util io) (ufo-thread-pool))

(let* ([thread-pool (init-thread-pool 1)]
        [input-port (current-input-port)]
        [output-port (current-output-port)]
        [initialization-json (string-append
    	"{\n" +
		"    \"id\": \"1\",\n" 
		"    \"method\": \"initialize\",\n" 
		"    \"params\": {\n" 
		"        \"textDocument\": {\n" 
		"            \"uri\": \"${file}\"\n" 
		"        },\n" 
		"        \"position\": {\n" 
		"            \"line\": ${line},\n" 
		"            \"character\": ${char}\n" 
		"        }\n" 
		"    },\n" 
		"    \"jsonrpc\": \"2.0\"\n" 
		"}")]
        [header (string-append 
        "GET /example.http HTTP/1.1\n"
        "Content-Length: "
        (number->string (string-length initialization-json))
        "\n\n")])

    (thread-pool-add-job thread-pool (lambda () (init-server input-port output-port)))

    (test-begin "init-stop test")
        (write-string input-port header)
        (write-string input-port initialization-json)
        (let ([result (read-port output-port)])
            (display result)
        )

        ; (test-equal a-list (read-json result))
    (test-end)

    (thread-pool-stop! thread-pool)
)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
