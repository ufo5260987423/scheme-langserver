#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (chezscheme)  (scheme-langserver) (scheme-langserver util io) (ufo-thread-pool))

(time
    (let* ( [shutdown-json (read-string "./tests/resources/shutdown.json") ]
            [shutdown-header (string-append 
            ; "GET /example.http HTTP/1.1\r\n"
            "Content-Length: "
            (number->string (bytevector-length (string->utf8 shutdown-json)))
            "\r\n\r\n")]

            [initialization-json (string-append
            "{\n" 
            "    \"id\": \"1\",\n" 
            "    \"method\": \"initialize\",\n" 
            "    \"params\": {\n" 
            "        \"processId\": 1,\n"
            "        \"rootPath\": \"" (current-directory) "\",\n"
            "        \"rootUri\": \"file://" (current-directory) "\",\n"
            "        \"capabilities\": {\"window\":{\"workDoneProgress\":true}}\n"
            "    },\n" 
            "    \"jsonrpc\": \"2.0\"\n" 
            "}")]
            [init-header (string-append 
            ; "GET /example.http HTTP/1.1\r\n"
            "Content-Length: "
            (number->string (bytevector-length (string->utf8 initialization-json)))
            "\r\n\r\n")]

            [input-port (open-bytevector-input-port (string->utf8 (string-append init-header initialization-json shutdown-header shutdown-json)))]
            [log-port (open-file-output-port "~/scheme-langserver.log" (file-options replace) 'block (make-transcoder (utf-8-codec)))]
            ; [output-port (standard-output-port)]
            [output-port (open-file-output-port "~/scheme-langserver.out" (file-options replace) 'none)])
        (init-server input-port output-port log-port #f #f)))