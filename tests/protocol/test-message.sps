#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver protocol message)
    (scheme-langserver util io)
    (scheme-langserver util json)
    (scheme-langserver util association))

(test-begin "read and write message")
    (let* ([input-port (current-input-port) ]
            [output-port (current-output-port)]
            [server-instance (make-server input-port output-port '() '() '() #f)]
            [initialize-json
                (generate-json 
                    (make-alist 'jsonrpc "2.0" 'id 0 'method "initialize" 
                        'params (make-alist 'processId 0 'rootPath (current-directory) 
                            'rootUri (string-append "file://" (current-directory)) 'capabilities '())))])
        (display "go")
        (newline)
        ; (write-string initialize-json input-port)
        ; (flush-output-port input-port)
        (display "o")
        (newline)
        ; (display (read-message server-instance))
        ; (test-equal "2.0" (assoc-ref 'jsonrpc (read-message server-instance)))
    )
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
