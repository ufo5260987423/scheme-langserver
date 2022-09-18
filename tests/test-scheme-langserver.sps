#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver)
    (scheme-langserver protocol message)
    (scheme-langserver util json)
    (scheme-langserver util association)
    )

(test-begin "initialize and shutdown")
    (let* ([input-port (current-input-port) ]
            [output-port (current-output-port)]
            [server-instance (make-server input-port output-port '() '() '() #f)]
            [initialize-bytevector (string->bytevector 
                    (generate-json (make-alist 'jsonrpc "2.0" 'id 0 'method "initialize" 'params 
                        (make-alist 'processId 0 'rootPath (current-directory) 
                            'rootUri (string-append "file://" (current-directory)) 'capabilities '())))
                    "utf-8")]
            [shutdown-bytevector (string->bytevector (generate-json (make-alist 'jsonrpc "2.0" 'id 1 'method "shutdown")) "utf-8")])
        ; (put-bytevector input-port initialize-bytevector)
        ; (flush-output-port input-port)
        ; (read-message server-instance)

        ; (put-bytevector input-port shutdown-bytevector)
        ; (flush-output-port input-port)
        ; (read-message server-instance)
        initialize-bytevector
        ; (test-equal 1 1)
    )
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
