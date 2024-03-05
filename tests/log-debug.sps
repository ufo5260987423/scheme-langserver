#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs
;;to read log and reproduce similar action for debug
(import (rnrs (6)) 
    (srfi :64 testing) 
    (scheme-langserver) 
    (scheme-langserver util io) )

(test-begin "log-debug")
(let loop ([lines (read-lines "~/ready-for-analyse.log")]
        [result '()]
        [batch '()]
        [read? #f])
    (if (not (null? lines))
        (let ([current-line (car lines)])
            (cond 
                [(and read? (not (equal? current-line "send-message")))
                    (loop (cdr lines) result (append batch `(,current-line)) #t)]
                [(and read? (equal? current-line "send-message"))
                    (let ([s
                            (fold-left 
                                (lambda (h t)
                                    (string-append h "\n" t ))
                                (car batch)
                                (cdr batch))])
                    (loop 
                        (cdr lines) 
                        `(,@result ,(string-append 
        "Content-Length: "
        (number->string (bytevector-length (string->utf8 s)))
        "\r\n\r\n" s))
                        '() 
                        #f))]
                [(and (not read?) (equal? current-line "read-message")) (loop (cddr lines) result '() #t)]
                [else (loop (cdr lines) result '() #f)]))
        (let* ([input-port (open-bytevector-input-port (string->utf8 (apply string-append result)))]
                [log-port (open-file-output-port "~/scheme-langserver.log" (file-options replace) 'block (make-transcoder (utf-8-codec)))]
                [output-port (open-file-output-port "~/scheme-langserver.out" (file-options replace) 'none)]
                [server-instance (init-server input-port output-port log-port #f #f #f)])
            (test-equal #f (server-shutdown? server-instance)))
        ))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
