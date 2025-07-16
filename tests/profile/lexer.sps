#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme) 
  (srfi :64 testing) 
  (scheme-langserver) 
  (scheme-langserver util io)
  (laesare reader)
  )

(test-begin "profile of chez's lexer")
(let* ( [path "./analysis/abstract-interpreter.sls"]
    [source (read-string path) ]
    [port (open-string-input-port source)]
    [source-file-descriptor (make-source-file-descriptor path (open-file-input-port path))])
  (time 
    (do 
      ((i 0 (+ i 1)))
      ((= i 1000))
      (set-port-position! port 0)
      (let loop ([position 0])
          (let-values ([(ann end-pos) (get-datum/annotations port source-file-descriptor position)]) 
            (if (= position (port-position port))
              '()
              (loop (port-position port))))))))
(test-end)

(test-begin "profile of laesare's lexer")
(let* ( [path "./analysis/abstract-interpreter.sls"]
    [source (read-string path) ]
    [port (open-string-input-port source)]
    [reader (make-reader port path)])
  (time 
    (do 
      ((i 0 (+ i 1)))
      ((= i 1000))
      (set-port-position! port 0)
      (read-annotated reader))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
