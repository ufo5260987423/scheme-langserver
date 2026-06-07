#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (only (srfi :13 strings) string-prefix?)
    (scheme-langserver util io)
    (scheme-langserver util path)
    (scheme-langserver analysis tokenizer)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver analysis workspace))

(define (make-test-file content)
  (let ([path (string-append (current-directory) "/tmp-test-tokenizer-" (number->string (random 100000)) ".scm")])
    (with-output-to-file path (lambda () (display content)))
    path))

(test-begin "tokenizer-diagnose")

(test-equal "source-file->annotations without document does not add diagnose"
  '()
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define (foo) 1)")]
           [annotations (source-file->annotations tmp tmp)])
      (delete-file tmp)
      '())))

(test-equal "source-file->annotations with document adds diagnose for unmatched paren"
  1
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              -1])
    (let* ([tmp (make-test-file "(define (foo) 1)")]
           [d (make-document (path->uri tmp) "(define (foo" '())])
      (document-diagnoses-set! d '())
      (source-file->annotations "(define (foo" tmp (consume-sps-auxiliary "(define (foo") #t d)
      (let ([result (length (document-diagnoses d))])
        (delete-file tmp)
        result))))

(test-equal "diagnose message starts with Syntax error:"
  #t
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              #f])
    (let* ([tmp (make-test-file "(define (foo) 1)")]
           [d (make-document (path->uri tmp) "(define (foo" '())])
      (document-diagnoses-set! d '())
      (source-file->annotations "(define (foo" tmp (consume-sps-auxiliary "(define (foo") #t d)
      (let ([msg (cadddr (car (document-diagnoses d)))])
        (delete-file tmp)
        (string-prefix? "Syntax error:" msg)))))

(test-equal "init-document adds diagnose for syntax error"
  1
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              -1])
    (let* ([tmp (make-test-file "(define (foo")]
           [d (init-document tmp 'r6rs)])
      (let ([result (length (document-diagnoses d))])
        (delete-file tmp)
        result))))

(test-equal "update-file-node-with-tail clears old diagnose and adds new"
  1
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              -1])
    (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
           [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
           [root (workspace-file-node workspace)]
           [target (walk-file root (string-append fixture "/main.scm.txt"))])
      (update-file-node-with-tail workspace target "(define (foo")
      (let ([count1 (length (document-diagnoses (file-node-document target)))])
        (update-file-node-with-tail workspace target "(define bar)")
        (let ([count2 (length (document-diagnoses (file-node-document target)))])
          (test-equal "valid syntax has no tokenizer diagnose" 0 count2)
          count1)))))

(test-equal "private-init-references preserves syntax diagnose"
  #t
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              #f])
    (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
           [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
           [root (workspace-file-node workspace)]
           [target (walk-file root (string-append fixture "/main.scm.txt"))])
      (update-file-node-with-tail workspace target "(define (foo")
      (let ([d (file-node-document target)])
        (refresh-workspace-for workspace target)
        (let ([diagnoses (document-diagnoses d)])
          (test-equal "syntax diagnose preserved after refresh" 1 (length diagnoses))
          (string-prefix? "Syntax error:" (cadddr (car diagnoses))))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
