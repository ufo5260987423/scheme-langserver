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
  2
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              -1])
    (let* ([tmp (make-test-file "(define (foo) 1)")]
           [d (make-document (path->uri tmp) "(define (foo" '())])
      (document-diagnoses-set! d '())
      (source-file->annotations "(define (foo" tmp (consume-sps-auxiliary "(define (foo") #t d 'r6rs)
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
      (source-file->annotations "(define (foo" tmp (consume-sps-auxiliary "(define (foo") #t d 'r6rs)
      (let ([msg (cadddr (car (document-diagnoses d)))])
        (delete-file tmp)
        (string-prefix? "Syntax error:" msg)))))

(test-equal "init-document adds diagnose for syntax error"
  2
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              -1])
    (let* ([tmp (make-test-file "(define (foo")]
           [d (init-document tmp 'r6rs)])
      (let ([result (length (document-diagnoses d))])
        (delete-file tmp)
        result))))

(test-equal "update-file-node-with-tail clears old diagnose and adds new"
  2
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
          (test-equal "syntax diagnose preserved after refresh" 2 (length diagnoses))
          (string-prefix? "Syntax error:" (cadddr (car diagnoses))))))))

(test-equal "R7RS #u8(...) is parsed as #vu8(...) in r7rs mode"
  '(define x #vu8(1 2 3))
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define x #u8(1 2 3))")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'r7rs)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "R7RS #\\null is parsed as #\\nul in r7rs mode"
  '(define x #\nul)
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define x #\\null)")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'r7rs)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "R7RS #\\escape is parsed as #\\esc in r7rs mode"
  '(define x #\esc)
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define x #\\escape)")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'r7rs)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "R6RS mode tolerant-parses #u8(...) instead of bytevector"
  '(define x u8 (1 2 3))
  (guard (e [else 'exception])
    (let* ([tmp (make-test-file "(define x #u8(1 2 3))")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'r6rs)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "S7 #<fails:...> is parsed as symbol in goldfish mode"
  (list 'define 'x (string->symbol "#<fails:...>"))
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file (string-append "(define x #" "<fails:" "...>" ")"))]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'goldfish)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "S7 raw string #\"\"\"\" is parsed as empty string in goldfish mode"
  '(define x "")
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define x #\"\"\"\")")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'goldfish)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "S7 raw string #\"\"hello\"\" is parsed as string in goldfish mode"
  '(define x "")
  (guard (e [else 
              (pretty-print `(EXCEPTION ,(condition-message e) ,(condition-irritants e)))
              'exception])
    (let* ([tmp (make-test-file "(define x #\"\"hello\"\")")]
           [text (call-with-input-file tmp get-string-all)]
           [annotations (source-file->annotations text tmp (consume-sps-auxiliary text) #t #f 'goldfish)])
      (let ([result (annotation-stripped (car annotations))])
        (delete-file tmp)
        result))))

(test-equal "R6RS mode reports diagnose for S7 #<fails:...>"
  1
  (guard (e [else -1])
    (let* ([tmp (make-test-file (string-append "(define x #" "<fails:" "...>" ")"))]
           [d (make-document (path->uri tmp) (call-with-input-file tmp get-string-all) '())])
      (document-diagnoses-set! d '())
      (source-file->annotations (document-text d) tmp (consume-sps-auxiliary (document-text d)) #t d 'r6rs)
      (let ([result (length (document-diagnoses d))])
        (delete-file tmp)
        result))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
