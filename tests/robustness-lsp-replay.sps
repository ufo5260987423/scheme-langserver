#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Robustness test suite for scheme-langserver — valid LSP messages carrying
;; edge-case Scheme code.  We verify that hover/definition/completion do not
;; crash after didOpen/didChange with malicious or pathological content.
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver)
  (scheme-langserver protocol server))

(define (make-lsp-message payload)
  (string-append
    "Content-Length: "
    (number->string (bytevector-length (string->utf8 payload)))
    "\r\n\r\n"
    payload))

(define (run-session messages)
  (let* ([input (open-bytevector-input-port
                  (string->utf8
                    (apply string-append
                      (map make-lsp-message messages))))]
         [output (open-file-output-port
                   "/dev/null"
                   (file-options no-fail)
                   'none)]
         [log (open-file-output-port
                "/dev/null"
                (file-options no-fail)
                'block
                (make-transcoder (utf-8-codec)))]
         [server (init-server input output log #f #f 'r6rs)])
    (server-shutdown? server)))

(define (esc s)
  ;; Escape a Scheme string for JSON
  (let loop ([i 0] [acc '()])
    (if (>= i (string-length s))
      (list->string (reverse acc))
      (let ([ch (string-ref s i)])
        (cond
          [(char=? ch #\\) (loop (+ i 1) (cons #\\ (cons #\\ acc)))]
          [(char=? ch #\") (loop (+ i 1) (cons #\" (cons #\\ acc)))]
          [(char=? ch #\newline) (loop (+ i 1) (cons #\n (cons #\\ acc)))]
          [(char=? ch #\return) (loop (+ i 1) (cons #\r (cons #\\ acc)))]
          [(char=? ch #\tab) (loop (+ i 1) (cons #\t (cons #\\ acc)))]
          [else (loop (+ i 1) (cons ch acc))])))))

(define (did-open-msg uri text)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\","
    "\"params\":{\"textDocument\":{"
    "\"uri\":\"" uri "\","
    "\"languageId\":\"scheme\","
    "\"version\":1,"
    "\"text\":\"" (esc text) "\"}}}"))

(define (did-change-msg uri version new-text)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didChange\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\",\"version\":"
    (number->string version)
    "},\"contentChanges\":[{\"text\":\"" (esc new-text) "\"}]}}"))

(define (hover-msg uri line char)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"textDocument/hover\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\"},\"position\":{"
    "\"line\":" (number->string line) ",\"character\":" (number->string char) "}}}"))

(define (definition-msg uri line char)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"textDocument/definition\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\"},\"position\":{"
    "\"line\":" (number->string line) ",\"character\":" (number->string char) "}}}"))

(define (completion-msg uri line char)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"textDocument/completion\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\"},\"position\":{"
    "\"line\":" (number->string line) ",\"character\":" (number->string char) "}}}"))

(define test-uri "file:///tmp/test.scm")

(define (test-content name text . extra-msgs)
  (test-assert
    (string-append "robustness: " name)
    (guard (e [else #f])
      (run-session
        `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
          ,(did-open-msg test-uri text)
          ,@extra-msgs
          "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}")))))

(define (test-content+change name old-text new-text . extra)
  (test-assert
    (string-append "robustness: didChange-" name)
    (guard (e [else #f])
      (run-session
        `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
          ,(did-open-msg test-uri old-text)
          ,(did-change-msg test-uri 2 new-text)
          ,@extra
          "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}")))))

(test-begin "lsp-robustness-content")

;; -------------------------------------------------------------------
;; 1.  Basic legal content
;; -------------------------------------------------------------------
(test-content "simple-define" "(define x 1)\n" (hover-msg test-uri 0 8))
(test-content "empty-file" "")
(test-content "only-whitespace" "   \n\n   \n")
(test-content "only-comments" ";; just a comment\n")

;; -------------------------------------------------------------------
;; 2.  Incomplete syntax
;; -------------------------------------------------------------------
(test-content "unclosed-paren" "(define x" (hover-msg test-uri 0 5))
(test-content "unclosed-string" "(define x \"hello" (hover-msg test-uri 0 5))
(test-content "unclosed-vector" "#(" (hover-msg test-uri 0 1))
(test-content "unclosed-quote" "'(" (hover-msg test-uri 0 1))
(test-content "bare-open-paren" "(" (hover-msg test-uri 0 0))
(test-content "bare-close-paren" ")" (hover-msg test-uri 0 0))

;; -------------------------------------------------------------------
;; 3.  Macro & syntax edges
;; -------------------------------------------------------------------
(test-content "empty-define-syntax"
  "(define-syntax foo)\n"
  (hover-msg test-uri 0 15))

(test-content "syntax-rules-no-clauses"
  "(define-syntax foo (syntax-rules ()))\n"
  (hover-msg test-uri 0 15))

(test-content "syntax-rules-missing-paren"
  "(define-syntax foo (syntax-rules \n"
  (hover-msg test-uri 0 15))

(test-content "let-syntax-empty"
  "(let-syntax () x)\n"
  (hover-msg test-uri 0 13))

;; -------------------------------------------------------------------
;; 4.  Deep nesting & huge structures
;; -------------------------------------------------------------------
(test-content "deeply-nested-lets"
  (let loop ([n 0] [acc "x"])
    (if (>= n 50) acc
      (loop (+ n 1) (string-append "(let ((x " acc ")) x)"))))
  (hover-msg test-uri 0 10))

(test-content "long-line"
  (make-string 5000 #\a)
  (hover-msg test-uri 0 10))

;; -------------------------------------------------------------------
;; 5.  Self-referencing / cyclic
;; -------------------------------------------------------------------
(test-content "self-referencing-macro"
  "(define-syntax m (syntax-rules () ((_ x) (m x))))\n(m 1)\n"
  (hover-msg test-uri 1 0))

;; -------------------------------------------------------------------
;; 6.  Special characters & escapes
;; -------------------------------------------------------------------
(test-content "string-with-escapes"
  "(define x \"\\n\\t\\\\\\\"\\x00\")\n"
  (hover-msg test-uri 0 8))

(test-content "identifier-with-special-chars"
  "(define |weird id!@#| 1)\n"
  (hover-msg test-uri 0 8))

;; -------------------------------------------------------------------
;; 7.  Duplicate definitions & conflicts
;; -------------------------------------------------------------------
(test-content "duplicate-defines"
  "(define x 1)\n(define x 2)\n(define x 3)\n"
  (hover-msg test-uri 2 8))

(test-content "define-after-use"
  "x\n(define x 1)\n"
  (hover-msg test-uri 0 0))

;; -------------------------------------------------------------------
;; 8.  Nonexistent import / library
;; -------------------------------------------------------------------
(test-content "import-nonexistent"
  "(import (nonexistent-library))\n"
  (hover-msg test-uri 0 8))

;; -------------------------------------------------------------------
;; 9.  didChange edges
;; -------------------------------------------------------------------
(test-content+change "to-empty" "(define x 1)\n" "")
(test-content+change "to-unclosed" "(define x 1)\n" "(define x")
(test-content+change "to-macro" "(define x 1)\n" "(define-syntax m (syntax-rules ()))\n")
(test-content+change "add-import" "(define x 1)\n" "(import (nonexistent))\n(define x 1)\n")
(test-content+change "rapid-flip" "(define x 1)\n" "(define y 2)\n" (hover-msg test-uri 0 8))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
