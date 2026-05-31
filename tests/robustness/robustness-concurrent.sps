#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; 多线程并发鲁棒性测试：快速交替发送 didChange / hover / completion，
;; 验证服务器不崩溃。
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver)
  (scheme-langserver protocol server))

;; -------------------------------------------------------------------
;; LSP 消息构造基础设施（与 robustness-lsp-replay.sps 保持一致）
;; -------------------------------------------------------------------
(define (make-lsp-message payload)
  (string-append
    "Content-Length: "
    (number->string (bytevector-length (string->utf8 payload)))
    "\r\n\r\n"
    payload))

(define (esc s)
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

(define (completion-msg uri line char)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"textDocument/completion\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\"},\"position\":{"
    "\"line\":" (number->string line) ",\"character\":" (number->string char) "}}}"))

(define (definition-msg uri line char)
  (string-append
    "{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"textDocument/definition\","
    "\"params\":{\"textDocument\":{\"uri\":\"" uri "\"},\"position\":{"
    "\"line\":" (number->string line) ",\"character\":" (number->string char) "}}}"))

(define test-uri "file:///tmp/test.scm")

;; -------------------------------------------------------------------
;; 多线程会话运行器
;; -------------------------------------------------------------------
(define (run-session-mt messages)
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
         [server (init-server input output log #t #f 'r6rs)])
    (server-shutdown? server)))

;; -------------------------------------------------------------------
;; 测试辅助
;; -------------------------------------------------------------------
(define (test-concurrent name messages)
  (test-assert
    (string-append "concurrent: " name)
    (guard (e [else #f])
      (run-session-mt messages))))

(test-begin "lsp-robustness-concurrent")

;; -------------------------------------------------------------------
;; 1. 基础多线程会话
;; -------------------------------------------------------------------
(test-concurrent "basic-mt-session"
  `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
    ,(did-open-msg test-uri "(define x 1)\n")
    ,(hover-msg test-uri 0 8)
    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))

;; -------------------------------------------------------------------
;; 2. 快速交替 didChange + hover
;; -------------------------------------------------------------------
(test-concurrent "rapid-didChange+hover"
  `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
    ,(did-open-msg test-uri "(define x 1)\n")
    ,(did-change-msg test-uri 2 "(define x 2)\n")
    ,(hover-msg test-uri 0 8)
    ,(did-change-msg test-uri 3 "(define y 3)\n")
    ,(hover-msg test-uri 0 8)
    ,(did-change-msg test-uri 4 "(define z 4)\n")
    ,(hover-msg test-uri 0 8)
    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))

;; -------------------------------------------------------------------
;; 3. 并发错字修复流
;; -------------------------------------------------------------------
(test-concurrent "typing-typos-stream"
  `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
    ,(did-open-msg test-uri "(define x 1)\n")
    ,(did-change-msg test-uri 2 "(define x ")          ;; 少右括号
    ,(hover-msg test-uri 0 8)
    ,(did-change-msg test-uri 3 "(define x 2)\n")      ;; 修正
    ,(completion-msg test-uri 0 8)
    ,(did-change-msg test-uri 4 "(define x \"hello")   ;; 字符串未闭合
    ,(hover-msg test-uri 0 8)
    ,(did-change-msg test-uri 5 "(define x \"hello\")\n") ;; 修正
    ,(definition-msg test-uri 0 8)
    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))

;; -------------------------------------------------------------------
;; 4. 宏编辑 + 并发请求
;; -------------------------------------------------------------------
(test-concurrent "macro-typing+concurrent-requests"
  `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
    ,(did-open-msg test-uri "(define-syntax m (syntax-rules () ((_ x) x)))\n(m 1)\n")
    ,(did-change-msg test-uri 2 "(define-syntax m (syntax-rules ") ;; 未闭合
    ,(hover-msg test-uri 0 15)
    ,(did-change-msg test-uri 3 "(define-syntax m (syntax-rules () ((_ x) (m x))))\n(m 1)\n") ;; 自引用
    ,(completion-msg test-uri 1 0)
    ,(hover-msg test-uri 1 0)
    ,(did-change-msg test-uri 4 "(define x 1)\n") ;; 切回普通代码
    ,(definition-msg test-uri 0 8)
    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))

;; -------------------------------------------------------------------
;; 5. 多文档并发操作
;; -------------------------------------------------------------------
(define uri-a "file:///tmp/a.scm")
(define uri-b "file:///tmp/b.scm")

(test-concurrent "multi-document-concurrent"
  `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
    ,(did-open-msg uri-a "(define foo 1)\n")
    ,(did-open-msg uri-b "(define bar 2)\n")
    ,(did-change-msg uri-a 2 "(define foo 10)\n")
    ,(hover-msg uri-b 0 8)
    ,(did-change-msg uri-b 2 "(define bar 20)\n")
    ,(hover-msg uri-a 0 8)
    ,(completion-msg uri-a 0 8)
    ,(definition-msg uri-b 0 8)
    "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
