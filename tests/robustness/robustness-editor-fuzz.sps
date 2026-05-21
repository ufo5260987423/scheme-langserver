#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; 基于真实 fixture 的编辑器随机变异 fuzzing。
;; 读取 workspace-fixtures 下的真实代码，随机插入/删除/替换字符，
;; 然后通过合法 LSP 消息发送，验证服务器不崩溃。
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver)
  (scheme-langserver protocol server))

;; -------------------------------------------------------------------
;; LSP 消息构造基础设施
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

;; -------------------------------------------------------------------
;; 安全会话运行器：捕获所有异常（包括非 condition），单轮 crash 不杀死整个测试进程
;; -------------------------------------------------------------------
(define (run-session-safe messages)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (e) (k #f))
        (lambda ()
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
            (server-shutdown? server)))))))

;; -------------------------------------------------------------------
;; Fixture 读取
;; -------------------------------------------------------------------
(define (read-fixture-text fixture-name file-name)
  (call-with-input-file
    (string-append (current-directory)
                   "/tests/resources/workspace-fixtures/"
                   fixture-name "/" file-name)
    (lambda (p) (get-string-all p))))

;; -------------------------------------------------------------------
;; 随机变异
;; -------------------------------------------------------------------
(define mutation-chars
  (string->list "([)]\"';#`,|@\\ \n\tx0+-"))

(define (random-char)
  (list-ref mutation-chars (random (length mutation-chars))))

(define (mutate-string s)
  (if (zero? (string-length s))
    (string (random-char))
    (let ([op (random 3)]
          [len (string-length s)]
          [pos (random (string-length s))])
      (case op
        [(0) ;; insert
         (string-append
           (substring s 0 pos)
           (string (random-char))
           (substring s pos len))]
        [(1) ;; delete
         (if (<= len 1)
           ""
           (string-append
             (substring s 0 pos)
             (substring s (+ pos 1) len)))]
        [(2) ;; replace
         (string-append
           (substring s 0 pos)
           (string (random-char))
           (substring s (+ pos 1) len))]
        [else s]))))

;; -------------------------------------------------------------------
;; 单轮 fuzz：变异 → didOpen → 随机请求 → shutdown
;; -------------------------------------------------------------------
(define (fuzz-round fixture-text uri)
  (let* ([mutated (mutate-string fixture-text)]
         [lines (max 1 (length (string-split mutated #\newline)))])
    (run-session-safe
      `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
        ,(did-open-msg uri mutated)
        ,(if (zero? (random 2))
           (hover-msg uri 0 1)
           (completion-msg uri 0 1))
        "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}"))))

;; -------------------------------------------------------------------
;; 对单个 fixture 跑 N 轮
;; -------------------------------------------------------------------
(define (test-fuzz-fixture name fixture-file rounds seed)
  (random-seed seed)
  (let ([text (read-fixture-text
                (car (string-split fixture-file #\/))
                (cadr (string-split fixture-file #\/)))])
    (let loop ([i 0] [crashes 0])
      (if (>= i rounds)
        (test-equal
          (string-append "fuzz-" name "-seed-" (number->string seed) "-crashes")
          0 crashes)
        (let ([ok? (fuzz-round text "file:///tmp/test.scm")])
          (loop (+ i 1) (if ok? crashes (+ crashes 1))))))))

;; -------------------------------------------------------------------
;; string-split 辅助（简单实现，按单个字符分割）
;; -------------------------------------------------------------------
(define (string-split s ch)
  (let loop ([i 0] [start 0] [acc '()])
    (if (>= i (string-length s))
      (reverse (cons (substring s start i) acc))
      (if (char=? ch (string-ref s i))
        (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))
        (loop (+ i 1) start acc)))))

;; -------------------------------------------------------------------
;; 测试开始
;; -------------------------------------------------------------------
(test-begin "lsp-robustness-editor-fuzz")

;; 5 个 fixture，每个跑 30 轮，seed 固定以保证可复现
(define rounds 30)

(test-fuzz-fixture "simple-lib" "simple-lib/main.scm.txt" rounds 42)
(test-fuzz-fixture "cascade-macro" "cascade-macro/lib.scm.txt" rounds 42)
(test-fuzz-fixture "let-syntax" "let-syntax-auto-resolve/consumer.scm.txt" rounds 42)
(test-fuzz-fixture "match" "match-auto-resolve/consumer.scm.txt" rounds 42)
(test-fuzz-fixture "record-type" "record-type/point.scm.txt" rounds 42)

;; 多线程 sanity check：用 simple-lib 跑 10 轮多线程变异
(test-begin "fuzz-mt-sanity")
(random-seed 42)
(let ([text (read-fixture-text "simple-lib" "main.scm.txt")])
  (let loop ([i 0] [crashes 0])
    (if (>= i 10)
      (test-equal "fuzz-simple-lib-mt-crashes" 0 crashes)
      (let ([mutated (mutate-string text)])
        (let ([ok? (call/cc
                     (lambda (k)
                       (with-exception-handler
                         (lambda (e) (k #f))
                         (lambda ()
                           (let* ([input (open-bytevector-input-port
                                           (string->utf8
                                             (apply string-append
                                               (map make-lsp-message
                                                 `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
                                                   ,(did-open-msg "file:///tmp/test.scm" mutated)
                                                   ,(hover-msg "file:///tmp/test.scm" 0 1)
                                                   "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}")))))]
                                  [output (open-file-output-port "/dev/null" (file-options no-fail) 'none)]
                                  [log (open-file-output-port "/dev/null" (file-options no-fail) 'block (make-transcoder (utf-8-codec)))]
                                  [server (init-server input output log #t #f 'r6rs)])
                             (server-shutdown? server))))))])
          (loop (+ i 1) (if ok? crashes (+ crashes 1))))))))
(test-end)

;; -------------------------------------------------------------------
;; 3. 连续编辑流模拟（增量错误注入）
;; -------------------------------------------------------------------
(test-begin "typing-stream")
(random-seed 123)
(let ([base-text (read-fixture-text "simple-lib" "main.scm.txt")])
  (let loop ([trial 0] [crashes 0])
    (if (>= trial 20)
      (test-equal "typing-stream-crashes" 0 crashes)
      (let* ([edits (let inner ([i 0] [text base-text] [acc '()])
                       (if (>= i 8)
                         (reverse acc)
                         (let ([next (mutate-string text)])
                           (inner (+ i 1) next (cons next acc)))))]
             [messages
               `(,"{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"processId\":123,\"rootUri\":\"file:///tmp\",\"capabilities\":{}}}"
                 ,(did-open-msg "file:///tmp/test.scm" base-text)
                 ,@(let inner2 ([i 0] [eds edits] [msgs '()])
                     (if (null? eds)
                       (reverse msgs)
                       (inner2
                         (+ i 1)
                         (cdr eds)
                         (cons
                           (did-change-msg "file:///tmp/test.scm" (+ i 2) (car eds))
                           (if (zero? (mod i 3))
                             (cons (if (zero? (random 2))
                                     (hover-msg "file:///tmp/test.scm" 0 1)
                                     (completion-msg "file:///tmp/test.scm" 0 1))
                                   msgs)
                             msgs)))))
                 "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"shutdown\"}")]
             [ok? (run-session-safe messages)])
        (loop (+ trial 1) (if ok? crashes (+ crashes 1)))))))
(test-end)

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
