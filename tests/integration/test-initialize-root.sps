#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

;; initialize: the LSP spec allows rootUri to be null. Resolve the workspace root
;; from rootUri, then the deprecated rootPath, then the first workspaceFolders entry.

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (srfi :13 strings)
  (scheme-langserver)
  (scheme-langserver protocol server)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node))

(define (frame json)
  (string-append
    "Content-Length: " (number->string (bytevector-length (string->utf8 json))) "\r\n\r\n"
    json))

(define fixture-path
  (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib"))

;; Returns (output . server-instance) for an initialize request with the given root fields.
(define (initialize-with root-fields)
  (let* ([out-path "/tmp/scheme-langserver-initialize-root.out"]
         [input-port
           (open-bytevector-input-port
             (string->utf8
               (frame (string-append
                 "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":1,"
                 root-fields
                 ",\"capabilities\":{}}}"))))]
         [output-port (open-file-output-port out-path (file-options no-fail) 'none)]
         [log-port (open-file-output-port "/tmp/scheme-langserver-initialize-root.log"
                     (file-options no-fail) 'block (make-transcoder (utf-8-codec)))]
         [server-instance (init-server input-port output-port log-port #f #f)])
    (close-port output-port)
    (close-port log-port)
    (cons
      (bytevector->string
        (call-with-port (open-file-input-port out-path) get-bytevector-all)
        (make-transcoder (utf-8-codec)))
      server-instance)))

(define (workspace-root server-instance)
  (let ([workspace (server-workspace server-instance)])
    (if (null? workspace) #f (file-node-path (workspace-file-node workspace)))))

(test-begin "initialize uses rootUri")
  (let ([r (initialize-with (string-append "\"rootUri\":\"file://" fixture-path "\",\"rootPath\":null,\"workspaceFolders\":null"))])
    (test-assert "succeeds" (string-contains (car r) "\"capabilities\""))
    (test-equal "workspace root" fixture-path (workspace-root (cdr r))))
(test-end)

(test-begin "initialize falls back to rootPath when rootUri is null")
  ;; This is what Helix sends (see #77).
  (let ([r (initialize-with (string-append "\"rootUri\":null,\"rootPath\":\"" fixture-path "\",\"workspaceFolders\":[]"))])
    (test-assert "succeeds" (string-contains (car r) "\"capabilities\""))
    (test-equal "workspace root" fixture-path (workspace-root (cdr r))))
(test-end)

(test-begin "initialize falls back to the first workspace folder")
  (let ([r (initialize-with
             (string-append
               "\"rootUri\":null,\"rootPath\":null,"
               "\"workspaceFolders\":[{\"uri\":\"file://" fixture-path "\",\"name\":\"simple-lib\"}]"))])
    (test-assert "succeeds" (string-contains (car r) "\"capabilities\""))
    (test-equal "workspace root" fixture-path (workspace-root (cdr r))))
(test-end)

(test-begin "initialize without any root fails cleanly")
  (let ([r (initialize-with "\"rootUri\":null,\"rootPath\":null,\"workspaceFolders\":null")])
    (test-assert "returns InvalidParams" (string-contains (car r) "\"code\":-32602"))
    (test-assert "explains why" (string-contains (car r) "no workspace root")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
