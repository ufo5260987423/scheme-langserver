#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (rnrs (6))
  (srfi :64 testing)
  (scheme-langserver)
  (scheme-langserver util io)
  (scheme-langserver util json)
  (scheme-langserver util association)
  (scheme-langserver util path))

(define (ensure-trailing-slash path)
  (if (and (> (string-length path) 0)
           (char=? (string-ref path (- (string-length path) 1)) #\/))
    path
    (string-append path "/")))

(define (wrap json)
  (let* ([bv (string->utf8 json)]
         [len (bytevector-length bv)])
    (string-append
      "Content-Length: " (number->string len) "\r\n\r\n"
      json)))

(define content-length-prefix "Content-Length: ")

(define (read-content-length port)
  (let loop ([maybe-len #f])
    (let ([line (read-to-CRNL port)])
      (cond
        [(string=? line "") (if maybe-len maybe-len 0)]
        [(and (>= (string-length line) (string-length content-length-prefix))
              (string=? (substring line 0 (string-length content-length-prefix)) content-length-prefix))
          (loop (string->number (substring line (string-length content-length-prefix) (string-length line))))]
        [else (loop maybe-len)]))))

(define utf8-transcoder (make-transcoder (utf-8-codec)))

(define (read-one-message port)
  (let ([len (read-content-length port)])
    (if (zero? len)
      #f
      (let* ([body (get-bytevector-n port len)]
             [json (bytevector->string body utf8-transcoder)])
        (read-json json)))))

(define (read-all-messages port)
  (let loop ([acc '()])
    (let ([m (read-one-message port)])
      (if m
        (loop (cons m acc))
        (reverse acc)))))

(define (find-message-by-id messages id)
  (find
    (lambda (m)
      (let ([p (assq 'id m)])
        (and p (equal? (cdr p) id))))
    messages))

(define (location-start-line loc)
  (assq-ref (assq-ref (assq-ref loc 'range) 'start) 'line))

(define (location-start-character loc)
  (assq-ref (assq-ref (assq-ref loc 'range) 'start) 'character))

(test-begin "peek declaration test")

(let* ([root (ensure-trailing-slash (current-directory))]
       [fixture-path (string-append root "tests/resources/peek-fixture.scm.txt")]
       [fixture-content (read-string fixture-path)]
       [workspace-path "/tmp/scheme-langserver-test-peek-declaration"]
       [file-path (string-append workspace-path "/peek-fixture.scm")]
       [workspace-uri (path->uri workspace-path)]
       [file-uri (path->uri file-path)]

       [init-json
         (string-append
           "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{"
           "\"processId\":1,"
           "\"rootUri\":\"" workspace-uri "\","
           "\"capabilities\":{}"
           "}}")]
       ;; This is a JSON-RPC notification: it has no `id`, so servers must not respond.
       [cfg-json
         "{\"jsonrpc\":\"2.0\",\"method\":\"workspace/didChangeConfiguration\",\"params\":{}}"]

       ;; Position is right *after* the declaration identifier `fun`.
       ;; Many clients (including Emacs integrations) may send such positions.
       [def-json
         (string-append
           "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"textDocument/definition\",\"params\":{"
           "\"textDocument\":{\"uri\":\"" file-uri "\"},"
           "\"position\":{\"line\":0,\"character\":12}"
           "}}")]

       [refs-json
         (string-append
           "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"textDocument/references\",\"params\":{"
           "\"textDocument\":{\"uri\":\"" file-uri "\"},"
           "\"position\":{\"line\":0,\"character\":12},"
           "\"context\":{\"includeDeclaration\":false}"
           "}}")]

       [mkdir-ok (guard (c [else #f]) (mkdir workspace-path))]
       [write-ok
         (let ([p (open-file-output-port
                    file-path
                    (file-options replace)
                    'block
                    (make-transcoder (utf-8-codec)))])
           (put-string p fixture-content)
           (close-port p)
           #t)]

       [stream (string->utf8 (string-append (wrap init-json) (wrap cfg-json) (wrap def-json) (wrap refs-json)))]
       [input-port (open-bytevector-input-port stream)])

  (call-with-values
    open-bytevector-output-port
    (lambda (output-port get-output-bytevector)
      (init-server input-port output-port '() #f #f)
      (let* ([out-bv (get-output-bytevector)]
             [out-port (open-bytevector-input-port out-bv)]
             [messages (read-all-messages out-port)]
             [def-msg (find-message-by-id messages 10)]
             [refs-msg (find-message-by-id messages 11)]
             [id-false-msg (find (lambda (m) (let ([p (assq 'id m)]) (and p (eq? (cdr p) #f)))) messages)])

        (test-assert "definition response exists" def-msg)
        (test-assert "references response exists" refs-msg)
        (test-assert "no response for notifications (id=false)" (not id-false-msg))

        (when def-msg
          (let ([def-result (assq-ref def-msg 'result)])
            (test-assert "definition non-empty" (and (vector? def-result) (> (vector-length def-result) 0)))
            (when (and (vector? def-result) (> (vector-length def-result) 0))
              (let ([loc (vector-ref def-result 0)])
                (test-equal "definition in same file" file-uri (assq-ref loc 'uri))
                (test-equal "definition on line 0" 0 (location-start-line loc))))))

        (when refs-msg
          (let ([refs-result (assq-ref refs-msg 'result)])
            (test-assert "references non-empty" (and (vector? refs-result) (> (vector-length refs-result) 0)))
            (test-assert "references include usage on line 1"
              (and (vector? refs-result)
                   (let loop ([i 0])
                     (if (= i (vector-length refs-result))
                       #f
                       (let ([loc (vector-ref refs-result i)])
                         (if (and (string=? file-uri (assq-ref loc 'uri))
                                  (= 1 (location-start-line loc))
                                  (= 1 (location-start-character loc)))
                           #t
                           (loop (+ i 1))))))))))))))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
