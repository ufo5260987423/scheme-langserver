#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (scheme-langserver protocol apis document-diagnostic)
    (scheme-langserver analysis workspace)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver util path)
    (scheme-langserver util association))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/script-only"))

(define (get-hello-document workspace)
  (let* ([root-file-node (workspace-file-node workspace)]
      [target-file-node (walk-file root-file-node
                          (string-append fixture "/hello.scm.txt"))])
    (file-node-document target-file-node)))

;; ------------------------------------------------------------------
;; 1. unpublish-diagnostics->list includes documents with empty diagnoses
;; ------------------------------------------------------------------
(test-begin "unpublish-diagnostics-empty-clear")
(let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [document (get-hello-document workspace)]
    [target-path (uri->path (document-uri document))])
  ; Simulate: document previously had a diagnostic that was fixed.
  ; After re-analysis document-diagnoses is '().
  (document-diagnoses-set! document '())
  (workspace-undiagnosed-paths-set! workspace (list target-path))
  
  ; Before fix: empty-diagnosis documents were filtered out, so the
  ; client never received a clear-notification.  After fix they must
  ; be present with an empty diagnostics vector.
  (let ([result (unpublish-diagnostics->list workspace)])
    (test-equal "result is a list" #t (list? result))
    (test-equal "includes the document even with empty diagnoses" 1 (length result))
    (test-equal "uri is correct" (document-uri document) (assq-ref (car result) 'uri))
    (test-equal "diagnostics vector is empty" 0 (vector-length (assq-ref (car result) 'diagnostics)))))
(test-end)

;; ------------------------------------------------------------------
;; 2. unpublish-diagnostics->list still includes documents with non-empty diagnoses
;; ------------------------------------------------------------------
(test-begin "unpublish-diagnostics-non-empty")
(let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [document (get-hello-document workspace)]
    [target-path (uri->path (document-uri document))])
  ; Add a diagnostic.
  (document-diagnoses-set! document '((0 10 2 "Test error")))
  (workspace-undiagnosed-paths-set! workspace (list target-path))
  
  (let ([result (unpublish-diagnostics->list workspace)])
    (test-equal "includes document with non-empty diagnoses" 1 (length result))
    (test-equal "uri is correct" (document-uri document) (assq-ref (car result) 'uri))
    (test-equal "diagnostics vector has one item" 1 (vector-length (assq-ref (car result) 'diagnostics)))))
(test-end)

;; ------------------------------------------------------------------
;; 3. unpublish-diagnostics->list clears undiagnosed-paths after processing
;; ------------------------------------------------------------------
(test-begin "unpublish-diagnostics-clears-paths")
(let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
    [document (get-hello-document workspace)]
    [target-path (uri->path (document-uri document))])
  (workspace-undiagnosed-paths-set! workspace (list target-path))
  (unpublish-diagnostics->list workspace)
  (test-equal "undiagnosed-paths cleared" '() (workspace-undiagnosed-paths workspace)))
(test-end)

;; ------------------------------------------------------------------
;; 4. unpublish-diagnostics->list survives stale paths (file deleted)
;; ------------------------------------------------------------------
(test-begin "unpublish-diagnostics-stale-path")
(let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)])
  ; Add a non-existent path.
  (workspace-undiagnosed-paths-set! workspace '("/nonexistent/path.scm"))
  
  ; walk-file returns '() for a missing file.  The old code crashed
  ; because it fed '() straight into file-node-document.
  (let ([result (unpublish-diagnostics->list workspace)])
    (test-equal "stale path is ignored" '() result)
    (test-equal "undiagnosed-paths still cleared" '() (workspace-undiagnosed-paths workspace))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
