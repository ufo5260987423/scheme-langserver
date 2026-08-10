#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver protocol apis file-change-notification)
  (scheme-langserver protocol apis document-sync)
  (scheme-langserver util path)
  (scheme-langserver util association))

(test-begin "file watching: open files are protected from external change events")
  (let* ([root (current-directory)]
      [tmp-dir (string-append root "/tests/resources/tmp-watching")]
      [file-path (string-append tmp-dir "/watched.scm")]
      [uri (path->uri file-path)]
      [cleanup (lambda () (system (string-append "rm -rf " tmp-dir)))]
      [open-uris (make-hashtable string-hash equal?)]
      [original-text "(define original 1)\n"]
      [open-text "(define open-version 2)\n"]
      [changed-text "(define changed-on-disk 3)\n"])
    (dynamic-wind
      (lambda () (cleanup) (mkdir tmp-dir))
      (lambda ()
        (let ([workspace (init-workspace tmp-dir 'scheme 'r6rs #f #f)])
          ;; Seed the file after workspace init so we can exercise did-create.
          (let ([p (open-file-output-port file-path (file-options replace) 'block (make-transcoder (utf-8-codec)))])
            (put-string p original-text)
            (close-port p))
          ;; 1. create and open the file with different in-memory text
          (did-create workspace (make-alist 'files (vector (make-alist 'uri uri))))
          (did-open workspace
            (make-alist 'textDocument
              (make-alist 'uri uri 'languageId "scheme" 'version 1 'text open-text))
            open-uris)

          (let ([doc (file-node-document (walk-file (workspace-file-node workspace) file-path))])
            (test-equal "did-open synchronizes client text" open-text (document-text doc))

            ;; 2. simulate external change on disk while file is still open
            (let ([p (open-file-output-port file-path (file-options replace) 'block (make-transcoder (utf-8-codec)))])
              (put-string p changed-text)
              (close-port p))

            (did-change-watched-files workspace
              (make-alist 'changes
                (vector (make-alist 'uri uri 'type 2)))
              open-uris)

            (test-equal "Change event ignored while file is open" open-text (document-text doc))

            ;; 3. close the file; disk is now authoritative again
            (did-close workspace
              (make-alist 'textDocument (make-alist 'uri uri))
              open-uris)

            (did-change-watched-files workspace
              (make-alist 'changes
                (vector (make-alist 'uri uri 'type 2)))
              open-uris)

            (test-equal "Change event applied after file is closed" changed-text (document-text doc)))))
      cleanup))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
