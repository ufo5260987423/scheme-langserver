#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver protocol apis file-change-notification)
  (scheme-langserver protocol apis document-sync)
  (scheme-langserver protocol apis completion)
  (scheme-langserver util path)
  (scheme-langserver util association))

(test-begin "file lifecycle: create -> open -> change -> complete -> close -> delete")
  (let* ([root (current-directory)]
      [tmp-dir (string-append root "/tests/resources/tmp-lifecycle")]
      [file-path (string-append tmp-dir "/lifecycle.scm")]
      [uri (path->uri file-path)]
      [cleanup
        (lambda ()
          (system (string-append "rm -rf " tmp-dir)))]
      [_ (begin (cleanup) 'ok)]
      [workspace (init-workspace root 'akku 'r6rs #f #f)]
      [initial-text "(define lifecycle-answer 42)\n(lifecycle-ans"])
    (dynamic-wind
      (lambda () (cleanup) (mkdir tmp-dir))
      (lambda ()
        ;; 1. write initial text to disk
        (let ([p (open-file-output-port file-path (file-options replace) 'block (make-transcoder (utf-8-codec)))])
          (put-string p initial-text)
          (close-port p))

        ;; 2. did-create: workspace discovers the new file
        (did-create workspace (make-alist 'files (vector (make-alist 'uri uri))))
        (test-assert "file visible after did-create"
          (not (null? (walk-file (workspace-file-node workspace) file-path))))

        ;; 3. did-open: load document
        (did-open workspace (make-alist 'textDocument
                         (make-alist 'uri uri 'languageId "scheme" 'version 1 'text initial-text)))
        (let ([doc (file-node-document (walk-file (workspace-file-node workspace) file-path))])
          (test-equal "text loaded after did-open" initial-text (document-text doc))

          ;; 4. completion at line 1 character 13 (inside "lifecycle-ans")
          (let ([result (completion workspace
                          (make-alist 'textDocument (make-alist 'uri uri)
                              'position (make-alist 'line 1 'character 13)))])
            (test-assert "completion returns vector" (vector? result))
            (test-assert "completion includes 'lifecycle-answer'"
              (find (lambda (item)
                      (and (list? item) (equal? "lifecycle-answer" (assq-ref item 'label))))
                (vector->list result))))

          ;; 5. did-change: append "wer)" to complete the identifier
          (did-change workspace (make-alist 'textDocument (make-alist 'uri uri 'version 2)
                              'contentChanges (vector
                                (make-alist 'range
                                  (make-alist 'start (make-alist 'line 1 'character 14)
                                    'end (make-alist 'line 1 'character 14))
                                  'text "wer)"))))
          (test-equal "text updated after did-change"
            "(define lifecycle-answer 42)\n(lifecycle-answer)"
            (document-text (file-node-document (walk-file (workspace-file-node workspace) file-path))))

          ;; 6. did-close
          (did-close workspace (make-alist 'textDocument (make-alist 'uri uri))))

        ;; 7. delete from disk
        (delete-file file-path)

        ;; 8. did-delete: remove from workspace
        (did-delete workspace (make-alist 'files (vector (make-alist 'uri uri))))
        (test-assert "file removed after did-delete"
          (null? (walk-file (workspace-file-node workspace) file-path))))
      cleanup))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
