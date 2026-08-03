#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver protocol apis completion)
  (scheme-langserver util association)
  (scheme-langserver util path))

(test-begin "completion on unresolved uri returns empty vector")
(let* ([workspace (init-workspace (current-directory) 'akku 'r6rs #f #f)]
       [params (make-alist
                 'textDocument (make-alist 'uri "file:///nonexistent/ignored.sls")
                 'position (make-alist 'line 0 'character 0))]
       [result (completion workspace params)])
  (test-equal "unresolved uri returns empty vector" '#() result))
(test-end)

(test-begin "completion item provides textEdit and suffix insertText")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/completion-helix")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
       [target-path (string-append fixture "/main.scm.txt")]
       [uri (path->uri target-path)]
       [params (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line 4 'character 6))]
       [result (completion workspace params)]
       [items (vector->list result)]
       [display-item (find (lambda (item)
                             (string=? "display" (assq-ref item 'label)))
                           items)])
  (test-assert "display candidate found" (not (eq? #f display-item)))
  (test-equal "insertText is suffix after prefix" "play" (assq-ref display-item 'insertText))
  (let ([text-edit (assq-ref display-item 'textEdit)])
    (test-equal "textEdit newText is full identifier" "display" (assq-ref text-edit 'newText))
    (let ([range (assq-ref text-edit 'range)]
          [start (assq-ref (assq-ref text-edit 'range) 'start)]
          [end (assq-ref (assq-ref text-edit 'range) 'end)])
      (test-equal "textEdit start line" 4 (assq-ref start 'line))
      (test-equal "textEdit start character" 3 (assq-ref start 'character))
      (test-equal "textEdit end line" 4 (assq-ref end 'line))
      (test-equal "textEdit end character" 6 (assq-ref end 'character)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
