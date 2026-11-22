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
(let* ([workspace (init-workspace (string-append (current-directory) "/util/") 'scheme 'r6rs #f #f)]
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

;; With auto-pairs, typing "(display " leaves the cursor on whitespace right
;; before the closing paren. Completion there used to raise
;; ("~s is not of type ~s" () index-node) when type inference was enabled
;; (the server's default).
(test-begin "completion on whitespace before a closing paren")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/completion-whitespace")]
       [workspace (init-workspace fixture 'txt 'r6rs #f #t)]
       [uri (path->uri (string-append fixture "/main.scm.txt"))]
       [complete
         (lambda (line character)
           (guard (e [else (list 'raised (if (condition? e) (condition-message e) e))])
             (completion workspace
               (make-alist
                 'textDocument (make-alist 'uri uri)
                 'position (make-alist 'line line 'character character)))))]
       [labels
         (lambda (result)
           (if (vector? result) (map (lambda (item) (assq-ref item 'label)) (vector->list result)) '()))])
  (let ([result (complete 1 9)])
    (test-assert "(display |) returns a vector" (vector? result))
    (test-assert "(display |) offers identifiers in scope" (member "greeting" (labels result))))
  (test-assert "( |) returns a vector" (vector? (complete 2 2)))
  (test-assert "(display \"a\" |) returns a vector" (vector? (complete 3 13)))
  ;; Other positions around top-level and nested lists keep working.
  (test-assert "top-level gree| returns a vector" (vector? (complete 4 4)))
  (test-assert "(list (display |)) offers greeting" (member "greeting" (labels (complete 5 15)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
