#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

;; Documents the client opens before they exist on disk (a new, unsaved buffer)
;; must be registered from the didOpen text, edited via didChange, and dropped on
;; didClose if they were never saved.

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver protocol apis document-sync)
  (scheme-langserver protocol apis completion)
  (scheme-langserver util path)
  (scheme-langserver util association))

(define root (string-append (current-directory) "/tests/resources/tmp-unsaved-document"))

(define (cleanup) (system (string-append "rm -rf " root)))

(define (write-file path text)
  (let ([p (open-file-output-port path (file-options replace) 'block (make-transcoder (utf-8-codec)))])
    (put-string p text)
    (close-port p)))

(define (open! workspace path text)
  (did-open workspace
    (make-alist 'textDocument (make-alist 'uri (path->uri path) 'languageId "scheme" 'version 0 'text text))))

(define (close! workspace path)
  (did-close workspace (make-alist 'textDocument (make-alist 'uri (path->uri path)))))

(define (insert! workspace path version line character text)
  (did-change workspace
    (make-alist
      'textDocument (make-alist 'uri (path->uri path) 'version version)
      'contentChanges
        (vector
          (make-alist 'range
            (make-alist 'start (make-alist 'line line 'character character)
              'end (make-alist 'line line 'character character))
            'text text)))))

(define (completion-labels workspace path line character)
  (map (lambda (item) (assq-ref item 'label))
    (vector->list
      (completion workspace
        (make-alist 'textDocument (make-alist 'uri (path->uri path))
          'position (make-alist 'line line 'character character))))))

(define (node workspace path) (walk-file (workspace-file-node workspace) path))

(cleanup)
(mkdir root)
;; One file on disk so the workspace has something in it.
(write-file (string-append root "/existing.scm.txt") "(define existing-answer 1)\n")

(test-begin "unsaved document is registered from didOpen text")
  (let* ([workspace (init-workspace root 'txt 'r6rs #f #f)]
      [path (string-append root "/unsaved.scm")]
      [text "(define unsaved-answer 42)\n(unsaved-ans)"])
    (test-assert "not on disk" (not (file-exists? path)))
    (open! workspace path text)
    (test-assert "node exists after didOpen" (file-node? (node workspace path)))
    (test-equal "document holds the client's text" text (document-text (file-node-document (node workspace path))))
    (test-assert "completion sees definitions in the unsaved buffer"
      (member "unsaved-answer" (completion-labels workspace path 1 12)))
    ;; Typing must keep working: this used to raise on every didChange.
    (insert! workspace path 1 1 12 "w")
    (test-equal "didChange applies to the unsaved buffer"
      "(define unsaved-answer 42)\n(unsaved-answ)"
      (document-text (file-node-document (node workspace path))))
    (close! workspace path)
    (test-assert "never-saved document is dropped on didClose" (null? (node workspace path))))
(test-end)

(test-begin "unsaved document in folders that don't exist yet")
  (let* ([workspace (init-workspace root 'txt 'r6rs #f #f)]
      [dir (string-append root "/new-dir")]
      [path (string-append dir "/deeper/unsaved.scm")])
    (open! workspace path "(define deep-answer 1)\n")
    (test-assert "node exists" (file-node? (node workspace path)))
    (test-assert "virtual folder created" (file-node-folder? (node workspace dir)))
    (close! workspace path)
    (test-assert "document dropped" (null? (node workspace path)))
    (test-assert "empty virtual folders pruned" (null? (node workspace dir))))
(test-end)

(test-begin "library defined in an unsaved document is unregistered on didClose")
  (let* ([workspace (init-workspace root 'txt 'r6rs #f #f)]
      [path (string-append root "/scratch-lib.scm")])
    (open! workspace path "(library (scratch lib) (export f) (import (rnrs)) (define (f) 1))\n")
    (test-assert "library registered"
      (not (null? (walk-library '(scratch lib) (workspace-library-node workspace)))))
    (close! workspace path)
    (test-assert "library unregistered"
      (null? (walk-library '(scratch lib) (workspace-library-node workspace)))))
(test-end)

(test-begin "document saved before didClose is kept")
  (let* ([workspace (init-workspace root 'txt 'r6rs #f #f)]
      [path (string-append root "/saved-later.scm")]
      [text "(define saved-answer 1)\n"])
    (open! workspace path text)
    (write-file path text)
    (close! workspace path)
    (test-assert "still in the workspace" (file-node? (node workspace path)))
    (delete-file path))
(test-end)

(test-begin "didChange for an unknown document is ignored")
  (let ([workspace (init-workspace root 'txt 'r6rs #f #f)])
    (test-assert "does not raise"
      (guard (e [else #f])
        (insert! workspace (string-append root "/never-opened.scm") 1 0 0 "x")
        #t)))
(test-end)

(cleanup)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
