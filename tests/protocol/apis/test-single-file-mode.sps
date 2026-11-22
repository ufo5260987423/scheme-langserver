#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; SPDX-License-Identifier: MIT
#!r6rs

;; Without a workspace root (initialize with rootUri, rootPath and workspaceFolders
;; all null), the server runs in single-file mode: the workspace holds exactly the
;; documents the client has open, and nothing is ever scanned from disk.

(import
  (rnrs (6))
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver protocol apis document-sync)
  (scheme-langserver protocol apis file-change-notification)
  (scheme-langserver protocol apis completion)
  (scheme-langserver util path)
  (scheme-langserver util association))

(define dir (string-append (current-directory) "/tests/resources/tmp-single-file-mode"))

(define (write-file path text)
  (let ([p (open-file-output-port path (file-options replace) 'block (make-transcoder (utf-8-codec)))])
    (put-string p text)
    (close-port p)))

(define (open! workspace path text)
  (did-open workspace
    (make-alist 'textDocument (make-alist 'uri (path->uri path) 'languageId "scheme" 'version 0 'text text))))

(define (close! workspace path)
  (did-close workspace (make-alist 'textDocument (make-alist 'uri (path->uri path)))))

(define (completion-labels workspace path line character)
  (map (lambda (item) (assq-ref item 'label))
    (vector->list
      (completion workspace
        (make-alist 'textDocument (make-alist 'uri (path->uri path))
          'position (make-alist 'line line 'character character))))))

(define (node workspace path) (walk-file (workspace-file-node workspace) path))
(define (root-children workspace) (file-node-children (workspace-file-node workspace)))

(system (string-append "rm -rf " dir))
(mkdir dir)
(write-file (string-append dir "/on-disk.scm") "(define on-disk-answer 1)\n(on-disk-ans)")
(write-file (string-append dir "/sibling.scm") "(define sibling-answer 2)\n")

(test-begin "single-file workspace starts empty")
  (let ([workspace (init-single-file-workspace 'akku 'r6rs #f #f)])
    (test-assert "single-file?" (workspace-single-file? workspace))
    (test-equal "nothing indexed" '() (root-children workspace)))
(test-end)

(test-begin "single-file mode: a file that exists on disk")
  (let* ([workspace (init-single-file-workspace 'akku 'r6rs #f #f)]
      [path (string-append dir "/on-disk.scm")])
    (open! workspace path "(define on-disk-answer 1)\n(on-disk-ans)")
    (test-assert "opened document is indexed" (file-node? (node workspace path)))
    (test-assert "siblings on disk are not scanned" (null? (node workspace (string-append dir "/sibling.scm"))))
    (test-assert "completion works" (member "on-disk-answer" (completion-labels workspace path 1 12)))
    (close! workspace path)
    (test-assert "dropped on didClose even though it exists on disk" (null? (node workspace path)))
    (test-equal "folders pruned" '() (root-children workspace)))
(test-end)

(test-begin "single-file mode: an unsaved buffer")
  (let* ([workspace (init-single-file-workspace 'akku 'r6rs #f #f)]
      [path (string-append dir "/unsaved.scm")])
    (open! workspace path "(define unsaved-answer 1)\n(unsaved-ans)")
    (test-assert "completion works" (member "unsaved-answer" (completion-labels workspace path 1 12)))
    (close! workspace path)
    (test-equal "dropped on didClose" '() (root-children workspace)))
(test-end)

(test-begin "single-file mode never scans the disk on file events")
  (let* ([workspace (init-single-file-workspace 'akku 'r6rs #f #f)]
      [sibling (string-append dir "/sibling.scm")])
    (did-change-watched-files workspace
      (make-alist 'changes (vector (make-alist 'uri (path->uri sibling) 'type 1))))
    (test-equal "watched-file create ignored" '() (root-children workspace))
    (did-create workspace (make-alist 'files (vector (make-alist 'uri (path->uri sibling)))))
    (test-equal "didCreateFiles ignored" '() (root-children workspace))
    (refresh-workspace workspace)
    (test-equal "refresh-workspace is a no-op" '() (root-children workspace)))
(test-end)

(system (string-append "rm -rf " dir))

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
