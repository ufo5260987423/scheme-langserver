#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (rnrs (6))
  (srfi :64 testing)
  (scheme-langserver util text)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver analysis package-manager akku)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver protocol alist-access-object))

;; from test-document.sps
(test-begin "walk-file for .scm and find document")
  (let* ([target-path (string-append (current-directory) "/.akku/lib/srfi/%3a13")]
      [root-file-node (init-virtual-file-system target-path '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
      [target-file-node (walk-file root-file-node (string-append target-path "/srfi-13.scm"))])
    (test-equal #f (null? (document-index-node-list (file-node-document target-file-node)))))
(test-end)

;; from test-file-node.sps
(test-begin "walk-file")
  (let* ([root-file-node (init-virtual-file-system "./util/" '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))])
    (test-equal "io.sls" (file-node-name (walk-file root-file-node "./util/io.sls"))))
(test-end)

;; from test-index-node.sps
(test-begin "pick-index-node")
  (let* ([workspace (init-workspace (string-append (current-directory) "/protocol") '() #f #f)]
      [root-file-node (workspace-file-node workspace)]
      [root-library-node (workspace-library-node workspace)]
      [target-file-node (walk-file root-file-node (string-append (current-directory) "/protocol/request.sls"))]
      [document (file-node-document target-file-node)]
      [index-node-list (document-index-node-list document)]
      [root-index-node (car index-node-list)]
      [index-node (find-define-with-params root-index-node 'read-message)])
    (test-equal #f (null? index-node)))
(test-end)

;; from test-library-node.sps
(test-begin "walk-library")
  (let* ([root-file-node (init-virtual-file-system "./util/" '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
      [root-library-node (init-library-node root-file-node)])
    (test-equal "io.sls"
      (file-node-name (car (library-node-file-nodes 
        (walk-library 
          '(scheme-langserver util io) 
          root-library-node))))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
