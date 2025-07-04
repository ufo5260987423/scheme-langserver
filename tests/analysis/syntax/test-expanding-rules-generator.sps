#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2025-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver util text)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis local-expand)
    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver protocol alist-access-object))

(test-begin "test-expansion-rule")
    (let* ( [workspace (init-workspace (current-directory) #f #f)]  
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/.akku/lib/srfi/:13/strings.chezscheme.sls"))]
            [document (file-node-document target-file-node)]
            [target-text (document-text document)]
            [target-index-node (pick-index-node-from (document-index-node-list document) (text+position->int target-text 69 4))]
            [include/resolve (car (find-available-references-for document target-index-node 'include/resolve))]
            [to-eval (annotation-stripped (index-node-datum/annotations (index-node-parent target-index-node)))])
        ; (pretty-print (local-expand to-eval document workspace))
        (test-equal #f (null? (local-expand to-eval document root-file-node (workspace-file-linkage workspace)))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
