#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (only (srfi :13 strings) string-suffix?)
    (scheme-langserver analysis package-manager file-filter)
    (scheme-langserver virtual-file-system file-node))

(test-begin "file-filter-extension")
  (let ([filter (make-extension-filter '(".sls" ".scm"))]
        [fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/mixed-extensions")])
    (test-equal #t (filter fixture))
    (test-equal #t (filter (string-append fixture "/lib.sls")))
    (test-equal #t (filter (string-append fixture "/lib.scm")))
    (test-equal #f (filter (string-append fixture "/lib.ss")))
    (test-equal #f (filter (string-append fixture "/lib.txt"))))
(test-end)

(test-begin "file-filter-scheme-preset")
  (let ([filter (make-scheme-file-filter)]
        [fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/mixed-extensions")])
    (test-equal #t (filter fixture))
    (test-equal #t (filter (string-append fixture "/lib.sls")))
    (test-equal #t (filter (string-append fixture "/lib.scm")))
    (test-equal #t (filter (string-append fixture "/lib.ss")))
    (test-equal #t (filter (string-append fixture "/lib.sps")))
    (test-equal #t (filter (string-append fixture "/lib.sld")))
    (test-equal #f (filter (string-append fixture "/lib.txt"))))
(test-end)

(test-begin "file-filter-config-predicate")
  (let ([predicate (lambda (path) (string-suffix? ".custom" path))]
        [fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/mixed-extensions")])
    (test-equal #t ((file-filter->predicate predicate fixture) "/some/file.custom"))
    (test-equal #f ((file-filter->predicate predicate fixture) "/some/file.sls"))
    (test-equal #t (file-filter-config? predicate))
    (test-equal #f (file-filter-config-serializable? predicate)))
(test-end)

(test-begin "file-filter-config-symbol")
  (let ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/mixed-extensions")])
    (test-equal #t (file-filter-config? 'scheme))
    (test-equal #t (file-filter-config-serializable? 'scheme))
    (test-equal #t (procedure? (file-filter->predicate 'scheme fixture)))
    (test-equal #t (procedure? (file-filter->predicate 'txt fixture)))
    (test-equal #t (procedure? (file-filter->predicate 'akku fixture))))
(test-end)

(test-begin "file-filter-config-extension-list")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/mixed-extensions")]
        [filter (file-filter->predicate '(".sls" ".scm.txt") fixture)])
    (test-equal #t (file-filter-config? '(".sls" ".scm.txt")))
    (test-equal #t (file-filter-config-serializable? '(".sls" ".scm.txt")))
    (test-equal #t (filter fixture))
    (test-equal #t (filter (string-append fixture "/lib.sls")))
    (test-equal #t (filter (string-append fixture "/extra.scm.txt")))
    (test-equal #f (filter (string-append fixture "/helper.scm"))))
(test-end)

(test-begin "scheme-file?/extensions")
  (test-equal #t (scheme-file?/extensions "/some/dir/lib.sls" '(".sls" ".scm")))
  (test-equal #t (scheme-file?/extensions "/some/dir/lib.scm" '(".sls" ".scm")))
  (test-equal #f (scheme-file?/extensions "/some/dir/lib.ss" '(".sls" ".scm")))
  (test-equal #f (scheme-file?/extensions "/some/dir" '(".sls" ".scm")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
