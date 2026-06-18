#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (chezscheme) (srfi :64 testing) 
  (scheme-langserver analysis package-manager akku))

(test-begin "akku path")
  (let ([checker (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list"))])
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/scheme-langserver")))
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/scheme-langserver/")))
    (test-equal #f (checker (string-append (current-directory) "/.akku/lib/scheme-langserver.chezscheme.sls")))
    (test-equal #f (checker (string-append (current-directory) "/.akku/lib/scheme-langserver.sls")))
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/ufo-match.chezscheme.sls")))
    (test-equal #f (checker (string-append (current-directory) "/.akku/")))
    (test-equal #t (checker (string-append (current-directory) "/scheme-langserver.sls")))
    (test-equal #t (checker (string-append (current-directory) "/util/path.sls")))
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/srfi/:13/strings.chezscheme.sls")))
    ; .akku/list contains included files under percent-encoded directories
    ; (e.g. %3a152) because include/resolve uses encoded strings; ensure the
    ; filter accepts the real filesystem paths as listed.
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/srfi/%3a152/r7rs-shim.scm")))
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib/")))
    (test-equal #t (checker (string-append (current-directory) "/.akku/lib"))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
