#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (scheme-langserver analyse index))

(test-begin "index-test")
    (test-equal '() (init-index '() (source-file->annotation "./util/io.sls")))
(test-end)


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
