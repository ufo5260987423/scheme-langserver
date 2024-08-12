;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (hashing private compat)
  (export bitwise-rotate-bit-field bitwise-reverse-bit-field)
  (import (only (rnrs) bitwise-rotate-bit-field bitwise-reverse-bit-field)))
