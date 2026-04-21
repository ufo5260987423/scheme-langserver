#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022-NOW WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) 
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system library-node)

  (scheme-langserver analysis package-manager akku)
  (scheme-langserver analysis tokenizer))

(test-begin "read ss")
  (test-equal 16 (length (source-file->annotations "./run.ss")))
(test-end)

(test-begin "read sps")
  (test-equal 6 (length (source-file->annotations "./tests/log-debug.sps")))
(test-end)

(test-begin "read scm")
  (test-equal 1 (length (source-file->annotations ".akku/lib/srfi/%3a37/srfi-37-reference.scm")))
(test-end)

(test-begin "tolerant parse")
  (test-equal 8 (length (source-file->annotations "tests/resources/incomplete.ss.test")))
(test-end)

;; The following tests verify that source-file->annotations gracefully degrades
;; various lexical and syntactic errors that Chez Scheme's get-datum may throw,
;; instead of crashing. See doc/get-datum-read-exceptions.md for details.

(test-begin "tolerant: eof in list")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/eof-list.ss.test")))
(test-end)

(test-begin "tolerant: sharp-sign prefix")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/sharp-sign.ss.test")))
(test-end)

(test-begin "tolerant: invalid boolean")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/boolean-invalid.ss.test")))
(test-end)

(test-begin "tolerant: boolean delimiter")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/boolean-delimiter.ss.test")))
(test-end)

(test-begin "tolerant: invalid character name")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/char-name.ss.test")))
(test-end)

(test-begin "tolerant: invalid character hex")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/char-hex.ss.test")))
(test-end)

(test-begin "tolerant: invalid character")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/char-invalid.ss.test")))
(test-end)

(test-begin "tolerant: invalid string escape")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/string-escape.ss.test")))
(test-end)

(test-begin "tolerant: invalid string hex")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/string-hex.ss.test")))
(test-end)

(test-begin "tolerant: invalid string intraline")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/string-intraline.ss.test")))
(test-end)

(test-begin "tolerant: invalid number syntax")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/number-syntax.ss.test")))
(test-end)

(test-begin "tolerant: number cannot represent")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/number-represent.ss.test")))
(test-end)

(test-begin "tolerant: unclosed gensym")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/gensym.ss.test")))
(test-end)

(test-begin "tolerant: invalid hash-bang")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/hash-bang.ss.test")))
(test-end)

(test-begin "tolerant: unexpected close parenthesis")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/close-paren.ss.test")))
(test-end)

(test-begin "tolerant: unexpected close bracket")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/close-bracket.ss.test")))
(test-end)

(test-begin "tolerant: paren/bracket mismatch")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/paren-bracket.ss.test")))
(test-end)

(test-begin "tolerant: unexpected dot")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/dot-unexpected.ss.test")))
(test-end)

(test-begin "tolerant: expected one item after dot")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/dot-expected-one.ss.test")))
(test-end)

(test-begin "tolerant: more than one item after dot")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/dot-more-than-one.ss.test")))
(test-end)

(test-begin "tolerant: too many vector elements")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/vector-too-many.ss.test")))
(test-end)

(test-begin "tolerant: invalid vector length")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/vector-invalid-length.ss.test")))
(test-end)

(test-begin "tolerant: non-fixnum in fxvector")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/fxvector-type.ss.test")))
(test-end)

(test-begin "tolerant: invalid fxvector length")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/fxvector-invalid-length.ss.test")))
(test-end)

(test-begin "tolerant: non-flonum in flvector")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/flvector-type.ss.test")))
(test-end)

(test-begin "tolerant: non-octet in bytevector")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/bytevector-non-octet.ss.test")))
(test-end)

(test-begin "tolerant: invalid value in bytevector")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/bytevector-invalid-value.ss.test")))
(test-end)

(test-begin "tolerant: stencil vector no mask")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/stencil-no-mask.ss.test")))
(test-end)

(test-begin "tolerant: invalid stencil vector mask")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/stencil-invalid-mask.ss.test")))
(test-end)

(test-begin "tolerant: record non-symbol")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/record-non-symbol.ss.test")))
(test-end)

(test-begin "tolerant: unrecognized record name")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/record-unknown.ss.test")))
(test-end)

(test-begin "tolerant: duplicate graph mark")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/graph-duplicate.ss.test")))
(test-end)

(test-begin "tolerant: missing graph mark")
  (test-assert (list? (source-file->annotations "tests/resources/tokenizer-exceptions/graph-missing.ss.test")))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
