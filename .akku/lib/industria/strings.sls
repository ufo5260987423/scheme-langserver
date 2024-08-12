;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2012, 2018 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(library (industria strings)
  (export string-split)
  (import (rnrs)
          (only (srfi :13 strings) string-index))

  ;; > (string-split "foo bar" #\space)
  ;; ("foo" "bar")

  ;; > (string-split "foo  bar" #\space)
  ;; ("foo" "" "bar")

  ;; > (string-split "" #\space)
  ;; ("")

  ;; > (string-split "foo bar baz" #\space 1)
  ;; ("foo" "bar baz")

  (define string-split
    (case-lambda
      ((str c max start end)
       (cond ((zero? max)
              (list (substring str start end)))
             ((string-index str c start end) =>
              (lambda (i)
                (cons (substring str start i)
                      (string-split str c (- max 1) (+ i 1) end))))
             (else
              (list (substring str start end)))))
      ((str c max start)
       (string-split str c max start (string-length str)))
      ((str c max)
       (string-split str c max 0 (string-length str)))
      ((str c)
       (string-split str c -1 0 (string-length str))))))
