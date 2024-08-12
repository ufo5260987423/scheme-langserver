#!r6rs
;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2012 Göran Weinholt <goran@weinholt.se>

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

;; There is a proper way of doing TCP in Chez, and this is not it.
;; This is the embarrassing way. But it's useful for testing.

(library (industria tcp)
  (export tcp-connect)
  (import (chezscheme))

  (define (tcp-connect host service)
    (putenv "_CHEZHOST_" host)
    (putenv "_CHEZSERVICE_" service)
    ;; Talk with netcat...
    (let-values (((o i e pid)
                  (open-process-ports "nc \"$_CHEZHOST_\" \"$_CHEZSERVICE_\"")))
      (close-port e)
      (values i o))))
