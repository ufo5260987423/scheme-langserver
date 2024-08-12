;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011, 2012 Göran Weinholt <goran@weinholt.se>

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

;; tcp-connect for GNU Guile. Tested with Guile 2.0.1.

(library (industria tcp)
  (export tcp-connect)
  (import (rnrs)
          (guile))

  ;; Returns: input-port output-port
  (define (tcp-connect host service)
    (let lp ((addrs (catch 'getaddrinfo-error
                      (lambda ()
                        (getaddrinfo host service 0 0 SOCK_STREAM))
                      (lambda (key errcode)
                        (error 'tcp-connect (gai-strerror errcode)
                               host service)))))
      (if (null? addrs)
          (error 'tcp-connect "Could not connect" host service)
          (let* ((addr (car addrs))
                 (s (socket (addrinfo:fam addr)
                            (addrinfo:socktype addr)
                            (addrinfo:protocol addr))))
            (catch 'system-error
                   (lambda ()
                     (connect s (addrinfo:addr addr))
                     (values s s))
                   (lambda (key subr message args . rest)
                     (if (null? (cdr addrs))
                         (error 'tcp-connect (apply format #f message args)
                                host service)
                         (lp (cdr addrs))))))))))
