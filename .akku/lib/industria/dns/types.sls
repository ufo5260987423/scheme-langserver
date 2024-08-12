;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2011, 2012, 2017, 2018 Göran Weinholt <goran@weinholt.se>

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

;; Parsing and formatting of DNS resource records

;; Text parsing (master file format) is not done yet.

;; TODO: NSEC wire formatting

;; TODO: parameter for only decoding the resources that have
;; compressed labels (for mad speed). procedure for going to and
;; from the raw form.

;; The RRs NS, SOA, CNAME and PTR can contain compressed labels. But
;; some servers compress labels in SRVs and more, so maybe it's safe?

(library (industria dns types)
  (export dns-resource-wire-read
          dns-resource-wire-write
          ;; dns-resource-read
          dns-resource-print

          ;; Fields common to all RR types
          dns-resource?
          dns-resource-name
          dns-resource-type
          dns-resource-class
          dns-resource-ttl

          ;; Unparsed RRs
          make-dns-resource/raw dns-resource/raw?
          dns-resource/raw-rdata

          ;; Known and parsed RRs
          make-dns-resource/A dns-resource/A?
          dns-resource/A-address

          make-dns-resource/NS dns-resource/NS?
          dns-resource/NS-name

          make-dns-resource/CNAME dns-resource/CNAME?
          dns-resource/CNAME-name

          make-dns-resource/SOA dns-resource/SOA?
          dns-resource/SOA-mname dns-resource/SOA-rname
          dns-resource/SOA-serial dns-resource/SOA-refresh
          dns-resource/SOA-retry dns-resource/SOA-expire
          dns-resource/SOA-minimum

          make-dns-resource/MX dns-resource/MX?
          dns-resource/MX-preference dns-resource/MX-exchange

          make-dns-resource/AAAA dns-resource/AAAA?
          dns-resource/AAAA-address

          make-dns-resource/SRV dns-resource/SRV?
          dns-resource/SRV-priority dns-resource/SRV-weight
          dns-resource/SRV-port dns-resource/SRV-target

          make-dns-resource/CERT dns-resource/CERT?
          dns-resource/CERT-type dns-resource/CERT-key-tag
          dns-resource/CERT-algorithm dns-resource/CERT-certificate

          make-dns-resource/DNAME dns-resource/DNAME?
          dns-resource/DNAME-name

          make-dns-resource/DS dns-resource/DS?
          dns-resource/DS-key-tag dns-resource/DS-algorithm
          dns-resource/DS-digest-type dns-resource/DS-digest

          make-dns-resource/SSHFP dns-resource/SSHFP?
          dns-resource/SSHFP-algorithm dns-resource/SSHFP-type
          dns-resource/SSHFP-fingerprint

          make-dns-resource/RRSIG dns-resource/RRSIG?
          dns-resource/RRSIG-type dns-resource/RRSIG-algorithm
          dns-resource/RRSIG-labels dns-resource/RRSIG-original-ttl
          dns-resource/RRSIG-expiration dns-resource/RRSIG-inception
          dns-resource/RRSIG-key-tag dns-resource/RRSIG-signer
          dns-resource/RRSIG-signature

          make-dns-resource/NSEC dns-resource/NSEC?
          dns-resource/NSEC-name dns-resource/NSEC-types

          make-dns-resource/DNSKEY dns-resource/DNSKEY?
          dns-resource/DNSKEY-flags dns-resource/DNSKEY-protocol
          dns-resource/DNSKEY-algorithm dns-resource/DNSKEY-key
          dns-resource/DNSKEY-key-tag

          make-dns-resource/TSIG dns-resource/TSIG?
          dns-resource/TSIG-algorithm dns-resource/TSIG-time
          dns-resource/TSIG-fudge dns-resource/TSIG-mac
          dns-resource/TSIG-original-id dns-resource/TSIG-error
          dns-resource/TSIG-other-data)
  (import (rnrs)
          (only (srfi :1 lists) iota append-map)
          (only (srfi :13 strings) string-join string-map)
          (srfi :19 time)
          (srfi :26 cut)
          (srfi :39 parameters)
          (ip-address)
          (struct pack)
          (industria bytevectors)
          (industria dns numbers)
          (industria dns private)
          (industria base64))

  (define (dns-resource-print p split-length r)
    (if (dns-resource/raw? r)
        (raw-format-text p split-length r)
        ((cadddr (vector-ref handlers (dns-resource-type r))) p split-length r)))

  (define (dns-resource-wire-read type name ttl class bv start end)
    ;; TODO: should maybe handle errors in such a way that raw records
    ;; are returned when the original can't be parsed properly?
    (if (> type 255)
        (raw-parse name ttl class type bv start end)
        ((car (vector-ref handlers type))
         name ttl class bv start end)))

  (define (dns-resource-wire-write p table offset r)
    (if (dns-resource/raw? r)
        (raw-rdata p table offset r)
        ((caddr (vector-ref handlers (dns-resource-type r))) p table offset r)))

  (define (print-hex bv split-length p)
    (do ((sz split-length (if (<= sz 0)
                              split-length
                              (- sz 2)))
         (i 0 (+ i 1)))
        ((= i (bytevector-length bv)))
      (let ((v (bytevector-u8-ref bv i)))
        (if (< v #x10) (write-char #\0 p))
        (display (number->string v 16) p)
        (if (<= sz 0) (write-char #\space p)))))

  (define (print-base64 bv split-length port)
    (display (string-map
              (lambda (c) (if (char=? c #\newline) #\space c))
              (base64-encode bv 0 (bytevector-length bv)
                             (and split-length
                                  (bitwise-and split-length -3))))
             port))

  (define (print-labels labels suffix p)
    (if (null? labels)
        (display "." p)
        (for-each (cut display-dns-label <> p) labels))
    (display suffix p))

  (define-record-type dns-resource
    (fields name ttl class type)
    (protocol
     (lambda (p)
       (lambda (name ttl class type)
         (p name ttl class type)))))

  (define-record-type dns-resource/raw
    (parent dns-resource)
    (fields rdata)
    (protocol
     (lambda (p)
       (lambda (name ttl class type bytevector)
         ((p name ttl class type) bytevector)))))

  (define (raw-parse name ttl class type bv start end)
    (make-dns-resource/raw name ttl class type (subbytevector bv start end)))

  (define (raw-rdata port table offset rec)
    (put-bytevector port (dns-resource/raw-rdata rec)))

  (define (raw-format-text port split-length rec)
    (let ((rdata (dns-resource/raw-rdata rec)))
      (display "\\# " port)
      (display (bytevector-length rdata) port)
      (display #\space port)
      (print-hex rdata split-length port)))

  ;; For each RR type there are four procedures: two for the binary
  ;; wire format and two for the master zone format. Parse and format.
  ;;; TODO: rrtype is u16, so extend this
  ;;; table when RR types > 256 are wanted
  (define handlers
    (vector-map (lambda (type)
                  (list (cut raw-parse <> <> <> type <> <> <>)
                        'raw-parse-text
                        raw-rdata raw-format-text))
                (list->vector (iota 256))))

;;; Record types

  ;; This code assumes that the class is IN. If other classes are ever
  ;; needed then dns-resource-wire-read etc has access to the class
  ;; and can call other procedures instead of these.

  (define-syntax define-dns-record-type
    (lambda (x)
      (define (symcat id . xs)
        (datum->syntax id
                       (string->symbol
                        (apply string-append (map symbol->string xs)))))
      (syntax-case x (type fields parser formatter protocol lambda)
        ((_ name
            (fields field-spec ...)
            (parser bparse              ;parse rdata, return fields
                    tparse)             ;parse text, return fields
            (formatter bformat          ;format rdata, return bytevector
                       tformat)         ;print rdata in text format
            (protocol
             (lambda (proto-parent)
               (lambda (proto-name
                         proto-ttl proto-class
                        proto-formals ...)
                 proto-body ...))))
         (with-syntax ((defname (symcat #'name 'dns-resource/
                                      (syntax->datum #'name)))
                       (make (symcat #'name 'make-dns-resource/
                                      (syntax->datum #'name))))
           #'(begin
               (define-record-type defname
                 (parent dns-resource)
                 (fields field-spec ...)
                 (protocol
                  (lambda (p)
                    (lambda (proto-name proto-ttl proto-class proto-formals ...)
                      (let ((proto-parent (p proto-name proto-ttl proto-class
                                             (dns-rrtype name))))
                        proto-body ...)))))
               (define dummy
                 (begin
                   (vector-set! handlers (dns-rrtype name)
                                (list
                                 (let ((parser bparse))
                                   (lambda (name ttl class bv start end)
                                     (apply make name ttl class
                                            (parser bv start end))))
                                 tparse bformat tformat))
                   #f)))))
        ((_ name
            (fields field-spec ...)
            p f)
         #'(define-dns-record-type name
             (fields field-spec ...) p f
             (protocol
              (lambda (parent)
                (lambda (name* ttl* class* field-spec ...)
                  (parent field-spec ...)))))))))

  (define-dns-record-type A
    (fields address)
    (parser
     (lambda (bv start end)
       ;; XXX: This doesn't handle the special format for CH
       (assert (= (- end start) 4))
       (list (subbytevector bv start end)))
     'TODO-A)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (dns-resource/A-address rec)))
     (lambda (p _ rec)
       (display (ipv4->string (dns-resource/A-address rec)) p))))

  (define (print-generic-rec p _ rec)
    (let* ((rtd (record-rtd rec))
           (fields (vector-length (record-type-field-names rtd))))
      (define (disp i)
        (let ((x ((record-accessor rtd i) rec)))
          (if (list? x)
              (print-labels x "" p)
              (display x p))))
      (disp 0)
      (do ((i 1 (+ i 1)))
          ((= i fields))
        (display " " p)
        (disp i))))

  (define (parse-wire-label bv start end)
    ;; Parse a single label. Useful for NS, CNAME, DNAME...
    (let-values (((server _) (parse-labels bv start)))
      (list server)))

  (define-dns-record-type NS
    (fields name)
    (parser parse-wire-label 'TODO-NS)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/NS-name rec) table offset))
     print-generic-rec))

  (define-dns-record-type CNAME
    (fields name)
    (parser parse-wire-label 'TODO-CNAME)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/CNAME-name rec) table offset))
     print-generic-rec))

  (define-dns-record-type SOA
    (fields mname rname serial refresh retry expire minimum)
    (parser
     (lambda (bv start end)
       (let*-values (((mname end-mname) (parse-labels bv start))
                     ((rname end-rname) (parse-labels bv end-mname))
                     ((serial refresh retry expire minimum)
                      (unpack "!u5L" bv end-rname)))
         (list mname rname serial refresh retry expire minimum)))
     'TODO-SOA)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/SOA-mname rec) table offset)
       (put-labels p (dns-resource/SOA-rname rec) table offset)
       (put-bytevector p (pack "!5L"
                               (dns-resource/SOA-serial rec)
                               (dns-resource/SOA-refresh rec)
                               (dns-resource/SOA-retry rec)
                               (dns-resource/SOA-expire rec)
                               (dns-resource/SOA-minimum rec))))
     print-generic-rec))

  (define-dns-record-type PTR
    (fields pointer)
    (parser
     (lambda (bv start end)
       (let-values (((pointer lend) (parse-labels bv start)))
         (assert (= lend end))
         (list pointer)))
     'TODO-SOA)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/PTR-pointer rec) table offset))
     print-generic-rec))

  (define-dns-record-type MX
    (fields preference exchange)
    (parser
     (lambda (bv start end)
       (let-values (((preference) (unpack "!uS" bv start))
                    ((exchange eend) (parse-labels bv (+ start 2))))
         (assert (= end eend))
         (list preference exchange)))
     'TODO-MX)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "!S" (dns-resource/MX-preference rec)))
       (put-labels p (dns-resource/MX-exchange rec) table offset))
     print-generic-rec))

  (define-dns-record-type TXT
    (fields strings)
    (parser
     (lambda (bv start end)
       (define (parse-character-strings bv start count)
         (if (zero? count)
             '()
             (let* ((len (unpack "C" bv start))
                    (end (+ start 1 len)))
               (cons (subbytevector bv (+ start 1) end)
                     (parse-character-strings bv end
                                              (- count 1 len))))))
       (list (parse-character-strings bv start (- end start))))
     'TODO-TXT)
    (formatter
     (lambda (p table offset rec)
       (for-each (lambda (bv)
                   (put-u8 p (bytevector-length bv))
                   (put-bytevector p bv))
                 (dns-resource/TXT-strings rec)))
     (lambda (p _ rec)
       (do ((rdata (dns-resource/TXT-strings rec) (cdr rdata)))
           ((null? rdata))
         (display-dns-string (car rdata) p)
         (unless (null? (cdr rdata))
           (display #\space))))))
  
  (define-dns-record-type AAAA
    (fields address)
    (parser
     (lambda (bv start end)
       (let ((data (subbytevector bv start (+ start 16))))
         (list data)))
     'TODO-AAAA)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (dns-resource/AAAA-address rec)))
     (lambda (p _ rec)
       (display (ipv6->string (dns-resource/AAAA-address rec)) p))))

  (define-dns-record-type SRV
    (fields priority weight port target)
    (parser
     (lambda (bv start end)
       (let-values (((priority weight port) (unpack "!uSSS" bv start))
                    ((target _)
                     (parse-labels bv (+ start (format-size "!uSSS")))))
         (list priority weight port target)))
     'TODO-SRV)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "!SSS" (dns-resource/SRV-priority rec)
                               (dns-resource/SRV-weight rec)
                               (dns-resource/SRV-port rec)))
       (put-labels p (dns-resource/SRV-target rec))) ;not compressed
     print-generic-rec))

  (define-dns-record-type CERT
    (fields type key-tag algorithm certificate)
    (parser
     (lambda (bv start end)
       (let-values (((type key-tag algo) (unpack "!uSSC" bv start)))
         (let ((cert (subbytevector bv (+ start (format-size "!uSSC")) end)))
           (list type key-tag algo cert))))
     'TODO-CERT)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "!SSC" (dns-resource/CERT-type rec)
                               (dns-resource/CERT-key-tag rec)
                               (dns-resource/CERT-algorithm rec)))
       (put-bytevector p (dns-resource/CERT-certificate rec)))
     (lambda (p split-size rec)
       (for-each (cut display <> p)
                 (list
                  (integer->dns-cert-type (dns-resource/CERT-type rec)) " "
                  (dns-resource/CERT-key-tag rec) " "
                  (dns-resource/CERT-algorithm rec) " "))
       (print-base64 (dns-resource/CERT-certificate rec) split-size p))))

  (define-dns-record-type DNAME
    (fields name)
    (parser parse-wire-label 'TODO-DNAME)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/DNAME-name rec))) ;not compressed
     print-generic-rec))

  (define-dns-record-type DS
    (fields key-tag algorithm digest-type digest)
    (parser
     (lambda (bv start end)
       (let-values (((key-tag algorithm digest-type) (unpack "!uSCC" bv start)))
         (list key-tag algorithm digest-type
               (subbytevector bv (+ start (format-size "!uSCC")) end))))
     'TODO-DS)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "!SCC" (dns-resource/DS-key-tag rec)
                               (dns-resource/DS-algorithm rec)
                               (dns-resource/DS-digest-type rec)))
       (put-bytevector p (dns-resource/DS-digest rec)))
     (lambda (p split-size rec)
       (let ((algo (dns-resource/DS-algorithm rec))
             (type (dns-resource/DS-digest-type rec)))
         (for-each (cut display <> p)
                   (list
                    (dns-resource/DS-key-tag rec) " "
                    algo " " type " "))
         (print-hex (dns-resource/DS-digest rec) split-size p)
         (display " ;" p)
         (cond ((assv algo (dnssec-algorithm)) =>
                (lambda (a) (display (cdr a) p))))
         (cond ((assv type (dnssec-digest)) =>
                (lambda (a)
                  (display ", " p)
                  (display (cdr a) p))))))))

  (define-dns-record-type SSHFP
    (fields algorithm type fingerprint)
    (parser
     (lambda (bv start end)
       (let-values (((algo type) (unpack "!CC" bv start)))
         (let ((fp (subbytevector bv (+ start (format-size "!CC")) end)))
           (list algo type fp))))
     'TODO-SSHFP)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "CC" (dns-resource/SSHFP-algorithm rec)
                               (dns-resource/SSHFP-type rec)))
       (put-bytevector p (dns-resource/SSHFP-fingerprint rec)))
     (lambda (p split-size rec)
       (let ((algo (dns-resource/SSHFP-algorithm rec))
             (type (dns-resource/SSHFP-type rec)))
         (for-each (cut display <> p)
                   (list algo " " type " "))
         (print-hex (dns-resource/SSHFP-fingerprint rec) split-size p)
         (display " ;" p)
         (cond ((assv algo (dns-sshfp-algorithm))
                => (lambda (x)
                     (display (cdr x) p))))
         (cond ((assv type (dns-sshfp-type))
                => (lambda (x)
                     (display " " p)
                     (display (cdr x) p))))))))

  (define (readable-time time)
    ;; Convert number of seconds elapsed since 1 January 1970 00:00:00
    ;; UTC, ignoring leap seconds, into a humanly readable string.
    ;; TODO: buggy, because SRFI-19 does not specify the epoch. :(
    (date->string (time-utc->date (make-time 'time-utc 0 time) 0)
                  "~Y~m~d~H~M~S"))

  (define-dns-record-type RRSIG
    (fields type algorithm labels original-ttl expiration inception
            key-tag signer signature)
    (parser
     (lambda (bv start end)
       (let*-values (((type algorithm labels ttl expiration inception key-tag)
                      ;; XXX:Y2106
                      (unpack "!uSCClLLS" bv start))
                     ((signer end-sn)
                      (parse-labels bv (+ start (format-size "!uSCClLLS")))))
         (list type algorithm labels ttl
               expiration inception key-tag
               signer (subbytevector bv end-sn end))))
     'TODO-RRSIG)
    (formatter
     (lambda (p table offset rec)
       (put-bytevector p (pack "!SCClLLS"
                               (dns-resource/RRSIG-type rec)
                               (dns-resource/RRSIG-algorithm rec)
                               (dns-resource/RRSIG-labels rec)
                               (dns-resource/RRSIG-original-ttl rec)
                               (dns-resource/RRSIG-expiration rec)
                               (dns-resource/RRSIG-inception rec)
                               (dns-resource/RRSIG-key-tag rec)))
       ;; Not compressed
       (put-labels p (dns-resource/RRSIG-signer rec))
       (put-bytevector p (dns-resource/RRSIG-signature rec)))
     (lambda (p split-size rec)
       (for-each (cut display <> p)
                 (list
                  (integer->dns-rrtype
                   (dns-resource/RRSIG-type rec)) " "
                  (integer->dnssec-algorithm
                   (dns-resource/RRSIG-algorithm rec)) " "
                  (dns-resource/RRSIG-labels rec) " "
                  (dns-resource/RRSIG-original-ttl rec) " "
                  (readable-time (dns-resource/RRSIG-expiration rec)) " "
                  (readable-time (dns-resource/RRSIG-inception rec)) " "
                  (dns-resource/RRSIG-key-tag rec) " "))
       (print-labels (dns-resource/RRSIG-signer rec) " " p)
       (print-base64 (dns-resource/RRSIG-signature rec) split-size p))))

  (define (pseudo-rr? r)
    ;; Returns #t if the given RR type should never occur in zone
    ;; data.
    (or (<= 128 r 255)                  ;Q and Meta
        (= r (dns-rrtype OPT))
        (= r 0)))

  (define (parse-type-bitmap bv start end)
    ;; Suppose that one was to encode the type space in a 65536-bit
    ;; number. That would be wasteful of space. But since there are
    ;; big sequences of zero bits, those sequences are removed: Type
    ;; bitmaps are sequences of a block window number, a length and a
    ;; few bytes. The block window number is the upper eight bits of a
    ;; type number, and the bytes represent an integer in little
    ;; endian byte and big endian bit order. The bits in this integer
    ;; are ones for any types that should be included in the bitmap.
    ;; TODO: test this with more than one block window
    (let ((count (- end start)))
      (if (< count 3)
          '()
          (let-values (((block len) (unpack "CC" bv start)))
            (unless (and (< len 32) (< len count))
              (error 'parse-type-bitmap "invalid length in type bitmap"))
            (append
             (append-map
              (lambda (base i)
                (let ((byte (bytevector-u8-ref bv i)))
                  (filter (lambda (type)
                            ;; Remove Q, Meta etc
                            (and type (not (pseudo-rr? type))))
                          (map (lambda (b)
                                 (and (fxbit-set? byte b)
                                      (fxior (fxarithmetic-shift-left block 8)
                                             ;; network bit order
                                             (+ base (- 7 b)))))
                               (iota 8 7 -1))))) ;reverse
              (iota len 0 8)
              (iota len (+ start 2)))
             (parse-type-bitmap bv (+ start 2 len) (- count 2 len)))))))

  (define-dns-record-type NSEC
    (fields name types)
    (parser
     (lambda (bv start end)
       (let-values (((next-domain-name end-ndn) (parse-labels bv start)))
         (list next-domain-name
               (parse-type-bitmap bv end-ndn end))))
     'TODO-NSEC)
    (formatter
     'TODO-NSEC
     (lambda (p _ rec)
       (print-labels (dns-resource/NSEC-name rec) "" p)
       (for-each
        (lambda (type)
          (display " " p)
          (display (integer->dns-rrtype type) p))
        (dns-resource/NSEC-types rec)))))

  (define (key-tag bv)
    ;; Key Tag from RFC 4034, Appendix B
    (do ((i 0 (+ i 1))
         (x #f (not x))
         (acc 0 (bitwise-and #xffffffff
                             (+ acc (fxarithmetic-shift-left
                                     (bytevector-u8-ref bv i)
                                     (if x 0 8))))))
        ((= i (bytevector-length bv))
         (bitwise-and #xffff
                      (+ acc (bitwise-and
                              (bitwise-arithmetic-shift-right acc 16)
                              #xffff))))))

  (define (put-dnskey-data p flags protocol algorithm key)
    (put-bytevector p (pack "!SCC" flags protocol algorithm))
    (put-bytevector p key))

  (define-dns-record-type DNSKEY
    (fields flags protocol algorithm key key-tag)
    (parser
     (lambda (bv start end)
       (let-values (((flags protocol algorithm) (unpack "!uSCC" bv start)))
         (let ((key (subbytevector bv (+ start (format-size "!uSCC")) end)))
           (list flags protocol algorithm key))))
     'TODO-DNSKEY)
    (formatter
     (lambda (p table offset rec)
       (put-dnskey-data p (dns-resource/DNSKEY-flags rec)
                        (dns-resource/DNSKEY-protocol rec)
                        (dns-resource/DNSKEY-algorithm rec)
                        (dns-resource/DNSKEY-key rec)))
     (lambda (p split-size rec)
       (for-each (cut display <> p)
                 (list
                  (dns-resource/DNSKEY-flags rec) " "
                  (dns-resource/DNSKEY-protocol rec) " "
                  (integer->dnssec-algorithm
                   (dns-resource/DNSKEY-algorithm rec)) " "))
       (print-base64 (dns-resource/DNSKEY-key rec) split-size p)
       (display " ;" p)
       (for-each
        (lambda (flag name)
          (when (= flag (fxand flag (dns-resource/DNSKEY-flags rec)))
            (display name p)
            (display #\space p)))
        (list dnskey-flag-zone dnskey-flag-revoke dnskey-flag-sep)
        '("ZONE" "REVOKE" "SEP"))
       (display "ID=" p)
       (display (dns-resource/DNSKEY-key-tag rec) p)))
    (protocol                           ;computes key-tag
     (lambda (p*)
       (lambda (name ttl class flags protocol algorithm key)
         (p* flags protocol algorithm key
             (if (= algorithm (dnssec-algorithm RSAMD5))
                 (unpack "!uS" key (- (bytevector-length key) 3))
                 (key-tag
                  (call-with-bytevector-output-port
                    (cut put-dnskey-data <> flags protocol algorithm key)))))))))

  (define-dns-record-type TSIG
    (fields algorithm time fudge mac original-id error other-data)
    (parser
     (lambda (bv start end)
       (let-values (((algorithm aend) (parse-labels bv start)))
         (let ((p (open-bytevector-input-port
                   (subbytevector bv aend end))))
           (let*-values (((time1 time2 fudge mac-len) (get-unpack p "!uSLSS"))
                         ((mac) (get-bytevector-n p mac-len))
                         ((original-id error other-len) (get-unpack p "!uSSS"))
                         ((other-data) (get-bytevector-n p other-len)))
             (list algorithm
                   (bitwise-ior (bitwise-arithmetic-shift-left time1 32)
                                time2)
                   fudge mac original-id error
                   (if (eof-object? other-data) #vu8() other-data))))))
     #f)
    (formatter
     (lambda (p table offset rec)
       (put-labels p (dns-resource/TSIG-algorithm rec))
       (let ((time (dns-resource/TSIG-time rec))
             (fudge (dns-resource/TSIG-fudge rec))
             (mac (dns-resource/TSIG-mac rec))
             (original-id (dns-resource/TSIG-original-id rec))
             (error (dns-resource/TSIG-error rec))
             (other (dns-resource/TSIG-other-data rec)))
         (put-bytevector p (pack "!uSLSS"
                                 (bitwise-bit-field time 32 48)
                                 (bitwise-bit-field time 0 32)
                                 fudge (bytevector-length mac)))
         (put-bytevector p mac)
         (put-bytevector p (pack "!uSSS" original-id error
                                 (bytevector-length other)))
         (put-bytevector p other)))
     (lambda (p split-size rec)
       ;; This isn't something that should be in a master file, but
       ;; it's nice to be able to read it.
       (let ((mac (dns-resource/TSIG-mac rec))
             (other (dns-resource/TSIG-other-data rec)))
         (print-labels (dns-resource/TSIG-algorithm rec) " " p)
         (for-each (cut display <> p)
                   (list
                    (dns-resource/TSIG-time rec) " "
                    (dns-resource/TSIG-fudge rec) " "
                    (bytevector-length mac) " "))
         (print-base64 mac split-size p)
         (for-each (cut display <> p)
                   (list
                    " " (dns-resource/TSIG-original-id rec) " "
                    (integer->dns-tsig-error (dns-resource/TSIG-error rec)) " "
                    (bytevector-length other) " "))
         (print-base64 other split-size p))))))
