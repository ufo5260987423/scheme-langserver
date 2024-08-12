;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2018, 2022 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Distinguished Encoding Rules (DER) for ASN.1

;; http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf

(library (industria der)
  (export
    decode translate
    encode
    data-type data-start-index data-length data-value
    ;; Re-exported for backward compatibility with version 2.0.0
    bit-string-length bit-string-unused bit-string->bytevector
    bit-string->integer bit-string-bit-set?)
  (import
    (rnrs (6))
    (only (srfi :1 lists) take)
    (srfi :19 time)
    (srfi :26 cut)
    (industria bit-strings)
    (industria bytevectors))

(define-syntax print
  (syntax-rules ()
    #;
    ((_ . args)
     (begin
       (for-each display (list . args))
       (newline)))
    ((_ . args) (values))))

(define latin-1-transcoder
  (make-transcoder (latin-1-codec)
                   (eol-style none)
                   (error-handling-mode raise)))

;;; Convert DER-encoded data from bytevectors to parse trees

(define (get-type bv start end)
  (let* ((t (bytevector-u8-ref bv start))
         (class (vector-ref '#(universal application context private)
                            (fxbit-field t 6 8)))
         (constructed (fxbit-set? t 5))
         (number (fxbit-field t 0 5)))
    (if (fx=? number #b11111)
        (let lp ((i (+ start 1)) (number 0))
          (unless (< i end)
            (error 'get-type "Unexpected end of data in type"))
          (let* ((v (bytevector-u8-ref bv i))
                 (number (bitwise-ior (bitwise-arithmetic-shift-left number 7)
                                      (fxand v #x7f))))
            (if (fxbit-set? v 7)
                (lp (+ i 1) number)
                (values constructed class number (+ i 1)))))
        (values constructed class number (+ start 1)))))

(define (get-length bv start end)
  (let ((b1 (bytevector-u8-ref bv start)))
    (cond ((fx=? b1 #b10000000)       ;indefinite form
           (error 'get-length "Indefinite length encountered in DER coding"))
          ((fxbit-set? b1 7)          ;long form
           (let ((extra (fxbit-field b1 0 7)))
             (unless (< (+ start extra 1) end)
               (error 'get-length "Unexpected end of data in length"))
             (let ((len (bytevector-uint-ref bv (+ start 1) (endianness big)
                                             extra)))
               (when (< len 128)
                 (error 'get-length "Unnecessarily long length in DER coding" len))
               (values len (+ start 1 extra)))))
          (else                       ;short form
           (values b1 (+ start 1))))))

(define (get-boolean bv start length)
  (unless (= length 1)
    (error 'get-boolean "Boolean of illegal length"))
  (let ((v (bytevector-u8-ref bv start)))
    (cond ((= v #xff) #t)
          ((= v #x00) #f)
          (else
           (error 'get-boolean
                  "Invalid boolean in DER data" v)))))

(define (get-integer bv start length)
  (when (and (> length 1)
             (memv (fxior (fxarithmetic-shift-left (bytevector-u8-ref bv start) 1)
                          (fxarithmetic-shift-right (bytevector-u8-ref bv (+ start 1))
                                                    7))
                   '(#x000 #x1FF)))
    (error 'get-integer "Unnecessarily long integer in DER data"
           (bytevector-u8-ref bv start) length))
  (bytevector-sint-ref bv start (endianness big) length))

(define (get-T61String bv start length)
  ;; T.61 TeletexString. The world's most complicated character
  ;; encoding ever designed by committee, but according to RFC5280
  ;; everyone treats this as ISO-8859-1. Support is optional...
  (bytevector->string (subbytevector bv start (+ start length))
                      latin-1-transcoder))

(define (get-IA5String bv start length)
  ;; T.50: The International Reference Alphabet No. 5. This used to
  ;; be weird, but they changed it in 1992 so it can be interpreted
  ;; precisely like US-ASCII.
  (let ((buf (subbytevector bv start (+ start length))))
    (bytevector-for-each
     (lambda (b)
       (unless (fx<? b #x80)
         (error 'get-IA5String "Invalid char in ia5-string" b buf)))
     buf)
    (bytevector->string buf latin-1-transcoder)))

(define (printable-character? c)
  (or (char<=? #\0 c #\9)
      (char<=? #\a c #\z)
      (char<=? #\A c #\Z)
      (memv c '(#\space #\' #\( #\) #\+ #\,
                #\- #\. #\/ #\: #\= #\?
                ;; not really permitted:
                #\*))))

(define (get-PrintableString bv start length)
  (let ((buf (subbytevector bv start (+ start length))))
    (let ((ret (bytevector->string buf latin-1-transcoder)))
      (string-for-each
       (lambda (c)
         (unless (printable-character? c)
           (error 'get-PrintableString "Invalid char in printable-string" c ret)))
       ret)
      ret)))

(define (get-UTF8String bv start length)
  (utf8->string (subbytevector bv start (+ start length))))

(define (get-UniversalString bv start length)
  (utf16->string (subbytevector bv start (+ start length))
                 (endianness big)))  ;FIXME: verify this

(define (get-GraphicString bv start length)
  ;; TODO: this is not really UTF-8
  (utf8->string (subbytevector bv start (+ start length))))

(define (get-VisibleString bv start length)
  ;; TODO: this is not really UTF-8
  (utf8->string (subbytevector bv start (+ start length))))

(define (get-GeneralString bv start length)
  ;; TODO: this is not really UTF-8
  (utf8->string (subbytevector bv start (+ start length))))

(define (get-octet-string bv start length)
  (subbytevector bv start (+ start length)))

(define (get-bit-string bv start length)
  ;; This is an arbitrary string of bits, which is not exactly the
  ;; same thing as a string of octets or an integer. The length
  ;; doesn't have to divide eight. The first byte specifies how many
  ;; bits at the end are unused.
  (if (= length 1)
      (integer->bit-string 0 0)
      (let ((unused (bytevector-u8-ref bv start)))
        (when (> unused 7)
          (error 'get-bit-string "Too many trailing bits in bit-string"))
        (make-bit-string (- (* (- length 1) 8) unused)
                         (get-octet-string bv (+ start 1) (- length 1))))))

(define (get-UTCTime bv start length)
  ;; YYMMDDHHMMSSZ
  (string->date (string-append (get-IA5String bv start length)
                               "+0000")
                "~y~m~d~H~M~SZ~z"))

(define (get-GeneralizedTime bv start length)
  ;; YYYYMMDDhhmmssZ  YYYYMMDDhhmmss.s+Z
  (define (parse time)
    ;; FIXME: check for invalid encodings
    (define (time-fraction time)
      ;; Returns the fraction. SRFI-19 has nanosecond precision, so at
      ;; most 9 decimals are useful.
      (substring time (string-length "YYYYMMDDhhmmss.")
                 (min (string-length "YYYYMMDDhhmmss.123456789")
                      (- (string-length time) 1))))
    (define (fraction->nanoseconds frac)
      (* (string->number frac 10)
         (expt 10 (- 9 (string-length frac)))))
    (let ((nsec                         ;nanoseconds
           (if (= (string-length time)
                  (string-length "YYYYMMDDhhmmssZ"))
               0
               (fraction->nanoseconds (time-fraction time))))
          ;; date without fractional seconds
          (d (string->date (string-append (substring time 0 (string-length
                                                             "YYYYMMDDhhmmss"))
                                          "Z+0000")
                           "~Y~m~d~H~M~SZ~z")))
      ;; add the nanoseconds
      (time-utc->date (add-duration (date->time-utc d)
                                    (make-time 'time-duration nsec 0))
                      0)))
  (parse (get-IA5String bv start length)))

(define (get-oid bv start length relative?)
  (define (get i)
    (if (= i (+ start length))
        '()
        (let lp ((i i) (number 0))
          (unless (< i (+ start length))
            (error 'decode "Unexpected end of data in object id"))
          (let* ((v (bytevector-u8-ref bv i))
                 (number (bitwise-ior (bitwise-arithmetic-shift-left number 7)
                                      (fxand v #x7f))))
            (if (fxbit-set? v 7)
                (lp (+ i 1) number)
                (cons number (get (+ i 1))))))))
  (define (solve-oid p)
    ;; Solve for X,Y in p=(X*40)+Y, X=0..2. For X in (0,1): Y=0..39.
    (cond ((< p 40) (values 0 p))
          ((< p 80) (values 1 (- p 40)))
          (else     (values 2 (- p 80)))))
  (let ((subids (get start)))
    (cond ((null? subids)
           (error 'decode "Empty object id"))
          (relative?
           subids)
          (else
           (let-values (((x y) (solve-oid (car subids))))
             (unless (= (car subids) (+ (* 40 x) y))
               (error 'decode "Invalid first subidentifier in object id"
                      (car subids)))
             (cons x (cons y (cdr subids))))))))

(define (get-sequence/set bv start length)
  (let ((end (+ start length)))
    (let lp ((start start)
             (ret '()))
      (if (= start end)
          (reverse ret)
          (let-values (((start* value) (get-value bv start end)))
            (lp start* (cons value ret)))))))

;; These are the names you need to use in your type definitions.
(define universal-types
  `#((reserved #f #f)                 ;end of contents marker, not used in DER
     (boolean ,get-boolean #f)
     (integer ,get-integer #f)
     (bit-string ,get-bit-string #f)
     (octet-string ,get-octet-string #f)
     (null ,(lambda (bv start length) #f) #f)
     (object-identifier ,(lambda (bv start length) (get-oid bv start length #f)) #f)
     (object-descriptor #f #f)
     (external #f #f)
     (real #f #f)
     (enumerated #f #f)
     (embedded-pdv #f #f)
     (utf8-string ,get-UTF8String #f)
     (relative-oid ,(lambda (bv start length) (get-oid bv start length #t)) #f)
     (reserved #f #f)
     (reserved #f #f)
     (sequence #f ,get-sequence/set)
     (set #f ,get-sequence/set)
     (numeric-string #f #f)
     (printable-string ,get-PrintableString #f)
     (t61-string ,get-T61String #f)
     (videotex-string #f #f)
     (ia5-string ,get-IA5String #f)
     (utc-time ,get-UTCTime #f)
     (generalized-time ,get-GeneralizedTime #f)
     (graphic-string ,get-GraphicString #f)
     (visible-string ,get-VisibleString #f)
     (general-string ,get-GeneralString #f)
     (universal-string ,get-UniversalString #f)
     (character-string #f #f)
     (bmp-string ,get-UniversalString #f)
     (reserved #f #f)))                       ;used in get-type

(define (get-value bv start* end*)
  ;; DER uses a Tag-Length-Value encoding. This procedure
  ;; recursively parses the value at index `start*'.
  (let*-values (((constructed class number startl) (get-type bv start* end*))
                ((len start) (get-length bv startl end*))
                ((end) (+ start len)))
    (unless (<= end end*)
      (error 'decode "Unexpected end of data in value" start* end* len))
    (if (eq? class 'universal)
        (let* ((decoder (vector-ref universal-types number))
               (offset (if constructed 2 1))
               (type-name (car decoder)))
          (print ";" type-name)
          (cond ((list-ref decoder offset) =>
                 (lambda (dec)
                   (values end (list type-name
                                     start* (- end start*)
                                     (dec bv start len)))))
                (else
                 (error 'decode "Unimplemented universal data type"
                        type-name constructed))))
        ;; Delay parsing of non-universal types.
        (values end (list (list (if constructed 'cons 'prim) class number)
                          start* (- end start*)
                          (if constructed
                              (get-sequence/set bv start len)
                              (subbytevector bv start end)))))))

(define decode
  (case-lambda
    ((bv start end)
     (let-values (((len value) (get-value bv start end)))
       value))
    ((bv)
     (let-values (((len value) (get-value bv 0 (bytevector-length bv))))
       value))))

(define (decode-implicit type value)
  ;; Given a universal type name, decode a previously implicitly coded value.
  (let lp ((i 0))
    (cond ((= i (vector-length universal-types))
           (error 'get-decoder "No decoder for this type" type))
          ((eq? (car (vector-ref universal-types i)) type)
           ((cadr (vector-ref universal-types i))
            value 0 (bytevector-length value)))
          (else (lp (+ i 1))))))

;;; These procedures take a parse tree and an ASN.1 type and mixes the
;;; two together, taking care of naming fields and handling default
;;; values etc.

;; field type example: (implicit context 2 ia5-string)
(define field-name car)
(define field-type cadr)
(define field-opts cddr)

(define field-type-implicit? (lambda (x) (eq? (car x) 'implicit)))
(define field-type-explicit? (lambda (x) (eq? (car x) 'explicit)))
(define field-type-class cadr)
(define field-type-number caddr)

(define data-type car)
(define data-start-index cadr)
(define data-length caddr)
(define data-value cadddr)

;; data type example: (prim context 2)
(define data-type-delayed? pair?)
(define data-type-constructed? (lambda (x) (eq? (car x) 'cons)))
(define data-type-primitive? (lambda (x) (eq? (car x) 'prim)))
(define data-type-class cadr)
(define data-type-number caddr)

(define (find-choice choices type)
  ;; Returns (field-name field-type ...) or #f if there's no match.
  (find
   (lambda (choice)
     (let ((ftype (field-type choice)))
       (or (eq? ftype type)
           (and (data-type-delayed? ftype)
                ;; FIXME: what about explicit?
                (and (field-type-implicit? ftype) (data-type-primitive? type))
                (eq? (field-type-class ftype) (data-type-class type))
                (eqv? (field-type-number ftype) (data-type-number type))))))
   choices))

;; TODO: Clean this up.

(define (translate-sequence data type format-field)
  (case (car type)
    ((integer)
     ;; An integer with named values
     (unless (eq? (car data) 'integer)
       (error 'translate "expected integer"))
     (let ((names (map (lambda (x) (cons (cdr x) (car x))) (cadr type))))
       (cond ((assv (cadddr data) names) => cdr)
             (else (error 'translate "bad named integer maybe?"
                          (data-value data))))))

    ((choice)
     (let ((f (find-choice (cdr type) (data-type data)))
           (d data))
       (unless f
         (error 'translate "no right choice" type data))
       (print "#;TAKEN-CHOICE " f)
       (print "#;WITH-DATA " d)
       (cond ((and (list? (field-type f)) (list? (data-type d))
                   (eq? 'implicit (car (field-type f)))
                   (eq? 'prim (car (data-type d)))
                   (eq? (cadr (field-type f)) (cadr (data-type d)))
                   (eq? (caddr (field-type f)) (caddr (data-type d))))
              (let ((implicit-type (list-ref (field-type f) 3)))
                (format-field (field-name f)
                              (field-type f)
                              (decode-implicit implicit-type (data-value data))
                              (data-start-index data) (data-length data))))

             ((and (list? (field-type f)) (list? (data-type d))
                   (eq? 'explicit (car (field-type f)))
                   (eq? 'cons (car (data-type d)))
                   (eq? (cadr (field-type f)) (cadr (data-type d)))
                   (eq? (caddr (field-type f)) (caddr (data-type d))))
              (let ((explicit-type (list-ref (field-type f) 3)))
                (format-field (field-name f)
                              (cadddr (field-type f))
                              (translate (data-value d)
                                         (cadddr (field-type f))
                                         format-field)
                              (data-start-index d)
                              (data-length d))))

             (else
              (format-field (field-name f)
                            (field-type f)
                            (translate data (field-type f) format-field)
                            (data-start-index data) (data-length data))))))

    ((sequence-of set-of)
     (unless (or (and (eq? (car type) 'sequence-of) (eq? (car data) 'sequence))
                 (and (eq? (car type) 'set-of) (eq? (car data) 'set)))
       (error 'translate "expected set/sequence" (car data)))
     (unless (<= (cadr type) (length data) (caddr type))
       (error 'translate "too little data for set"))
     (map (cut translate <> (cadddr type)) (data-value data)))

    ((sequence set)
     (print ";SEQUENCE/SET")
     (unless (eq? (car type) (car data))
       (error 'translate "expected set/sequence" type (car data)))
     (let lp ((fields (cdr type))
              (data (data-value data))
              (ret '()))
       (print "---")
       (print "These are the fields: " (length fields) " " fields)
       (print "And this is the data: " data)

       (cond ((null? fields)
              (reverse ret))
             ((and (null? data) (assoc 'default (field-opts (car fields)))) =>
              (lambda (default)
                ;; There's no more data, but here is a field with a default
                (print ";using default")
                (lp (cdr fields)
                    data
                    (cons (format-field (field-name (car fields))
                                        (field-type (car fields))
                                        (cadr default)
                                        #f #f)
                          ret))))
             ((null? data)
              (error 'translate "non-optional data missing" fields))
             (else
              (let ((f (car fields))
                    (d (car data)))
                (print ";Field:\n  " (field-name f) ", " (field-type f) ", "
                       (field-opts f))
                (print ";data1:\n  " (data-type d) ", " (data-value d))
                (cond ((and (list? (field-type f)) (list? (data-type d))
                            (eq? 'explicit (car (field-type f)))
                            (eq? 'cons (car (data-type d)))
                            (eq? (cadr (field-type f)) (cadr (data-type d)))
                            (eq? (caddr (field-type f)) (caddr (data-type d))))
                       ;; (field-type f): (explicit context number type)
                       ;; (data-type d): (explicit context number)
                       (lp (cdr fields)
                           (cdr data)
                           (cons (format-field (field-name f)
                                               (cadddr (field-type f))
                                               (translate (car (data-value d))
                                                          (cadddr (field-type f))
                                                          format-field)
                                               (data-start-index d)
                                               (data-length d))
                                 ret)))

                      ;; CHOICE
                      ((and (list? (field-type f))
                            (eq? (car (field-type f)) 'choice)
                            (find-choice (cdr (field-type f))
                                         (data-type d)))
                       =>
                       (lambda (choice)
                         (lp (cdr fields)
                             (cdr data)
                             (cons (format-field (car choice)
                                                 (cadr choice)
                                                 (translate d (cadr choice)
                                                            format-field)
                                                 (data-start-index d)
                                                 (data-length d))
                                   ret))))

                      ;; FIXME: implicit
                      ((and (list? (field-type f)) (list? (data-type d))
                            (eq? 'implicit (car (field-type f)))
                            (eq? 'prim (car (data-type d)))
                            (eq? (cadr (field-type f)) (cadr (data-type d)))
                            (eq? (caddr (field-type f)) (caddr (data-type d))))
                       ;; (field-type f): (implicit context number type)
                       ;; (data-type d): (prim context number)
                       (error 'translate "implicit data handling not implemented"
                              f d))

                      ((or (and (list? (field-type f))
                                (not (memq (car (field-type f))
                                           '(implicit explicit choice))))
                           (eq? (data-type d) (field-type f))
                           (eq? (field-type f) 'ANY))
                       ;; The field type matches the data type
                       (lp (cdr fields)
                           (cdr data)
                           (cons (format-field (field-name f)
                                               (field-type f)
                                               (translate (car data) (field-type f)
                                                          format-field)
                                               (data-start-index (car data))
                                               (data-length (car data)))
                                 ret)))

                      ((assoc 'default (field-opts f)) =>
                       (lambda (default)
                         (print ";using default")
                         (lp (cdr fields)
                             data
                             (cons (format-field (field-name f)
                                                 (field-type f)
                                                 (cadr default)
                                                 #f #f)
                                   ret))))
                      (else
                       (error 'translate
                              "unexpected data" (caar data) (field-type f)))))))))
    (else

     (error 'translate "error in ASN.1 type" data type))))

(define translate
  (case-lambda
    ((data type)
     (translate data type (lambda (name type value start len) value)))
    ((data type format-field)
     (print "; Translate:")
     (print "#;data: " data)
     (print "#;type: " type "\n")
     (cond ((list? type)
            (translate-sequence data type format-field))
           ((eq? 'ANY type)
            ;; Delay interpretation. Usually the ANY type is
            ;; combined with an OID which decides the type.
            data)
           ((eq? type (data-type data))
            (data-value data))
           (else
            (error 'translate "unexpected type" type (car data)))))))

;;; Encode in DER

;; This uses the same data representation as the DER decoder, but
;; data-start-index and data-length are ignored.

(define *class-universal*   #b00000000)
(define *class-application* #b01000000)
(define *class-context*     #b10000000)
(define *class-private*     #b11000000)
(define *type-constructed*  #b00100000)
(define *tag-extended*      #b00011111)

(define (encode-type flags tag byte*)
  (if (fx>=? tag 31)
      (cons (fxior flags *tag-extended*)
            (let lp ((tag tag) (ret (cons (bitwise-and tag #x7f) byte*)))
              (let ((tag (bitwise-arithmetic-shift-right tag 7)))
                (if (fxzero? tag)
                    ret
                    (let ((byte (fxior #x80 (bitwise-and tag #x7f))))
                      (lp tag (cons byte ret)))))))
      (cons (fxior flags tag) byte*)))

(define (encode-primitive class tag byte*)
  (encode-type class tag byte*))

(define (encode-constructed class tag byte*)
  (encode-type (fxior class *type-constructed*) tag byte*))

(define (encode-length len byte*)
  (if (< len 128)
      (cons len byte*)
      (let ((bv (uint->bytevector len)))
        (when (> (bytevector-length bv) #x7f)
          (error 'encode-length "Unencodable length" len))
        (cons (fxior #x80 (bytevector-length bv))
              (cons bv byte*)))))

(define (flat-length data)
  (fold-left (lambda (acc datum)
               (cond ((pair? datum)
                      (+ (flat-length datum) acc))
                     ((bytevector? datum)
                      (+ (bytevector-length datum) acc))
                     (else
                      (assert (fx<=? 0 datum 255))
                      (+ 1 acc))))
             0
             data))

(define (encode* data)
  (let ((type (data-type data)) (value (data-value data)))
    (case type
      ((boolean)
       (encode-primitive *class-universal* 1
                         (encode-length 1 (if value '(#xFF) '(#x00)))))

      ((integer)
       (let ((bv (if (zero? value) #vu8(0) (sint->bytevector value))))
         (encode-primitive *class-universal* 2
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((bit-string)
       (let ((bv (bit-string->bytevector value)))
         (encode-primitive *class-universal* 3
                           (encode-length (+ 1 (bytevector-length bv))
                                          (list (bit-string-unused value) bv)))))

      ((octet-string)
       (encode-primitive *class-universal* 4
                         (encode-length (bytevector-length value) (list value))))

      ((null)
       (encode-primitive *class-universal* 5
                         (encode-length 0 '())))

      ((object-identifier relative-oid)
       (let ((bv
              (call-with-bytevector-output-port
                (lambda (p)
                  (define (put-subid v)
                    (if (= v 0)
                        (put-u8 p 0)
                        (let* ((bytes (div (+ 6 (bitwise-length v)) 7))
                               (offset (* (- bytes 1) 7)))
                          (let lp ((bytes bytes) (offset offset))
                            (unless (= bytes 0)
                              (put-u8 p (bitwise-ior
                                         (if (= bytes 1) 0 #x80) ;last?
                                         (bitwise-bit-field v offset (+ offset 7))))
                              (lp (- bytes 1) (- offset 7)))))))
                  (case (data-type data)
                    ((object-identifier)
                     (put-subid (+ (* (car value) 40) (cadr value)))
                     (for-each put-subid (cddr value)))
                    (else
                     (for-each put-subid value)))))))
         (encode-primitive *class-universal*
                           (if (eq? (data-type data) 'object-identifier) 6 13)
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((sequence set)
       (assert (list? value))
       (let ((v* (map encode* value)))
         (encode-constructed *class-universal*
                             (if (eq? (data-type data) 'sequence)
                                 16 17)
                             (encode-length (flat-length v*)
                                            v*))))

      ((utf8-string)
       (let ((bv (string->utf8 value)))
         (encode-primitive *class-universal* #x0c
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((printable-string)
       (string-for-each
        (lambda (c)
          (unless (printable-character? c)
            (error 'encode "Unencodable char in printable-string" c value)))
        value)
       (let ((bv (string->bytevector value latin-1-transcoder)))
         (encode-primitive *class-universal* #x13
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((t61-string)
       (string-for-each
        (lambda (c)
          (unless (fx<? (char->integer c) 256)
            (error 'encode "Unencodable char in t61-string" c value)))
        value)
       (let ((bv (string->bytevector value latin-1-transcoder)))
         (encode-primitive *class-universal* #x14
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((ia5-string)
       (string-for-each
        (lambda (c)
          (unless (fx<? (char->integer c) 128)
            (error 'encode "Unencodable char in ia5-string" c value)))
        value)
       (let ((bv (string->bytevector value latin-1-transcoder)))
         (encode-primitive *class-universal* #x16
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      ((visible-string)
       ;; FIXME: validate the characters
       (let ((bv (string->bytevector value latin-1-transcoder)))
         (encode-primitive *class-universal* #x1a
                           (encode-length (bytevector-length bv)
                                          (list bv)))))

      (else
       (cond
         ((data-type-delayed? type)
          (let ((class (case (data-type-class type)
                         ((application) *class-application*)
                         ((context) *class-context*)
                         ((private) *class-private*)
                         (else *class-universal*))))
            (if (data-type-constructed? type)
                (let ((v* (map encode* value)))
                  (encode-constructed class
                                      (data-type-number type)
                                      (encode-length (flat-length v*)
                                                     v*)))
                (encode-primitive class
                                  (data-type-number type)
                                  (encode-length (bytevector-length value)
                                                 (list value))))))
         (else
          (assertion-violation 'encode "Unimplemented type in DER data"
                               (data-type data))))))))

;; Encode DER data and write it to the port. If port is #f then the
;; encoded data is returned as a bytevector. TODO: Otherwise the
;; length of the written data is returned.
(define (encode port data)
  (if (not port)
      (call-with-bytevector-output-port
        (lambda (p) (encode p data) p))
      (let-values (((bytes) (encode* data)))
        (let lp ((bytes bytes))
          ;; Flatten the data into the port.
          (for-each (lambda (datum)
                      (cond ((pair? datum)
                             (lp datum))
                            ((bytevector? datum)
                             (put-bytevector port datum))
                            (else
                             (put-u8 port datum))))
                    bytes))))))
