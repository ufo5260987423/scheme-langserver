#!r6rs
;; Akku.scm wrote this file based on "chibi-string-0.9.0/chibi/string.sld"

;;> A cursor-oriented string library.  Provides efficient string
;;> utilities for implementations with or without fast random-access
;;> strings.

(library
  (chibi string)
  (export
    string-cursor?
    string-cursor-start
    string-cursor-end
    string-cursor-ref
    string-cursor<?
    string-cursor<=?
    string-cursor>?
    string-cursor>=?
    string-cursor=?
    string-cursor-next
    string-cursor-prev
    substring-cursor
    string-cursor->index
    string-index->cursor
    string-cursor-forward
    string-cursor-back
    string-null?
    string-every
    string-any
    string-join
    string-split
    string-count
    string-trim
    string-trim-left
    string-trim-right
    string-mismatch
    string-mismatch-right
    string-prefix?
    string-suffix?
    string-find
    string-find-right
    string-find?
    string-skip
    string-skip-right
    string-fold
    string-fold-right
    string-map
    string-for-each
    string-contains
    make-string-searcher
    string-downcase-ascii
    string-upcase-ascii
    call-with-input-string
    call-with-output-string)
  (import
    (scheme base)
    (scheme char)
    (srfi :14)
    (except (srfi :1) make-list list-copy)
    (only (srfi :13) string-contains))
  (define (string-cursor->index str i) i)
  (define (string-index->cursor str i) i)
  (define string-cursor? integer?)
  (define string-cursor<? <)
  (define string-cursor>? >)
  (define string-cursor=? =)
  (define string-cursor<=? <=)
  (define string-cursor>=? >=)
  (define string-cursor-ref string-ref)
  (define (string-cursor-start s) 0)
  (define string-cursor-end string-length)
  (define (string-cursor-next s i) (+ i 1))
  (define (string-cursor-prev s i) (- i 1))
  (define (substring-cursor s start . o)
    (substring
      s
      start
      (if (pair? o) (car o) (string-length s))))
  (define (string-concatenate orig-ls . o)
    (let ((sep (if (pair? o) (car o) ""))
          (out (open-output-string)))
      (let lp ((ls orig-ls))
        (cond ((pair? ls)
               (if (and sep (not (eq? ls orig-ls)))
                 (write-string sep out))
               (write-string (car ls) out)
               (lp (cdr ls)))))
      (get-output-string out)))
  (define string-size string-length)
  (define (call-with-input-string str proc)
    (let* ((in (open-input-string str)) (res (proc in)))
      (close-input-port in)
      res))
  (define (call-with-output-string proc)
    (let ((out (open-output-string)))
      (proc out)
      (let ((res (get-output-string out)))
        (close-output-port out)
        res)))
  (include "string.scm"))
