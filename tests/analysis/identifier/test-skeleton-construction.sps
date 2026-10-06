#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
#!r6rs

;; TODO-16: the annotation-carrying skeleton builder must produce trees
;; structurally IDENTICAL to the pretty-print + reparse path it
;; replaces (children structure + stripped datum at every node),
;; including the quote-abbreviation normalization ('x reprints as
;; (quote x), whose reparse DROPS the quote head from children) and
;; improper tails. Positions are allowed (and expected) to differ.

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis tokenizer))

(define fixture
  (string-append (current-directory) "/tests/resources/workspace-fixtures/match-auto-resolve"))

(define (node-shape-equal? a b)
  (and (equal? (index-node-expression a) (index-node-expression b))
    (= (length (index-node-children a)) (length (index-node-children b)))
    (let loop ((ca (index-node-children a)) (cb (index-node-children b)))
      (cond
        ((null? ca) #t)
        ((node-shape-equal? (car ca) (car cb)) (loop (cdr ca) (cdr cb)))
        (else #f)))))

(define (make-call src)
  (with-output-to-file "/tmp/sk-test.scm" (lambda () (display src)) 'replace)
  (init-index-node '() (car (source-file->annotations "/tmp/sk-test.scm"))))

(test-begin "annotation skeleton == reparse skeleton (shape)")
(let* ((ws (init-workspace fixture 'txt 'r6rs #f #f))
    (root (workspace-file-node ws))
    (lib (workspace-library-node ws))
    (doc (file-node-document (walk-file root (string-append fixture "/consumer.scm.txt"))))
    (match-ref (car (find-available-references-for doc
                     (find-index-node-recursive
                       (lambda (n) (let ((e (annotation-stripped-expression n)))
                                     (and (pair? e) (not (null? e)) (eq? 'match (car e)))))
                       (car (document-index-node-list doc)))
                     'match)))
    (def-node (identifier-reference-initialization-index-node match-ref))
    (raw-gen (index-node-expansion-generator (caddr (index-node-children def-node))))
    (src1 "(match y ((? number? q) (list q)))")
    (src2 "(match y ('a 'b) (else #f))"))
  (for-each
    (lambda (src)
      (let* ((call-node (make-call src))
          (ufo-doc (file-node-document (walk-file root (string-append fixture "/ufo-match.scm.txt")))))
        (putenv "SLSCHEME_SKELETON_REPARSE" "1")
        (let ((reparse-result (raw-gen root lib ufo-doc call-node)))
          (putenv "SLSCHEME_SKELETON_REPARSE" "")
          (let ((built-result (raw-gen root lib ufo-doc call-node)))
            (test-assert (string-append "shape equal: " src)
              (and reparse-result built-result
                (node-shape-equal? (cdr reparse-result) (cdr built-result))))))))
    (list src1 src2)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
