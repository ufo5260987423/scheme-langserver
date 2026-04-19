#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis package-manager txt-filter)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis identifier reference))

(define (has-identifier? refs id)
  (not (null? (filter (lambda (r) (eq? id (identifier-reference-identifier r))) refs))))

(define (get-script-ordered-refs fixture filename)
  (let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root-file-node (workspace-file-node workspace)]
         [script-path (string-append fixture "/" filename)]
         [script-node (walk-file root-file-node script-path)]
         [doc (file-node-document script-node)])
    (document-ordered-reference-list doc)))

(define (get-library-root-refs fixture filename)
  (let* ([workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root-file-node (workspace-file-node workspace)]
         [consumer-path (string-append fixture "/" filename)]
         [consumer-node (walk-file root-file-node consumer-path)]
         [doc (file-node-document consumer-node)]
         [root-index-node (car (document-index-node-list doc))])
    (index-node-references-import-in-this-node root-index-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import-references: direct resolution from fixture lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "import-references from fixture lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root-library-node (workspace-library-node workspace)]
         [lib-path (string-append fixture "/lib.scm.txt")]
         [lib-node (walk-file (workspace-file-node workspace) lib-path)]
         [doc (file-node-document lib-node)]
         [refs (import-references doc root-library-node '(fixtures import-test lib))])
    (test-equal 3 (length refs))
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #t (has-identifier? refs 'baz)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library-import-process via library consumer fixture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "library-import-process plain")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-library-root-refs fixture "consumer-plain.scm.txt")])
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #t (has-identifier? refs 'baz)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import-process via script fixtures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "import-process plain")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-plain.scm.txt")])
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #t (has-identifier? refs 'baz)))
(test-end)

(test-begin "import-process only")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-only.scm.txt")])
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #f (has-identifier? refs 'baz)))
(test-end)

(test-begin "import-process except")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-except.scm.txt")])
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #f (has-identifier? refs 'baz)))
(test-end)

(test-begin "import-process prefix")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-prefix.scm.txt")])
    (test-equal #t (has-identifier? refs 't:foo))
    (test-equal #t (has-identifier? refs 't:bar))
    (test-equal #t (has-identifier? refs 't:baz))
    (test-equal #f (has-identifier? refs 'foo))
    ;; Verify type is pointer
    (let ([t-foo (car (filter (lambda (r) (eq? 't:foo (identifier-reference-identifier r))) refs))])
      (test-equal 'pointer (identifier-reference-type t-foo))
      (test-equal #f (null? (identifier-reference-parents t-foo)))))
(test-end)

(test-begin "import-process rename")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-rename.scm.txt")])
    (test-equal #t (has-identifier? refs 'renamed-foo))
    (test-equal #f (has-identifier? refs 'foo))
    (let ([renamed (car (filter (lambda (r) (eq? 'renamed-foo (identifier-reference-identifier r))) refs))])
      (test-equal 'pointer (identifier-reference-type renamed))
      (test-equal #f (null? (identifier-reference-parents renamed)))))
(test-end)

(test-begin "import-process alias")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-alias.scm.txt")])
    ;; Alias should expose both original and aliased names
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'aliased-foo))
    (let ([aliased (car (filter (lambda (r) (eq? 'aliased-foo (identifier-reference-identifier r))) refs))])
      (test-equal 'pointer (identifier-reference-type aliased))))
(test-end)

(test-begin "import-process for run")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/import-test")]
         [refs (get-script-ordered-refs fixture "script-for.scm.txt")])
    (test-equal #t (has-identifier? refs 'foo))
    (test-equal #t (has-identifier? refs 'bar))
    (test-equal #t (has-identifier? refs 'baz)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
