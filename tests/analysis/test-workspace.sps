#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme) 
    (srfi :64 testing) 
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system library-node)

    (scheme-langserver analysis package-manager akku)
    (scheme-langserver analysis package-manager txt-filter)
    (scheme-langserver analysis workspace)
    (scheme-langserver analysis tokenizer)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis identifier rules library-import)
    (scheme-langserver analysis util)

    (only (srfi :13 strings) string-contains))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existing tests (preserved)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-virtual-file-system")
    (test-equal "scheme-langserver.sls" 
        (find (lambda(n) (equal? n "scheme-langserver.sls")) 
        (map file-node-name 
            (file-node-children (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))))))
(test-end)

(test-begin "init-index-node")
    (test-equal 'library 
        (annotation-stripped 
            (car 
                (annotation-expression 
                (index-node-datum/annotations
                    (init-index-node '() (car (source-file->annotations "./util/io.sls"))))))))
(test-end)

(test-begin "init-library-node")
    (let* ( [root-file-node (init-virtual-file-system "./util/" '() (generate-akku-acceptable-file-filter (string-append "./util" "/.akku/list")))]
            [root-library-node (init-library-node root-file-node)])
        (test-equal 'scheme-langserver (library-node-name (car (library-node-children root-library-node)))))
(test-end)

(test-begin "refresh-workspace-for+update-file-node-with-tail test")
    (let* ([workspace (init-workspace (string-append (current-directory) "/util/"))]
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/util/cartesian-product.sls"))])
        (update-file-node-with-tail 
            workspace
            target-file-node 
            "(library (scheme-langserver util cartesian-product1)\n  (export cartesian-product)\n  (import (rnrs))\n(define (cartesian-product . lists)\n  (fold-right \n    (lambda (xs ys)\n      (apply append \n        (map (lambda (x)\n          (map (lambda (y)\n            (cons x y))\n            ys))\n        xs)))\n    '(())\n    lists))\n)\n)"
            )
        (refresh-workspace-for workspace target-file-node)
        (test-equal #f (null? (walk-library '(scheme-langserver util cartesian-product1) root-library-node))))
(test-end)

(test-begin "library-import-process")
    (let* ( [workspace (init-workspace (current-directory) #f #f)]  
            [root-file-node (workspace-file-node workspace)]
            [root-library-node (workspace-library-node workspace)]
            [target-file-node (walk-file root-file-node (string-append (current-directory) "/run.ss"))]
            [document (file-node-document target-file-node)])
        (test-equal 
            'init-server
            (find 
                (lambda (identifier) (equal? identifier 'init-server))
                (map identifier-reference-identifier (document-ordered-reference-list document)))))
(test-end)

(test-begin "init-workspace-basic-test-r7rs")
(let* ([workspace (init-workspace (string-append (current-directory) "/tests/resources/r7rs") 'txt 'r7rs #f #f)]
        [root-file-node (workspace-file-node workspace)]
        [root-library-node (workspace-library-node workspace)])
    (test-equal #f (null? root-file-node))
    (test-equal #f (null? root-library-node)))
(test-end)

(test-begin "init-workspace-basic-test-s7")
(let* ([workspace (init-workspace (string-append (current-directory) "/tests/resources/r7rs") 'txt 's7 #f #f)]
        [root-file-node (workspace-file-node workspace)]
        [root-library-node (workspace-library-node workspace)])
    (test-equal #f (null? root-file-node))
    (test-equal #f (null? root-library-node)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. refresh-workspace resets undiagnosed-paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "refresh-workspace resets undiagnosed-paths")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [initial-paths (workspace-undiagnosed-paths workspace)])
    ;; After init, undiagnosed-paths should contain the fixture files
    (test-equal #f (null? initial-paths))
    ;; Clear undiagnosed-paths manually to simulate consumption
    (workspace-undiagnosed-paths-set! workspace '())
    (test-equal '() (workspace-undiagnosed-paths workspace))
    ;; Full refresh should repopulate
    (refresh-workspace workspace)
    (test-equal #f (null? (workspace-undiagnosed-paths workspace)))
    ;; Should contain the math file
    (test-equal #f (null? (find (lambda (p) (string-contains "/math.scm.txt" p)) (workspace-undiagnosed-paths workspace)))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8. attach-new-file tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "attach-new-file existing node")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [existing-path (string-append fixture "/math.scm.txt")]
         [result (attach-new-file existing-path root (workspace-facet workspace) 'r6rs)])
    (test-equal #f (null? result))
    (test-equal existing-path (file-node-path result)))
(test-end)

(test-begin "attach-new-file filtered path")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [bad-path (string-append fixture "/readme.md")]
         [result (attach-new-file bad-path root (workspace-facet workspace) 'r6rs)])
    (test-equal '() result))
(test-end)

(test-begin "attach-new-file nonexistent path")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [missing-path (string-append fixture "/does-not-exist.scm.txt")]
         [result (attach-new-file missing-path root (workspace-facet workspace) 'r6rs)])
    (test-equal '() result))
(test-end)

(test-begin "attach-new-file deeper path")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [deeper-path (string-append fixture "/sub/deeper.scm.txt")])
    ;; Create the deeper file temporarily (sub/ already exists)
    (with-output-to-file deeper-path (lambda () (display "(display 1)")))
    (let ([result (attach-new-file deeper-path root (workspace-facet workspace) 'r6rs)])
      (test-equal #f (null? result))
      (test-equal deeper-path (file-node-path result))
      ;; Clean up
      (delete-file deeper-path))
    ;; After deletion, attach again should return '() because file does not exist
    (let ([result2 (attach-new-file deeper-path root (workspace-facet workspace) 'r6rs)])
      (test-equal '() result2)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. init-document boundary cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-document empty file")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/empty-project")]
         [empty-path (string-append fixture "/empty.scm.txt")])
    (with-output-to-file empty-path (lambda () (display "")))
    (let ([doc (init-document empty-path 'r6rs)])
      (test-equal #f (null? doc))
      (test-equal "" (document-text doc))
      (test-equal '() (document-index-node-list doc))
      (delete-file empty-path)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10. Plain text edit incremental update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "update-file-node-with-tail plain edit")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [target-file-node (walk-file root (string-append fixture "/math.scm.txt"))]
         [orig-text (document-text (file-node-document target-file-node))]
         [new-text "(library (fixtures simple-lib math)\n  (export add mul)\n  (import (rnrs))\n  (define (add a b) (+ a b))\n  (define (mul a b) (* a b)))\n"])
    ;; Plain body edit: library identifiers unchanged
    (update-file-node-with-tail workspace target-file-node new-text)
    ;; Document text should be updated
    (test-equal new-text (document-text (file-node-document target-file-node)))
    ;; The document itself should now be refreshable (via get-reference-path-to)
    (test-equal #t (document-refreshable? (file-node-document target-file-node)))
    (refresh-workspace-for workspace target-file-node)
    ;; After refresh, should no longer be refreshable
    (test-equal #f (document-refreshable? (file-node-document target-file-node))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Script file (no library) incremental refresh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "refresh-workspace-for script file updates undiagnosed-paths")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/script-only")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [script-node (walk-file root (string-append fixture "/hello.scm.txt"))])
    ;; Script files have no library identifiers
    (test-equal '() (get-library-identifiers-list (file-node-document script-node) 'r6rs))
    ;; Clear undiagnosed-paths
    (workspace-undiagnosed-paths-set! workspace '())
    (test-equal '() (workspace-undiagnosed-paths workspace))
    ;; Edit and refresh
    (update-file-node-with-tail workspace script-node "(display \"world\")\n")
    (refresh-workspace-for workspace script-node)
    ;; Should have appended the script path
    (test-equal #f (null? (workspace-undiagnosed-paths workspace)))
    (test-equal #f (null? (find (lambda (p) (string-contains "/hello.scm.txt" p)) (workspace-undiagnosed-paths workspace)))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 12. Empty directory / all-filtered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-workspace empty directory")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/empty-project")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)])
    (test-equal #f (null? root))
    (test-equal fixture (file-node-path root))
    (test-equal '() (file-node-children root)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 13. Library header removal and addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "update-file-node-with-tail removes library header")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [lib-node (walk-file root (string-append fixture "/math.scm.txt"))]
         [old-id '(fixtures simple-lib math)])
    ;; Verify library exists before edit
    (test-equal #f (null? (walk-library old-id (workspace-library-node workspace))))
    ;; Remove library header
    (update-file-node-with-tail workspace lib-node "(define x 1)\n")
    (refresh-workspace-for workspace lib-node)
    ;; Library node should be gone
    (test-equal '() (walk-library old-id (workspace-library-node workspace))))
(test-end)

(test-begin "update-file-node-with-tail adds library header")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/script-only")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [script-node (walk-file root (string-append fixture "/hello.scm.txt"))])
    ;; Script files have no real library identifier
    (test-equal '() (get-library-identifiers-list (file-node-document script-node) 'r6rs))
    ;; Add a library header
    (update-file-node-with-tail workspace script-node
      "(library (fixtures script-only hello)\n  (export greet)\n  (import (rnrs))\n  (define (greet) 'hi))\n")
    (refresh-workspace-for workspace script-node)
    ;; Library should now exist
    (test-equal #f (null? (walk-library '(fixtures script-only hello) (workspace-library-node workspace)))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 14. Threaded mode basic test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-workspace threaded mode")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
         [workspace (init-workspace fixture 'txt 'r6rs #t #f)]
         [root (workspace-file-node workspace)]
         [linkage (workspace-file-linkage workspace)])
    ;; Mutex should exist
    (test-equal #f (null? (workspace-mutex workspace)))
    ;; File linkage should be built
    (test-equal #f (null? linkage))
    ;; References should have been analysed
    (test-equal #f (null? root)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 15. Structural assertions on workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "workspace file-node parent chain")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [math-node (walk-file root (string-append fixture "/math.scm.txt"))])
    (test-equal #f (null? math-node))
    (test-equal root (file-node-parent math-node))
    (test-equal "math.scm.txt" (file-node-name math-node))
    (test-equal #f (file-node-folder? math-node)))
(test-end)

(test-begin "workspace document refreshable lifecycle")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root (workspace-file-node workspace)]
         [math-node (walk-file root (string-append fixture "/math.scm.txt"))]
         [doc (file-node-document math-node)])
    ;; After init + analysis, document should not be refreshable
    (test-equal #f (document-refreshable? doc))
    ;; After text mutation, should become refreshable
    (update-file-node-with-tail workspace math-node "(library (fixtures simple-lib math)\n  (export add)\n  (import (rnrs))\n  (define (add a b) (+ a b)))\n")
    (test-equal #t (document-refreshable? doc))
    ;; After refresh, should be back to not refreshable
    (refresh-workspace-for workspace math-node)
    (test-equal #f (document-refreshable? doc)))
(test-end)

(test-begin "workspace library-node tree structure")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [root-lib (workspace-library-node workspace)])
    ;; Root library node should have 'fixtures as first child
    (test-equal 'fixtures (library-node-name (car (library-node-children root-lib))))
    ;; Two-libs should have two library nodes: helper and main
    (let ([two-libs-node (walk-library '(fixtures two-libs) root-lib)])
      (test-equal #f (null? two-libs-node))
      (test-equal 2 (length (library-node-children two-libs-node)))))
(test-end)

(test-begin "workspace file-linkage non-empty")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
         [linkage (workspace-file-linkage workspace)])
    (test-equal #f (null? linkage))
    ;; There should be paths in the linkage maps
    (test-equal #f (null? (file-linkage-path->id-map linkage)))
    (test-equal #f (null? (file-linkage-id->path-map linkage))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 16. Type inference mode basic test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-workspace with type-inference")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
         [workspace (init-workspace fixture 'txt 'r6rs #f #t)]
         [root (workspace-file-node workspace)])
    (test-equal #f (null? root))
    (test-equal #t (workspace-type-inference? workspace)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
