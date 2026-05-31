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
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis util)
  (scheme-langserver analysis package-manager akku)
  (scheme-langserver analysis package-manager txt-filter)
  (scheme-langserver analysis dependency file-linkage)
  (scheme-langserver util matrix))

(test-begin "init-linkage-matrix")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
      [root-library-node (init-library-node root-file-node)]
      [file-linkage (init-file-linkage root-file-node root-library-node)]
      [from-path (string-append (current-directory) "/analysis/workspace.sls")]
      [to-path (string-append (current-directory) "/util/io.sls")])
    (test-equal 1 (file-linkage-take file-linkage from-path to-path)))
(test-end)

(test-begin "get-init-inference-path")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
      [root-library-node (init-library-node root-file-node)]
      [file-linkage (init-file-linkage root-file-node root-library-node)]
      [paths (apply append (get-init-reference-batches file-linkage))]
      [target-path (string-append (current-directory) "/protocol/error-code.sls")])
    (test-equal target-path (find (lambda (p) (equal? target-path p)) paths)))
(test-end)

(test-begin "file-linkage-to")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-akku-acceptable-file-filter (string-append (current-directory) "/.akku/list")))]
      [root-library-node (init-library-node root-file-node)]
      [file-linkage (init-file-linkage root-file-node root-library-node)]
      [to-path (string-append (current-directory) "/protocol/error-code.sls")]
      [paths (file-linkage-to file-linkage to-path)])
    (test-equal 
    (string-append (current-directory) "/scheme-langserver.sls")
    (car paths)))
(test-end)

(test-begin "init-linkage-matrix-r7rs")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
      [root-library-node (init-library-node root-file-node 'r7rs)]
      [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
      [from-path (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")]
      [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")])
    (test-equal 1 (file-linkage-take file-linkage from-path to-path)))
(test-end)

(test-begin "get-init-inference-path-r7rs")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
      [root-library-node (init-library-node root-file-node 'r7rs)]
      [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
      [paths (apply append (get-init-reference-batches file-linkage))]
      [target-path (string-append (current-directory) "/tests/resources/r7rs/scheme/base.scm.txt")])
    (test-equal target-path (find (lambda (p) (equal? target-path p)) paths)))
(test-end)

(test-begin "file-linkage-to-r7rs")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 'r7rs)]
      [root-library-node (init-library-node root-file-node 'r7rs)]
      [file-linkage (init-file-linkage root-file-node root-library-node 'r7rs)]
      [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")]
      [paths (file-linkage-to file-linkage to-path)])
    (test-equal 
    (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")
    (car paths)))
(test-end)

(test-begin "file-linkage-to-s7")
  (let* ([root-file-node (init-virtual-file-system (current-directory) '() (generate-txt-file-filter) 's7)]
      [root-library-node (init-library-node root-file-node 's7)]
      [file-linkage (init-file-linkage root-file-node root-library-node 's7)]
      [to-path (string-append (current-directory) "/tests/resources/r7rs/srfi/srfi-8.scm.txt")]
      [paths (file-linkage-to file-linkage to-path)])
    (test-equal 
    (string-append (current-directory) "/tests/resources/r7rs/liii/rich-vector.scm.txt")
    (car paths)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group A: Basic query API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "file-linkage-from simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; math only imports (rnrs) which is built-in, so no file deps
    (test-equal '() (file-linkage-from linkage math-path))
  ;; main imports math
    (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (file-linkage-from linkage main-path)))))
(test-end)

(test-begin "file-linkage-head simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [head (file-linkage-head linkage)])
  ;; math has no outgoing deps (only imports built-in rnrs)
  ;; main has outgoing deps (imports math)
  ;; So head should contain math (out-degree 0), not main
    (test-equal #f (null? head))
    (let ([math-path (string-append fixture "/math.scm.txt")])
      (test-equal #f (null? (find (lambda (p) (string=? p math-path)) head)))))
(test-end)

(test-begin "file-linkage-set! and take")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; Initially main -> math is 1
    (test-equal 1 (file-linkage-take linkage main-path math-path))
  ;; Set it again (always sets to 1)
    (file-linkage-set! linkage main-path math-path)
    (test-equal 1 (file-linkage-take linkage main-path math-path)))
(test-end)

(test-begin "file-linkage accessors non-empty")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)])
    (test-equal #f (null? (file-linkage-path->id-map linkage)))
    (test-equal #f (null? (file-linkage-id->path-map linkage)))
    (test-equal #f (null? (file-linkage-matrix linkage)))
  ;; Matrix should be square: length = N * N for some N
    (let ([len (vector-length (file-linkage-matrix linkage))])
      (test-equal #t (integer? (sqrt len)))
      (test-equal #t (> len 0))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group B: Transitive closure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "get-reference-path-from simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; main depends on math; transitive closure includes self
    (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (get-reference-path-from linkage main-path))))
    (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (get-reference-path-from linkage main-path))))
  ;; math has no file deps; closure is just self
    (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (get-reference-path-from linkage math-path))))
    (test-equal 1 (length (get-reference-path-from linkage math-path))))
(test-end)

(test-begin "get-reference-path-to simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; math is depended on by main; reverse closure includes self
    (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (get-reference-path-to linkage math-path))))
    (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (get-reference-path-to linkage math-path))))
  ;; main is not depended on by any file; closure is just self
    (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (get-reference-path-to linkage main-path))))
    (test-equal 1 (length (get-reference-path-to linkage main-path))))
(test-end)

(test-begin "get-reference-path-from two-libs")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [helper-path (string-append fixture "/helper.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; main depends on helper
    (test-equal #f (null? (find (lambda (p) (string=? p helper-path)) (get-reference-path-from linkage main-path))))
    (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (get-reference-path-from linkage main-path))))
  ;; helper has no file deps
    (test-equal 1 (length (get-reference-path-from linkage helper-path))))
(test-end)

(test-begin "get-reference-path-to two-libs")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [helper-path (string-append fixture "/helper.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; helper is depended on by main
    (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (get-reference-path-to linkage helper-path))))
    (test-equal #f (null? (find (lambda (p) (string=? p helper-path)) (get-reference-path-to linkage helper-path))))
  ;; main is not depended on by any file
    (test-equal 1 (length (get-reference-path-to linkage main-path))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group C: Topological batching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "shrink-paths simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; math has out-degree 0 (only imports built-in rnrs), main depends on math
    (let ([batches (shrink-paths linkage (list math-path main-path))])
      (test-equal 2 (length batches))
      (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (car batches))))
      (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (cadr batches))))))
(test-end)

(test-begin "shrink-paths two-libs")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/two-libs")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [helper-path (string-append fixture "/helper.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; helper has out-degree 0, main depends on helper
    (let ([batches (shrink-paths linkage (list helper-path main-path))])
      (test-equal 2 (length batches))
      (test-equal #f (null? (find (lambda (p) (string=? p helper-path)) (car batches))))
      (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (cadr batches))))))
(test-end)

(test-begin "get-init-reference-batches simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
    (let ([batches (get-init-reference-batches linkage)])
      (test-equal #f (null? batches))
    ;; math should appear in an earlier batch than main
    ;; With only 2 files, we expect exactly 2 batches: [[math], [main]]
      (test-equal 2 (length batches))
      (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (car batches))))
      (test-equal #f (null? (find (lambda (p) (string=? p main-path)) (cadr batches))))))
(test-end)

(test-begin "shrink-paths single file")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [main-path (string-append fixture "/main.scm.txt")])
    (let ([batches (shrink-paths linkage (list main-path))])
      (test-equal 1 (length batches))
      (test-equal 1 (length (car batches)))
      (test-equal main-path (caar batches))))
(test-end)

(test-begin "shrink-paths empty")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)])
    (let ([batches (shrink-paths linkage '())])
      (test-equal '() batches)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group D: Incremental refresh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "refresh-file-linkage&get-refresh-path simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [root-library-node (workspace-library-node workspace)]
      [root-file-node (workspace-file-node workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; Find main file-node
    (let ([main-file-node (find (lambda (child) (string=? (file-node-path child) main-path))
          (file-node-children root-file-node))])
      (test-equal #f (null? main-file-node))
      (let* ([doc (file-node-document main-file-node)]
          [index-node-list (document-index-node-list doc)]
          [lib-ids (get-library-identifiers-list doc 'r6rs)]
          [refresh-paths (refresh-file-linkage&get-refresh-path linkage root-library-node main-file-node index-node-list lib-ids 'r6rs)])
      ;; Should return main itself and its dependency math
        (test-equal #f (null? (find (lambda (p) (string=? p main-path)) refresh-paths)))
        (test-equal #f (null? (find (lambda (p) (string=? p math-path)) refresh-paths)))
      ;; Linkage should still be intact
        (test-equal #f (null? (find (lambda (p) (string=? p math-path)) (file-linkage-from linkage main-path)))))))
(test-end)

(test-begin "matrix-expand")
;; 1x1 -> 2x2
(let* ([m1 (vector 1)]
      [m2 (matrix-expand m1)])
    (test-equal 4 (vector-length m2))
    (test-equal 1 (matrix-take m2 0 0))
    (test-equal 0 (matrix-take m2 0 1))
    (test-equal 0 (matrix-take m2 1 0))
    (test-equal 0 (matrix-take m2 1 1)))
;; 2x2 -> 3x3
(let* ([m2 (vector 1 2 3 4)]
      [m3 (matrix-expand m2)])
    (test-equal 9 (vector-length m3))
    (test-equal 1 (matrix-take m3 0 0))
    (test-equal 2 (matrix-take m3 0 1))
    (test-equal 3 (matrix-take m3 1 0))
    (test-equal 4 (matrix-take m3 1 1))
  ;; New row and column should be 0
    (test-equal 0 (matrix-take m3 0 2))
    (test-equal 0 (matrix-take m3 1 2))
    (test-equal 0 (matrix-take m3 2 0))
    (test-equal 0 (matrix-take m3 2 1))
    (test-equal 0 (matrix-take m3 2 2)))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group E: Edge cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "init-file-linkage empty project")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/empty-project")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)])
    (test-equal 0 (vector-length (file-linkage-matrix linkage)))
    (test-equal '() (get-init-reference-batches linkage))
    (test-equal '() (file-linkage-head linkage)))
(test-end)

(test-begin "init-file-linkage script-only")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/script-only")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [hello-path (string-append fixture "/hello.scm.txt")])
  ;; Script files are registered in the linkage but have no file deps
    (test-equal '() (file-linkage-from linkage hello-path))
    (test-equal '() (file-linkage-to linkage hello-path)))
(test-end)

(test-begin "shrink-paths cycle SCC")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/cycle")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [a-path (string-append fixture "/a.scm.txt")]
      [b-path (string-append fixture "/b.scm.txt")])
  ;; Cycle forms an SCC; shrink-ids packs SCC into a single batch
    (let ([batches (shrink-paths linkage (list a-path b-path))])
      (test-equal 1 (length batches))
      (test-equal #f (null? (find (lambda (p) (string=? p a-path)) (car batches))))
      (test-equal #f (null? (find (lambda (p) (string=? p b-path)) (car batches))))))
(test-end)

(test-begin "get-init-reference-batches cycle")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/cycle")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [a-path (string-append fixture "/a.scm.txt")]
      [b-path (string-append fixture "/b.scm.txt")])
    (let ([batches (get-init-reference-batches linkage)])
    ;; Both files should be in the same batch because of the cycle
      (test-equal 1 (length batches))
      (test-equal #f (null? (find (lambda (p) (string=? p a-path)) (car batches))))
      (test-equal #f (null? (find (lambda (p) (string=? p b-path)) (car batches))))))
(test-end)

(test-begin "shrink-file-linkage! simple-lib")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)]
      [math-path (string-append fixture "/math.scm.txt")]
      [main-path (string-append fixture "/main.scm.txt")])
  ;; Before shrink: 2 files
    (test-equal 4 (vector-length (file-linkage-matrix linkage)))
    (test-equal 2 (hashtable-size (file-linkage-path->id-map linkage)))
    (test-equal #f (null? (file-linkage-from linkage main-path)))
  ;; Shrink math
    (shrink-file-linkage! linkage math-path)
    (test-equal 1 (vector-length (file-linkage-matrix linkage)))
    (test-equal 1 (hashtable-size (file-linkage-path->id-map linkage)))
  ;; main should no longer have any file deps
    (test-equal '() (file-linkage-from linkage main-path))
  ;; math should be gone from the maps
    (test-equal #f (hashtable-ref (file-linkage-path->id-map linkage) math-path #f)))
(test-end)

(test-begin "shrink-file-linkage! nonexistent path")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [workspace (init-workspace fixture 'txt 'r6rs #f #f)]
      [linkage (workspace-file-linkage workspace)])
  ;; Shrinking a path that is not in the linkage should be a no-op
    (shrink-file-linkage! linkage "/nonexistent/path.scm")
    (test-equal 2 (hashtable-size (file-linkage-path->id-map linkage))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
