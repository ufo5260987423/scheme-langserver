#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
  (chezscheme)
  (srfi :64 testing)
  (only (srfi :13 strings) string-prefix?)

  (scheme-langserver analysis workspace)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver util contain))

(define (private:copy-directory from to)
  (when (file-directory? to)
    (private:delete-directory to))
  (mkdir to)
  (for-each
    (lambda (entry)
      (let ([from-path (string-append from "/" entry)]
          [to-path (string-append to "/" entry)])
        (if (file-directory? from-path)
          (private:copy-directory from-path to-path)
          (call-with-input-file from-path
            (lambda (in)
              (let ([out (open-file-output-port to-path (file-options replace) 'block (native-transcoder))])
                (let loop ([c (read-char in)])
                  (cond
                    [(eof-object? c) (close-port out)]
                    [else (write-char c out) (loop (read-char in))]))))))))
    (directory-list from)))

(define (private:collect-shared-pairs root)
  (let ([pairs '()])
    (let loop ([node root])
      (when (index-node-shared-reference node)
        (set! pairs (cons (cons (index-node-shared-reference node) node) pairs)))
      (for-each loop (index-node-children node)))
    pairs))

(define (private:delete-directory path)
  (for-each
    (lambda (entry)
      (let ([full (string-append path "/" entry)])
        (if (file-directory? full)
          (private:delete-directory full)
          (delete-file full))))
    (directory-list path))
  (delete-directory path))

(test-begin "workspace-cache")

(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
    [tmp-root (string-append (current-directory) "/tests/resources/workspace-fixtures/.tmp-cache-test")]
    [work-dir (string-append tmp-root "/simple-lib")]
    [cache-dir (string-append tmp-root "/cache")]
    [cache-file (string-append cache-dir "/workspace.fasl")]
    [main-path (string-append work-dir "/main.scm.txt")]
    [math-path (string-append work-dir "/math.scm.txt")])

  (when (file-directory? tmp-root)
    (private:delete-directory tmp-root))
  (mkdir tmp-root)
  (private:copy-directory fixture work-dir)
  (mkdir cache-dir)

  (test-begin "cold-start-and-save")
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "workspace? cold" (workspace? workspace))
      ;; Mimic server shutdown by saving cache explicitly.
      (save-workspace-cache-for! workspace cache-dir 'r6rs #f #f)
      (test-assert "cache-file-exists" (file-exists? cache-file))
      (let ([child-paths (map file-node-path (file-node-children (workspace-file-node workspace)))])
        (test-assert "main-in-children cold" (contain? child-paths main-path))
        (test-assert "math-in-children cold" (contain? child-paths math-path))))
  (test-end)

  (test-begin "cache-load")
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "workspace? load" (workspace? workspace))
      (let ([child-paths (map file-node-path (file-node-children (workspace-file-node workspace)))])
        (test-assert "main-in-children load" (contain? child-paths main-path))
        (test-assert "math-in-children load" (contain? child-paths math-path)))
      ;; After loading from cache, undiagnosed-paths should be seeded with all
      ;; file paths so that diagnostics can be published without re-analysis.
      (let ([undiagnosed (workspace-undiagnosed-paths workspace)])
        (test-assert "undiagnosed-paths includes main" (contain? undiagnosed main-path))
        (test-assert "undiagnosed-paths includes math" (contain? undiagnosed math-path))
        (test-assert "undiagnosed-paths is sorted" (equal? undiagnosed (sort string<? undiagnosed)))))
  (test-end)

  (test-begin "cache-invalidation-on-change")
    (let ([p (open-file-output-port math-path (file-options replace) 'block (native-transcoder))])
      (put-string p "(define modified 1)\n")
      (close-port p))
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "workspace? invalid" (workspace? workspace))
      (let ([doc (document-text (file-node-document (walk-file (workspace-file-node workspace) math-path)))])
        (test-assert "math-text-modified" (string-prefix? "(define modified" doc))))
  (test-end)

  (test-begin "cache-incremental-refresh-preserved-unchanged")
    ;; Reset fixture to cached state, then change only math.scm.txt.
    (private:copy-directory fixture work-dir)
    (let ([p (open-file-output-port math-path (file-options replace) 'block (native-transcoder))])
      (put-string p "(define modified 2)\n")
      (close-port p))
    (let* ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)]
           [main-doc (document-text (file-node-document (walk-file (workspace-file-node workspace) main-path)))]
           [math-doc (document-text (file-node-document (walk-file (workspace-file-node workspace) math-path)))])
      (test-assert "workspace? incremental" (workspace? workspace))
      (test-assert "math-text-incremental" (string-prefix? "(define modified 2" math-doc))
      ;; main.scm.txt should still be present and unchanged
      (test-assert "main-text-preserved" (not (string-prefix? "(define modified" main-doc))))
  (test-end)

  (test-begin "cache-incremental-add-file")
    ;; Reset fixture, then add a new file.
    (private:copy-directory fixture work-dir)
    (let ([new-path (string-append work-dir "/extra.scm.txt")])
      (let ([p (open-file-output-port new-path (file-options replace) 'block (native-transcoder))])
        (put-string p "(define extra 42)\n")
        (close-port p))
      (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
        (test-assert "workspace? add" (workspace? workspace))
        (test-assert "extra-file-present" (file-node? (walk-file (workspace-file-node workspace) new-path)))
        (let ([doc (document-text (file-node-document (walk-file (workspace-file-node workspace) new-path)))])
          (test-assert "extra-text" (string-prefix? "(define extra" doc)))))
  (test-end)

  (test-begin "cache-incremental-delete-file")
    ;; Reset fixture, then delete math.scm.txt.
    (private:copy-directory fixture work-dir)
    (delete-file math-path)
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "workspace? delete" (workspace? workspace))
      (test-assert "math-file-absent" (not (file-node? (walk-file (workspace-file-node workspace) math-path))))
      (let ([child-paths (map file-node-path (file-node-children (workspace-file-node workspace)))])
        (test-assert "main-still-present" (contain? child-paths main-path))
        (test-assert "math-not-in-children" (not (contain? child-paths math-path)))))
  (test-end)

  (when (file-directory? tmp-root)
    (private:delete-directory tmp-root)))

(test-begin "cache-load-reattaches-expansion-generators")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-macro-auto-resolve")]
      [tmp-root (string-append (current-directory) "/tests/resources/workspace-fixtures/.tmp-cache-gen")]
      [work-dir (string-append tmp-root "/macro")]
      [cache-dir (string-append tmp-root "/cache")]
      [macro-path (string-append work-dir "/macro.scm.txt")])
    (define (find-generator workspace)
      (let* ([document (file-node-document (walk-file (workspace-file-node workspace) macro-path))]
          [result #f])
        (for-each
          (lambda (top)
            (let walk ([node top])
              (let ([expr (guard (e [else '()]) (annotation-stripped (index-node-datum/annotations node)))])
                (when (and (list? expr) (not (null? expr)) (eq? 'define-syntax (car expr)) (not result))
                  (let ([generator (index-node-expansion-generator (car (reverse (index-node-children node))))])
                    (when (procedure? generator) (set! result generator))))
                (for-each walk (index-node-children node)))))
          (document-index-node-list document))
        result))
    (when (file-directory? tmp-root)
      (private:delete-directory tmp-root))
    (mkdir tmp-root)
    (private:copy-directory fixture work-dir)
    (mkdir cache-dir)
    ;; Cold start: the abstract interpreter attaches generators via the
    ;; define-syntax post-procedure.
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "generator present cold" (procedure? (find-generator workspace)))
      (save-workspace-cache-for! workspace cache-dir 'r6rs #f #f))
    ;; Generators are procedures and cannot be fasl-serialized, so a cache
    ;; load must re-attach them from the macro definition forms.
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "generator present after cache load" (procedure? (find-generator workspace))))
    (private:delete-directory tmp-root))
(test-end)

(test-begin "cache-round-trip-preserves-cyclic-literal-shared-reference")
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/cyclic-literal")]
      [tmp-root (string-append (current-directory) "/tests/resources/workspace-fixtures/.tmp-cache-cyclic")]
      [work-dir (string-append tmp-root "/cyclic-literal")]
      [cache-dir (string-append tmp-root "/cache")]
      [cache-file (string-append cache-dir "/workspace.fasl")]
      [lib-path (string-append work-dir "/lib.scm.txt")])

    (when (file-directory? tmp-root)
      (private:delete-directory tmp-root))
    (mkdir tmp-root)
    (private:copy-directory fixture work-dir)
    (mkdir cache-dir)

    ;; Cold start and save cache.
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "cyclic workspace cold ok" (workspace? workspace))
      (let* ([doc (file-node-document (walk-file (workspace-file-node workspace) lib-path))]
          [root (car (document-index-node-list doc))]
          [cold-pairs (private:collect-shared-pairs root)])
        (test-equal "cold start finds shared-reference pair" 1 (length cold-pairs))
        (test-assert "cold reference points to definition"
          (eq? (index-node-shared-reference (cdar cold-pairs)) (caar cold-pairs))))
      (save-workspace-cache-for! workspace cache-dir 'r6rs #f #f)
      (test-assert "cyclic cache-file-exists" (file-exists? cache-file)))

    ;; Load from cache and verify shared-reference preserved.
    (let ([workspace (init-workspace work-dir 'txt 'r6rs #f #f cache-dir)])
      (test-assert "cyclic workspace load ok" (workspace? workspace))
      (let* ([doc (file-node-document (walk-file (workspace-file-node workspace) lib-path))]
          [root (car (document-index-node-list doc))]
          [loaded-pairs (private:collect-shared-pairs root)])
        (test-equal "load finds shared-reference pair" 1 (length loaded-pairs))
        (test-assert "loaded reference points to loaded definition"
          (eq? (index-node-shared-reference (cdar loaded-pairs)) (caar loaded-pairs)))))

    (when (file-directory? tmp-root)
      (private:delete-directory tmp-root)))
(test-end)

(test-end)

(test-begin "cache-saved-on-exit-in-single-thread-mode")
  ;; Regression: private:save-workspace-cache-if-any unconditionally took
  ;; workspace-mutex, which is '() in single-thread mode, so the cache was
  ;; never saved when an ST server shut down.
  (let* ([tmp-root "/tmp/scheme-langserver-test-cache-st-exit"]
      [cache-dir (string-append tmp-root "/cache")]
      [cache-file (string-append cache-dir "/workspace.fasl")]
      [fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
      [exit-script
        (string-append
          "(import (rnrs (6)) (scheme-langserver) (scheme-langserver util io))"
          "(define fixture \"" fixture "\")"
          "(define init-json (string-append \"{\\\"id\\\":\\\"1\\\",\\\"method\\\":\\\"initialize\\\",\\\"jsonrpc\\\":\\\"2.0\\\",\\\"params\\\":{\\\"processId\\\":1,\\\"rootUri\\\":\\\"file://\" fixture \"\\\",\\\"capabilities\\\":{}}}\"))"
          "(define shutdown-json \"{\\\"id\\\":\\\"2\\\",\\\"method\\\":\\\"shutdown\\\",\\\"jsonrpc\\\":\\\"2.0\\\"}\")"
          "(define exit-json \"{\\\"method\\\":\\\"exit\\\",\\\"jsonrpc\\\":\\\"2.0\\\"}\")"
          "(define (framed json) (string-append \"Content-Length: \" (number->string (bytevector-length (string->utf8 json))) \"\\r\\n\\r\\n\" json))"
          "(define input (open-bytevector-input-port (string->utf8 (string-append (framed init-json) (framed shutdown-json) (framed exit-json)))))"
          "(define output (open-file-output-port \"/dev/null\" (file-options no-fail) 'none))"
          "(define log (open-file-output-port \"/dev/null\" (file-options no-fail) 'block (make-transcoder (utf-8-codec))))"
          "(init-server input output log #f #f 'r6rs #f \"" cache-dir "\" 'txt)")])
    (when (file-directory? tmp-root)
      (private:delete-directory tmp-root))
    (mkdir tmp-root)
    (call-with-port
      (open-file-output-port "/tmp/cache-st-exit.sps" (file-options replace) 'block (make-transcoder (utf-8-codec)))
      (lambda (p) (display exit-script p) (newline p)))
    (test-equal 0 (system ". .akku/bin/activate && scheme --script /tmp/cache-st-exit.sps >/dev/null 2>&1"))
    (test-assert "single-thread exit saves workspace cache" (file-exists? cache-file))
    (when (file-directory? tmp-root)
      (private:delete-directory tmp-root)))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
