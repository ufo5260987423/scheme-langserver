#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (chezscheme)
  (ufo-persistence)
  (scheme-langserver analysis workspace-cache)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis dependency file-linkage))

(define (usage)
  (display "Usage: dump-workspace-cache.sps <cache-directory>\n" (current-error-port))
  (exit 1))

(define (count-file-nodes node)
  (if (file-node? node)
    (+ 1 (apply + (map count-file-nodes (file-node-children node))))
    0))

(define (count-documents node)
  (if (file-node? node)
    (let ([doc (file-node-document node)])
      (+ (if (document? doc) 1 0)
         (apply + (map count-documents (file-node-children node)))))
    0))

(define (count-index-nodes index-node)
  (if (index-node? index-node)
    (+ 1 (apply + (map count-index-nodes (index-node-children index-node))))
    0))

(define (count-total-index-nodes node)
  (if (file-node? node)
    (let ([doc (file-node-document node)])
      (+ (if (document? doc)
           (apply + (map count-index-nodes (document-index-node-list doc)))
           0)
         (apply + (map count-total-index-nodes (file-node-children node)))))
    0))

(define (count-identifier-references-in-index-node index-node)
  (if (index-node? index-node)
    (+ (length (index-node-references index-node))
       (length (index-node-excluded-references index-node))
       (apply + (map count-identifier-references-in-index-node (index-node-children index-node))))
    0))

(define (count-total-identifier-references node)
  (if (file-node? node)
    (let ([doc (file-node-document node)])
      (+ (if (document? doc)
           (+ (length (document-ordered-reference-list doc))
              (apply + (map count-identifier-references-in-index-node (document-index-node-list doc))))
           0)
         (apply + (map count-total-identifier-references (file-node-children node)))))
    0))

(define (count-library-nodes node)
  (if (library-node? node)
    (+ 1 (apply + (map count-library-nodes (library-node-children node))))
    0))

(define (collect-file-paths node)
  (if (file-node? node)
    (cons (file-node-path node)
          (apply append (map collect-file-paths (file-node-children node))))
    '()))

(define (matrix-dimension matrix)
  (let ([len (vector-length matrix)])
    (inexact->exact (sqrt len))))

(define (main args)
  (when (null? args)
    (usage))
  (let* ([cache-dir (car args)]
         [cache-file (string-append cache-dir "/workspace.bin")])
    (unless (file-exists? cache-file)
      (display (string-append "Cache file not found: " cache-file "\n") (current-error-port))
      (exit 1))

    (init-workspace-cache-registry!)
    (let ([wrapper (restore-object cache-file)])
      (unless (and (pair? wrapper) (eq? 'cache-wrapper (car wrapper)))
        (display "Invalid cache wrapper.\n" (current-error-port))
        (exit 1))

      (let ([manifest (cadr wrapper)]
            [payload (caddr wrapper)])
        (display "=== Manifest ===\n")
        (pretty-print manifest)

        (let ([file-node (cdr (assq 'file-node payload))]
              [library-node (cdr (assq 'library-node payload))]
              [file-linkage (cdr (assq 'file-linkage payload))]
              [threaded? (cdr (assq 'threaded? payload))]
              [type-inference? (cdr (assq 'type-inference? payload))]
              [top-environment (cdr (assq 'top-environment payload))]
              [undiagnosed-paths (cdr (assq 'undiagnosed-paths payload))])

          (display "\n=== Workspace Settings ===\n")
          (format #t "threaded?: ~a\n" threaded?)
          (format #t "type-inference?: ~a\n" type-inference?)
          (format #t "top-environment: ~a\n" top-environment)
          (format #t "undiagnosed-paths count: ~a\n" (length undiagnosed-paths))

          (display "\n=== File Tree ===\n")
          (format #t "file-node count: ~a\n" (count-file-nodes file-node))
          (format #t "document count: ~a\n" (count-documents file-node))
          (format #t "file paths:\n")
          (for-each (lambda (p) (format #t "  ~a\n" p)) (collect-file-paths file-node))

          (display "\n=== AST / References ===\n")
          (format #t "total index-node count: ~a\n" (count-total-index-nodes file-node))
          (format #t "total identifier-reference count: ~a\n" (count-total-identifier-references file-node))

          (display "\n=== Library Tree ===\n")
          (format #t "library-node count: ~a\n" (count-library-nodes library-node))

          (display "\n=== File Linkage ===\n")
          (let ([matrix (file-linkage-matrix file-linkage)])
            (format #t "matrix dimension: ~ax~a\n" (matrix-dimension matrix) (matrix-dimension matrix))
            (format #t "path->id-map entries: ~a\n" (vector-length (hashtable-keys (file-linkage-path->id-map file-linkage))))
            (format #t "id->path-map entries: ~a\n" (vector-length (hashtable-keys (file-linkage-id->path-map file-linkage))))))))))

(main (command-line-arguments))
