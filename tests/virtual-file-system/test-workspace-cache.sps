#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(import 
    (chezscheme)
    (srfi :64 testing)
    (ufo-persistence)
    (scheme-langserver analysis workspace-cache)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)
    (scheme-langserver analysis workspace))

(define (private:make-workspace-payload ws)
  `((file-node . ,(workspace-file-node ws))
    (library-node . ,(workspace-library-node ws))
    (file-linkage . ,(workspace-file-linkage ws))
    (threaded? . ,(workspace-threaded? ws))
    (type-inference? . ,(workspace-type-inference? ws))
    (top-environment . ,(workspace-top-environment ws))
    (undiagnosed-paths . ,(workspace-undiagnosed-paths ws))))

(define (private:cache-file cache-dir)
  (string-append cache-dir "/workspace.bin"))

(define (private:write-file path text)
  (when (file-exists? path)
    (delete-file path))
  (call-with-output-file path
    (lambda (port) (display text port))))

(test-begin "workspace-cache-step1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 1: Registry initialization does not crash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "registry-init")
(init-workspace-cache-registry!)
(test-assert "registry initialized without error" #t)
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 2: file-node roundtrip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "file-node-roundtrip")
(let* ([root (make-file-node "/project" "project" '() #t '() '())]
       [child (make-file-node "/project/lib.sls" "lib.sls" root #f '() '())])
  (file-node-children-set! root `(,child))
  (let ([bv (object->bytevector root)])
    (let ([root* (bytevector->object bv)])
      (test-assert "deserialized is file-node?" (file-node? root*))
      (test-equal "/project" (file-node-path root*))
      (test-equal #t (file-node-folder? root*))
      (let ([children* (file-node-children root*)])
        (test-equal 1 (length children*))
        (test-equal "/project/lib.sls" (file-node-path (car children*)))
        ;; Parent reference should be preserved via cyclic reference handling
        (test-equal root* (file-node-parent (car children*)))))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 3: document roundtrip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "document-roundtrip")
(let ([doc (make-document "file:///test.sls" "(define x 1)" '())])
  (document-index-node-list-set! doc '(n1 n2))
  (document-refreshable?-set! doc #f)
  (let ([bv (object->bytevector doc)])
    (let ([doc* (bytevector->object bv)])
      (test-assert "deserialized is document?" (document? doc*))
      (test-equal "file:///test.sls" (document-uri doc*))
      (test-equal "(define x 1)" (document-text doc*))
      (test-equal '(n1 n2) (document-index-node-list doc*))
      (test-equal #f (document-refreshable? doc*))
      (test-assert "line-length-vector is vector" (vector? (document-line-length-vector doc*))))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 4: file-linkage roundtrip (with path->id-map cleared)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "file-linkage-roundtrip")
(let ([linkage (make-file-linkage (make-eq-hashtable) (make-eq-hashtable) '#(0 0 0 0))])
  (let ([bv (object->bytevector linkage)])
    (let ([linkage* (bytevector->object bv)])
      (test-assert "deserialized is file-linkage?" (file-linkage? linkage*))
      (test-equal 4 (vector-length (file-linkage-matrix linkage*))))))
(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 5: workspace roundtrip (minimal via alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "workspace-roundtrip")
(let* ([root-file (make-file-node "/project" "project" '() #t '() '())]
       [root-lib (make-library-node '(project) '() '() '())]
       [linkage (make-file-linkage (make-eq-hashtable) (make-eq-hashtable) '#(0))]
       [ws (make-workspace root-file root-lib linkage 'akku #f #f 'r6rs)]
       [cache-dir "/tmp/test-workspace-cache"])
  (when (file-directory? cache-dir)
    (when (file-exists? (private:cache-file cache-dir))
      (delete-file (private:cache-file cache-dir)))
    (delete-directory cache-dir))
  (save-workspace-cache! (private:make-workspace-payload ws) cache-dir 'akku 'r6rs)
  (let ([payload (load-workspace-cache cache-dir 'akku 'r6rs)])
    (test-equal "/project" (file-node-path (cdr (assq 'file-node payload))))
    (test-equal 'r6rs (cdr (assq 'top-environment payload))))
  ;; Cleanup
  (when (file-directory? cache-dir)
    (when (file-exists? (private:cache-file cache-dir))
      (delete-file (private:cache-file cache-dir)))
    (delete-directory cache-dir)))
(test-end)

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: Full workspace roundtrip with fixture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-begin "workspace-cache-step2")

(test-begin "full-workspace-roundtrip")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
       [cache-dir (string-append (current-directory) "/tests/resources/workspace-fixtures/.test-cache")]
       [math-path (string-append fixture "/math.scm.txt")])
  
  ;; Clean up any stale cache
  (when (file-directory? cache-dir)
    (when (file-exists? (private:cache-file cache-dir))
      (delete-file (private:cache-file cache-dir)))
    (delete-directory cache-dir))
  
  ;; First init: no cache, creates fresh workspace
  (let ([workspace (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
    (test-equal #f (null? (walk-file (workspace-file-node workspace) math-path)))
    (test-equal #f (null? (file-linkage-path->id-map (workspace-file-linkage workspace))))
    ;; Save cache explicitly, mimicking server shutdown behavior
    (save-workspace-cache-for! workspace cache-dir 'txt 'r6rs))
  
  ;; Second init: cache exists, should load from cache
  (let ([workspace* (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
    (let ([root-file-node* (workspace-file-node workspace*)]
          [root-library-node* (workspace-library-node workspace*)]
          [linkage* (workspace-file-linkage workspace*)])
      
      ;; 1. file-node tree integrity
      (test-equal #f (null? (walk-file root-file-node* math-path)))
      (let ([math-node* (walk-file root-file-node* math-path)])
        (test-equal math-path (file-node-path math-node*))
        (test-equal root-file-node* (file-node-parent math-node*)))
      
      ;; 2. library-node tree integrity
      (test-equal #f (null? (walk-library '(fixtures simple-lib math) root-library-node*)))
      
      ;; 3. file-linkage integrity: path->id-map rebuilt
      (test-equal #f (null? (file-linkage-path->id-map linkage*)))
      (test-equal #f (null? (file-linkage-id->path-map linkage*)))
      ;; Verify the mapping is correct
      (let ([math-id (hashtable-ref (file-linkage-path->id-map linkage*) math-path #f)])
        (test-equal math-path (hashtable-ref (file-linkage-id->path-map linkage*) math-id #f)))
      
      ;; 4. document text preserved
      (let ([math-doc* (file-node-document (walk-file root-file-node* math-path))])
        (test-equal #f (null? math-doc*))
        (test-assert "document text is string" (string? (document-text math-doc*)))
        (test-equal #f (document-refreshable? math-doc*)))
      
      ;; 5. diagnoses cleared
      (let ([math-doc* (file-node-document (walk-file root-file-node* math-path))])
        (test-equal '() (document-diagnoses math-doc*)))
      
      ;; 6. undiagnosed-paths cleared
      (test-equal '() (workspace-undiagnosed-paths workspace*))
      
      ;; 7. top-environment preserved
      (test-equal 'r6rs (workspace-top-environment workspace*))))
  
  ;; Cleanup
  (when (file-directory? cache-dir)
    (when (file-exists? (private:cache-file cache-dir))
      (delete-file (private:cache-file cache-dir)))
    (delete-directory cache-dir)))
(test-end)

(test-begin "manifest-mismatch-invalidates-cache")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
       [cache-dir (string-append (current-directory) "/tests/resources/workspace-fixtures/.test-cache")]
       [cache-file (private:cache-file cache-dir)])
  
  ;; Clean up
  (when (file-directory? cache-dir)
    (when (file-exists? cache-file)
      (delete-file cache-file))
    (delete-directory cache-dir))
  
  ;; Create cache with txt/r6rs
  (let ([workspace (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
    (save-workspace-cache-for! workspace cache-dir 'txt 'r6rs))
  (test-assert "cache created" (workspace-cache-available? cache-dir))
  
  ;; Corrupt the wrapper's manifest by rewriting it with a different top-environment
  (let* ([wrapper (restore-object cache-file)]
         [payload (caddr wrapper)]
         [bad-manifest '(cache-manifest
                           (format-version 1)
                           (ufo-persistence-version 2)
                           (langserver-version "x")
                           (facet txt)
                           (top-environment r7rs)
                           (created-at "x"))])
    (persist-object cache-file `(cache-wrapper ,bad-manifest ,payload)))
  
  ;; Next init should ignore the corrupted cache and create fresh workspace
  (let ([workspace (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
    (test-assert "fresh workspace created after manifest mismatch"
      (not (null? (workspace-file-node workspace)))))
  
  ;; Cleanup
  (when (file-directory? cache-dir)
    (when (file-exists? cache-file)
      (delete-file cache-file))
    (delete-directory cache-dir)))
(test-end)

(test-begin "cache-consistency-refresh")
(let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-lib")]
       [cache-dir (string-append (current-directory) "/tests/resources/workspace-fixtures/.test-cache")]
       [cache-file (private:cache-file cache-dir)]
       [math-path (string-append fixture "/math.scm.txt")]
       [original-text (call-with-input-file math-path get-string-all)]
       [modified-text "(library (fixtures simple-lib math)\n  (export add)\n  (import (rnrs))\n  (define (add a b) (+ a b 1)))\n"])
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      ;; Clean up stale cache
      (when (file-directory? cache-dir)
        (when (file-exists? cache-file)
          (delete-file cache-file))
        (delete-directory cache-dir))
      ;; Create and save cache
      (let ([workspace (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
        (save-workspace-cache-for! workspace cache-dir 'txt 'r6rs))
      (test-assert "cache created" (workspace-cache-available? cache-dir))
      ;; Modify disk file
      (private:write-file math-path modified-text)
      ;; Load from cache: should detect mismatch and refresh
      (let ([workspace* (init-workspace fixture 'txt 'r6rs #f #f cache-dir)])
        (let ([math-doc* (file-node-document (walk-file (workspace-file-node workspace*) math-path))])
          (test-equal modified-text (document-text math-doc*)))))
    (lambda ()
      ;; Restore original file content
      (private:write-file math-path original-text)
      ;; Clean up cache
      (when (file-directory? cache-dir)
        (when (file-exists? cache-file)
          (delete-file cache-file))
        (delete-directory cache-dir)))))
(test-end)

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
