;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2026 WANG Zheng
;; SPDX-License-Identifier: MIT
#!r6rs

(library (scheme-langserver analysis workspace-cache)
  (export 
    init-workspace-cache-registry!
    workspace-cache-available?
    load-workspace-cache
    save-workspace-cache!)

  (import 
    (chezscheme)

    ;; These imports ensure the record types that appear in the cached object
    ;; graph are loaded before fasl-read is called.  Chez fasl stores record
    ;; type names and reconstructs records by looking up their RTDs in the
    ;; current environment.
    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registry initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unlike ufo-persistence, Chez fasl does not require explicit RTD
;; registration.  It only requires that the record type definitions have
;; been loaded.  This function is kept as a convenient hook for callers and
;; as a place to document which modules must be imported.
(define (init-workspace-cache-registry!)
  ;; All record types are loaded as a side effect of the import statements
  ;; above.  No extra work is necessary for Chez fasl.
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache path helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:cache-file-path cache-path)
  (string-append cache-path "/workspace.fasl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version and environment detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:get-langserver-version)
  (or
    (guard (e [else #f])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports "git describe --tags --always --dirty 2>/dev/null"
                                        (buffer-mode block)
                                        (native-transcoder))])
        (let ([line (get-line from-stdout)])
          (close-output-port to-stdin)
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (if (or (eof-object? line) (string=? line ""))
            #f
            line))))
    (guard (e [else #f])
      (call-with-input-file ".version"
        (lambda (p)
          (let ([line (get-line p)])
            (if (eof-object? line) #f line)))))
    "unknown"))

(define (private:get-chez-version)
  (scheme-version))

(define (private:get-machine-type)
  (machine-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record fingerprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a string that changes when any of the workspace-related record
;; types changes its field layout.  This is an extra safety net beyond the
;; langserver-version check.
(define (private:record-fingerprint)
  (let ([rtd+instances
          `((file-node . ,(make-file-node "" "" '() #f '() '()))
            (library-node . ,(make-library-node '() '() '() '()))
            (document . ,(make-document "" "" '()))
            (index-node . ,(make-index-node '() 0 0 '() '() '() '() '()))
            (identifier-reference . ,(make-identifier-reference 'foo '() '() '() '() '() '() '()))
            (file-linkage . ,(make-file-linkage (make-eq-hashtable) (make-eq-hashtable) '#(0))))])
    (let ([content
            (apply string-append
              (map
                (lambda (pair)
                  (let ([rtd (record-rtd (cdr pair))])
                    (string-append
                      (symbol->string (car pair))
                      ":"
                      (symbol->string (record-type-name rtd))
                      ":"
                      (apply string-append
                        (map symbol->string
                          (vector->list (record-type-field-names rtd))))
                      ";")))
                rtd+instances))])
      (number->string (string-hash content) 16))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:pad2 n)
  (let ([s (number->string n)])
    (if (= 1 (string-length s))
      (string-append "0" s)
      s)))

(define (private:format-current-date)
  (let ([d (current-date)])
    (string-append
      (number->string (date-year d)) "-"
      (private:pad2 (date-month d)) "-"
      (private:pad2 (date-day d)) "T"
      (private:pad2 (date-hour d)) ":"
      (private:pad2 (date-minute d)) ":"
      (private:pad2 (date-second d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manifest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:make-manifest facet top-environment type-inference? threaded?)
  `(cache-manifest
    (format-version 2)
    (langserver-version ,(private:get-langserver-version))
    (chez-version ,(private:get-chez-version))
    (machine-type ,(private:get-machine-type))
    (facet ,facet)
    (top-environment ,top-environment)
    (type-inference? ,type-inference?)
    (threaded? ,threaded?)
    (created-at ,(private:format-current-date))
    (record-fingerprint ,(private:record-fingerprint))))

(define (private:manifest-matches? manifest facet top-environment type-inference? threaded?)
  (and (pair? manifest)
       (eq? 'cache-manifest (car manifest))
       (equal? '(format-version 2) (assq 'format-version (cdr manifest)))
       (equal? `(facet ,facet) (assq 'facet (cdr manifest)))
       (equal? `(top-environment ,top-environment) (assq 'top-environment (cdr manifest)))
       (equal? `(type-inference? ,type-inference?) (assq 'type-inference? (cdr manifest)))
       (equal? `(threaded? ,threaded?) (assq 'threaded? (cdr manifest)))
       (equal? `(chez-version ,(private:get-chez-version))
               (assq 'chez-version (cdr manifest)))
       (equal? `(machine-type ,(private:get-machine-type))
               (assq 'machine-type (cdr manifest)))
       (equal? `(record-fingerprint ,(private:record-fingerprint))
               (assq 'record-fingerprint (cdr manifest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:make-wrapper manifest payload)
  `(cache-wrapper ,manifest ,payload))

(define (private:wrapper-manifest wrapper)
  (cadr wrapper))

(define (private:wrapper-payload wrapper)
  (caddr wrapper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FASL I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:save-fasl path obj)
  (let ([tmp-path (string-append path ".tmp")])
    ;; Atomic write: write to temp file then rename.
    ;; FASL requires a binary port; pass #f as transcoder.
    (let ([p (open-file-output-port tmp-path
                                    (file-options no-fail)
                                    'block
                                    #f)])
      (fasl-write obj p)
      (close-port p))
    (when (file-exists? path)
      (delete-file path))
    (rename-file tmp-path path)))

(define (private:load-fasl path)
  (let ([p (open-file-input-port path
                                  (file-options)
                                  'block
                                  #f)])
    (let ([obj (fasl-read p)])
      (close-port p)
      obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (workspace-cache-available? cache-path)
  (and cache-path
       (file-directory? cache-path)
       (file-exists? (private:cache-file-path cache-path))))

(define (load-workspace-cache cache-path facet top-environment type-inference? threaded?)
  (unless (workspace-cache-available? cache-path)
    (raise 'workspace-cache-missing))
  (let ([wrapper (private:load-fasl (private:cache-file-path cache-path))])
    (unless (and (pair? wrapper) (eq? 'cache-wrapper (car wrapper)))
      (raise 'workspace-cache-corrupted))
    (let ([manifest (private:wrapper-manifest wrapper)]
          [payload (private:wrapper-payload wrapper)])
      (unless (private:manifest-matches? manifest facet top-environment type-inference? threaded?)
        (raise 'workspace-cache-manifest-mismatch))
      payload)))

(define (save-workspace-cache! payload cache-path facet top-environment type-inference? threaded?)
  (unless (file-directory? cache-path)
    (mkdir cache-path))
  (let ([manifest (private:make-manifest facet top-environment type-inference? threaded?)])
    (private:save-fasl (private:cache-file-path cache-path)
                       (private:make-wrapper manifest payload))))

) ; library
