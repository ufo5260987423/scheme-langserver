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
    (ufo-persistence)

    (scheme-langserver analysis identifier reference)
    (scheme-langserver analysis dependency file-linkage)

    (scheme-langserver virtual-file-system index-node)
    (scheme-langserver virtual-file-system document)
    (scheme-langserver virtual-file-system file-node)
    (scheme-langserver virtual-file-system library-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record type registry for ufo-persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:register-record-type! make-instance)
  (let ([instance (make-instance)])
    (let ([rtd (record-rtd instance)])
      (register-record-type! 
        (record-type-name rtd)
        rtd
        (record-constructor rtd)))))

(define (private:register-built-in-record-type! name make-instance)
  (let ([instance (make-instance)])
    (let ([rtd (record-rtd instance)])
      (register-record-type! name rtd (record-constructor rtd)))))

(define (init-workspace-cache-registry!)
  ;; Built-in Chez record types that appear in the workspace graph.
  ;; We create dummy instances just to obtain their RTDs/constructors.
  (let ([port (open-bytevector-input-port (string->bytevector "" (native-transcoder)))])
    (let ([sfd (make-source-file-descriptor "dummy.ss" port)])
      (private:register-built-in-record-type! 'source-file-descriptor 
        (lambda () sfd))
      (let ([port2 (transcoded-port 
                     (open-bytevector-input-port (string->bytevector "x" (native-transcoder)))
                     (native-transcoder))])
        (call-with-values
          (lambda () (get-datum/annotations port2 sfd 0))
          (lambda (ann n)
            (private:register-built-in-record-type! 'annotation 
              (lambda () ann))
            (private:register-built-in-record-type! 'source 
              (lambda () (annotation-source ann))))))))

  ;; scheme-langserver workspace record types
  (private:register-record-type! 
    (lambda () (make-file-node "" "" '() #f '() '())))
  (private:register-record-type! 
    (lambda () (make-library-node '() '() '() '())))
  (private:register-record-type! 
    (lambda () (make-document "" "" '())))
  (private:register-record-type! 
    (lambda () (make-index-node '() 0 0 '() '() '() '() '())))
  (private:register-record-type! 
    (lambda () (make-identifier-reference 'foo '() '() '() '() '() '() '())))
  (private:register-record-type! 
    (lambda () (make-file-linkage (make-eq-hashtable) (make-eq-hashtable) '#(0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache path helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:cache-file-path cache-path)
  (string-append cache-path "/workspace.bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version detection
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
    "unknown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manifest
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

(define (private:make-manifest facet top-environment langserver-version)
  `(cache-manifest
     (format-version 1)
     (ufo-persistence-version 2)
     (langserver-version ,langserver-version)
     (facet ,facet)
     (top-environment ,top-environment)
     (created-at ,(private:format-current-date))))

(define (private:manifest-matches? manifest facet top-environment langserver-version)
  (and (pair? manifest)
       (eq? 'cache-manifest (car manifest))
       (equal? '(format-version 1) (assq 'format-version (cdr manifest)))
       (equal? '(ufo-persistence-version 2) (assq 'ufo-persistence-version (cdr manifest)))
       (equal? `(facet ,facet) (assq 'facet (cdr manifest)))
       (equal? `(top-environment ,top-environment) (assq 'top-environment (cdr manifest)))
       (equal? `(langserver-version ,langserver-version) (assq 'langserver-version (cdr manifest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We wrap the payload together with a small manifest inside a single
;; cache file.  The manifest is checked before the payload is used, so
;; stale or incompatible caches are rejected without having to fully
;; deserialize the workspace graph.

(define (private:make-wrapper manifest payload)
  `(cache-wrapper ,manifest ,payload))

(define (private:wrapper-manifest wrapper)
  (cadr wrapper))

(define (private:wrapper-payload wrapper)
  (caddr wrapper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; We serialize an alist payload, NOT the workspace record itself,
;; because the workspace's 'facet' field holds a procedure which
;; ufo-persistence cannot handle. The caller builds the payload.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (workspace-cache-available? cache-path)
  (and cache-path
       (file-directory? cache-path)
       (file-exists? (private:cache-file-path cache-path))))

(define (load-workspace-cache cache-path facet top-environment)
  (unless (workspace-cache-available? cache-path)
    (raise 'workspace-cache-missing))
  (let ([wrapper (restore-object (private:cache-file-path cache-path))])
    (unless (and (pair? wrapper) (eq? 'cache-wrapper (car wrapper)))
      (raise 'workspace-cache-corrupted))
    (let ([manifest (private:wrapper-manifest wrapper)]
          [payload (private:wrapper-payload wrapper)])
      (unless (private:manifest-matches? manifest facet top-environment (private:get-langserver-version))
        (raise 'workspace-cache-manifest-mismatch))
      (let ([file-linkage (cdr (assq 'file-linkage payload))])
        (private:rebuild-file-linkage-path->id-map! file-linkage))
      payload)))

(define (save-workspace-cache! payload cache-path facet top-environment)
  (unless (file-directory? cache-path)
    (mkdir cache-path))
  ;; Remove stale two-file-cache metadata left by older versions.
  (let ([stale-manifest (string-append cache-path "/manifest.sexp")])
    (when (file-exists? stale-manifest)
      (delete-file stale-manifest)))
  (let ([manifest (private:make-manifest facet top-environment (private:get-langserver-version))])
    (persist-object (private:cache-file-path cache-path) (private:make-wrapper manifest payload))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deserialization postprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (private:rebuild-file-linkage-path->id-map! linkage)
  (let ([id->path-map (file-linkage-id->path-map linkage)])
    (when id->path-map
      (let ([new-path->id-map (make-hashtable string-hash equal?)]
            [keys (hashtable-keys id->path-map)])
        (let loop ([i 0])
          (when (< i (vector-length keys))
            (let ([id (vector-ref keys i)])
              (hashtable-set! new-path->id-map (hashtable-ref id->path-map id #f) id))
            (loop (+ i 1))))
        (file-linkage-path->id-map-set! linkage new-path->id-map)))))

) ; library
