#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Memory investigation script for auto macro expansion
;; Phase 1: Per-function allocation attribution via (statistics)
;; Phase 2: Object-type census via (object-counts)
;; Phase 3: Process-level baseline via external time -v
;;
;; Run with:
;;   source .akku/bin/activate
;;   ulimit -v $((20*1024*1024))
;;   /usr/bin/time -v scheme --script bin/memory-investigation.ss
#!r6rs

(import
  (chezscheme)
  (srfi :64 testing)
  (scheme-langserver util text)
  (scheme-langserver util path)
  (scheme-langserver util test)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system index-node)
  (scheme-langserver virtual-file-system document)
  (scheme-langserver virtual-file-system library-node)
  (scheme-langserver protocol alist-access-object)
  (scheme-langserver analysis identifier reference)
  (scheme-langserver analysis identifier expanders expansion-wrap)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis workspace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fmt-bytes b)
  (cond
    [(>= b (* 1024 1024 1024)) (format "~,2f GiB" (/ b 1024.0 1024.0 1024.0))]
    [(>= b (* 1024 1024)) (format "~,2f MiB" (/ b 1024.0 1024.0))]
    [(>= b 1024) (format "~,2f KiB" (/ b 1024.0))]
    [else (format "~a B" b)]))

(define (fmt-time t)
  (format "~,3f s" (* 1.0 t 1e-9)))

(define-syntax with-memory-sampling
  (syntax-rules ()
    [(_ label body ...)
     (let ([s0 (statistics)]
           [t0 (current-time)])
       (let ([result (begin body ...)])
         (let ([s1 (statistics)]
               [t1 (current-time)])
           (printf "[SAMPLE] ~a | alloc=~a | gc-cpu=~a | gc-real=~a | wall=~a\n"
             label
             (fmt-bytes (- (sstats-bytes s1) (sstats-bytes s0)))
             (fmt-time (time-nanosecond (time-difference (sstats-gc-cpu s1) (sstats-gc-cpu s0))))
             (fmt-time (time-nanosecond (time-difference (sstats-gc-real s1) (sstats-gc-real s0))))
             (format "~,3f s" (+ 0.0 (time-second (time-difference t1 t0))
                                 (/ (time-nanosecond (time-difference t1 t0)) 1e9))))
           result)))]))

(define (print-object-counts-delta label before after)
  (printf "\n[OBJECT-COUNTS] ~a\n" label)
  (printf "  type          before        after         delta\n")
  (for-each
    (lambda (type)
      (let ([b (assq type before)]
            [a (assq type after)])
        (when (and b a)
          (let ([b-count (car (cdadr b))]
                [a-count (car (cdadr a))])
            (printf "  ~13a ~13a ~13a ~13a\n"
              type b-count a-count (- a-count b-count))))))
    '(pair vector string symbol closure hashtable box record)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reproduced core of test-auto-resolve-basic.sps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-simple-let-test)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/simple-macro-auto-resolve")]
      [workspace-instance (with-memory-sampling "init-workspace(simple-let)"
                            (init-workspace fixture 'txt 'r6rs #f #f))]
      [root-file-node (workspace-file-node workspace-instance)]
      [root-library-node (workspace-library-node workspace-instance)]
      [file-linkage (workspace-file-linkage workspace-instance)]
      [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'simple-let (car expr)))))
        root-index-node)]
      [call-reference (car (find-available-references-for document call-node 'simple-let))]
      [syntax-expander (identifier-reference-syntax-expander call-reference)]
      [binding-list-node (cadr (index-node-children call-node))]
      [binding-node (car (index-node-children binding-list-node))]
      [var-node (car (index-node-children binding-node))])
    (let ([rule (with-memory-sampling "expansion-generator->rule(simple-let)"
                  (expansion-generator->rule syntax-expander step file-linkage '() '()))])
      (with-memory-sampling "rule+shallow-copy(simple-let)"
        (rule root-file-node root-library-node document call-node)))
    (index-node-references-export-to-other-node var-node)))

(define (run-let-syntax-test)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/let-syntax-auto-resolve")]
      [workspace-instance (with-memory-sampling "init-workspace(let-syntax)"
                            (init-workspace fixture 'txt 'r6rs #f #f))]
      [root-file-node (workspace-file-node workspace-instance)]
      [root-library-node (workspace-library-node workspace-instance)]
      [file-linkage (workspace-file-linkage workspace-instance)]
      [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [let-syntax-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'let-syntax (car expr)))))
        root-index-node)]
      [call-node (caddr (index-node-children let-syntax-node))]
      [call-reference (car (find-available-references-for document call-node 'simple-let))]
      [syntax-expander (identifier-reference-syntax-expander call-reference)]
      [binding-list-node (cadr (index-node-children call-node))]
      [binding-node (car (index-node-children binding-list-node))]
      [var-node (car (index-node-children binding-node))])
    (let ([rule (with-memory-sampling "expansion-generator->rule(let-syntax)"
                  (expansion-generator->rule syntax-expander step file-linkage '() '()))])
      (with-memory-sampling "rule+shallow-copy(let-syntax)"
        (rule root-file-node root-library-node document call-node)))
    (index-node-references-export-to-other-node var-node)))

(define (run-syntax-case-test)
  (let* ([fixture (string-append (current-directory) "/tests/resources/workspace-fixtures/syntax-case-auto-resolve")]
      [workspace-instance (with-memory-sampling "init-workspace(syntax-case)"
                            (init-workspace fixture 'txt 'r6rs #f #f))]
      [root-file-node (workspace-file-node workspace-instance)]
      [root-library-node (workspace-library-node workspace-instance)]
      [file-linkage (workspace-file-linkage workspace-instance)]
      [target-file-node (walk-file root-file-node (string-append fixture "/consumer.scm.txt"))]
      [document (file-node-document target-file-node)]
      [root-index-node (car (document-index-node-list document))]
      [call-node (find-index-node-recursive
        (lambda (n)
          (let ([expr (annotation-stripped-expression n)])
            (and (list? expr) (not (null? expr)) (eq? 'simple-let (car expr)))))
        root-index-node)]
      [call-reference (car (find-available-references-for document call-node 'simple-let))]
      [syntax-expander (identifier-reference-syntax-expander call-reference)]
      [binding-list-node (cadr (index-node-children call-node))]
      [binding-node (car (index-node-children binding-list-node))]
      [var-node (car (index-node-children binding-node))])
    (let ([rule (with-memory-sampling "expansion-generator->rule(syntax-case)"
                  (expansion-generator->rule syntax-expander step file-linkage '() '()))])
      (with-memory-sampling "rule+shallow-copy(syntax-case)"
        (rule root-file-node root-library-node document call-node)))
    (index-node-references-export-to-other-node var-node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(collect)
(define counts-before (object-counts))

(define exports-simple-let (run-simple-let-test))
(collect)
(define counts-after-simple-let (object-counts))

(define exports-let-syntax (run-let-syntax-test))
(collect)
(define counts-after-let-syntax (object-counts))

(define exports-syntax-case (run-syntax-case-test))
(collect)
(define counts-after-syntax-case (object-counts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Report
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "\n========== MEMORY INVESTIGATION REPORT ==========\n")
(printf "Note: full-project / cascade tests currently hang (abstract-interpreter recursion).\n")
(printf "Using light-weight fixtures as baseline.\n\n")

(print-object-counts-delta "after simple-let" counts-before counts-after-simple-let)
(print-object-counts-delta "after let-syntax" counts-after-simple-let counts-after-let-syntax)
(print-object-counts-delta "after syntax-case" counts-after-let-syntax counts-after-syntax-case)

(printf "\n[EXPORTS] simple-let: ~a\n" (length exports-simple-let))
(printf "[EXPORTS] let-syntax: ~a\n" (length exports-let-syntax))
(printf "[EXPORTS] syntax-case: ~a\n" (length exports-syntax-case))

(printf "\n=================================================\n")
(printf "For process-level RSS, run with:\n")
(printf "  /usr/bin/time -v scheme --script bin/memory-investigation.ss\n")
