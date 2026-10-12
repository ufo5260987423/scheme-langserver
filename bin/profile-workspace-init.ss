#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Phase-level performance profile of cold workspace initialization.
;; Run: source .akku/bin/activate && scheme --script bin/profile-workspace-init.ss [path] [type-inference?]
#!r6rs

(import
  (chezscheme)
  (scheme-langserver analysis workspace)
  (scheme-langserver analysis abstract-interpreter)
  (scheme-langserver analysis dependency file-linkage)
  (scheme-langserver analysis identifier rules library-import)
  (scheme-langserver analysis dependency file-linkage)
  (scheme-langserver analysis identifier rules library-import)
  (scheme-langserver analysis package-manager file-filter)
  (scheme-langserver virtual-file-system file-node)
  (scheme-langserver virtual-file-system library-node))

(define (current-real-ms)
  (let ([t (current-time)])
    (+ (* 1000 (time-second t))
       (div (time-nanosecond t) 1000000))))

(define (maybe-time->ms x)
  (if (record? x)
      (+ (* 1000 (time-second x)) (div (time-nanosecond x) 1000000))
      x))

(define (gc-cpu-ms)
  (maybe-time->ms (sstats-gc-cpu (statistics))))

;; Returns (real cpu gc result)
(define (timed thunk)
  (collect)
  (let ([real0 (current-real-ms)]
        [cpu0 (cpu-time)]
        [gc0 (gc-cpu-ms)])
    (let ([result (thunk)])
      (values (- (current-real-ms) real0)
              (- (cpu-time) cpu0)
              (- (gc-cpu-ms) gc0)
              result))))

(define (count-file-nodes node)
  (+ 1 (apply + (map count-file-nodes (file-node-children node)))))

(define (report label real cpu gc extra)
  (printf "  ~a: ~5d ms real ~5d ms cpu ~5d ms gc~a~n" label real cpu gc
          (or extra "")))

(define (main)
  (let* ([args (command-line-arguments)]
         [path (if (null? args) (current-directory) (car args))]
         [type-inference? (and (pair? (cdr args)) (equal? (cadr args) "#t"))]
         [file-filter (if (and (pair? (cdr args)) (pair? (cddr args))) (string->symbol (caddr args)) 'akku)]
         [top-environment 'r6rs])
    (printf "profiling cold init of ~a (type-inference? ~a)~n" path type-inference?)

    (let-values ([(_real _cpu _gc facet) (timed (lambda () (file-filter->predicate file-filter path)))])
      (let-values ([(real cpu gc root-file-node) (timed (lambda () (init-virtual-file-system path '() facet top-environment)))])
        (report "1. init-virtual-file-system" real cpu gc
                (format "  (~d files)" (count-file-nodes root-file-node)))
        (let ([acc real])
          (let-values ([(real cpu gc root-library-node) (timed (lambda () (init-library-node root-file-node top-environment)))])
            (report "2. init-library-node" real cpu gc #f)
            (set! acc (+ acc real))
            (let-values ([(real cpu gc file-linkage) (timed (lambda () (init-file-linkage root-file-node root-library-node top-environment)))])
              (report "3. init-file-linkage" real cpu gc #f)
              (set! acc (+ acc real))
              (let-values ([(real cpu gc batches) (timed (lambda () (get-init-reference-batches file-linkage)))])
                (report "4. get-init-reference-batches" real cpu gc
                        (format "  (~d batches, ~d paths)" (length batches) (apply + (map length batches))))
                (set! acc (+ acc real))
                ;; make-workspace is not exported, so run the full init-workspace
                ;; and derive the init-references cost by subtraction.
                (let-values ([(real cpu gc _) (timed
                                (lambda () (init-workspace path file-filter top-environment #f type-inference?)))])
                  (report "5. init-references (derived)" (- real acc) cpu gc #f)
                  (report "   total init-workspace      " real cpu gc #f))))))
    (printf "== excluded-refs counters (ns) ==~n")
    (debug:excl-profile-report)
    (printf "done.~n")))))

(main)
